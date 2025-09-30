library(dplyr)
library(readxl)
library(writexl)
library(janitor)
library(ggplot2)
library(tidyr)

setwd("C:\\Users\\A159692\\Documents\\these_Axelle")

data_final <- read_xlsx("data_final.xlsx")

# Récupérer juste les lignes enfants et transformer heure en format lisible 
df <- data_final %>%
  filter(!is.na(id_data_enf_x)) %>% # Récupérer enfants
  mutate(
    # Conversion en heures décimales et suppression des valeurs entre 7 et 16h 
    H_coucher_sem_heures = {
      h <- H_coucher_sem / 3600
      ifelse(h >= 7 & h <= 16, NA_real_, h)
    },
    
    # Format HH:MM, NA si heures aberrantes
    H_coucher_sem = ifelse(
      is.na(H_coucher_sem_heures),
      NA_character_,
      sprintf("%02d:%02d", floor(H_coucher_sem_heures), round((H_coucher_sem_heures %% 1) * 60))
    )
  ) %>%
  mutate(across(where(is.character), as.factor)) # caractères en facteur

# résumer tableau 
glimpse(df)


######################## Remaniement Tableau ################################


# Liste des variables ordinales à regrouper
vars_ordinales <- c(
  "sdq_relat3_p1", "sdq_relat4_p1", "sdq_relat5_p1",
  "sdq_relat1_p1", "sdq_relat2_p1",
  "sdq_comport1", "sdq_comport2", "sdq_comport3", "sdq_comport4",
  "sdq_emot1", "sdq_emot2", "sdq_emot3", "sdq_emot4", "sdq_emot5",
  "sdq_hyper1", "sdq_hyper2", "sdq_hyper3", "sdq_hyper4"
)

df <- df %>%
  select(vars_ordinales, c("age_enf","sexe_enf", "scolarise", "H_coucher_sem", 
                           "sejour_hospit_yn", "consult_med_urg_yn", "nb_consult_med_urg", 
                           "motif_consult_med_urg", "AcVC_yn", "nb_AcVC_total", "nb_AcVC_3mois",
                           "lieu_last_AcVC", "type_last_AcVC", "type_hab2", "type_hab", "type_hab2_autre")) 

# Remplacer les colonnes dans df par le regroupement 0 / 1-2
df <- df %>%
  mutate(across(
    all_of(vars_ordinales),
    ~ case_when(
      .x %in% c(1,2) ~ 1,   # regroupe 1 et 2
      .x == 0 ~ 0,
      TRUE ~ NA_real_        # garder les NA
    )
  ))%>% # Convertir en facteur ordonné pour analyses ou ggplot
  mutate(across(
    all_of(vars_ordinales),
    ~ factor(.x, levels = c(0,1), ordered = TRUE)
  )) %>% # remplacer Autres par valeurs dans bonne categorie
  mutate(type_hab2 = case_when(
    type_hab2 != "Autre" ~ as.character(type_hab2),  # garder les autres valeurs
    grepl("caravane|mobil home", type_hab2_autre, ignore.case = TRUE) ~ "Habitat mobile (caravane)",
    grepl("maison|appartement|logement|immeuble|chalet|construction", type_hab2_autre, ignore.case = TRUE) ~ "Construction ou assimilé",
    TRUE ~ "Habitat mixte (caravane et constructions ou assimilés)"  # par défaut si aucun des critères précédents
  ))

# Maintenant le tableau de fréquences utilisera directement ce regroupement
tableau_freq <- df %>%
  select(all_of(vars_ordinales)) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "valeur"
  ) %>%
  group_by(variable, valeur) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(variable) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>%
  arrange(variable, valeur)

print(tableau_freq)

glimpse(df)


write_xlsx(df, "tableau_final_corrige.xlsx")


########################### Analyse stat ####################################

# Partir de ce tableau pour garder l'original
data <- df

# Colonnes à analyser
vars <- c("H_coucher_sem_heures", "age_enf", "poidskg_enf", "taille_enf", 
          "sdq_hyper1", "sdq_hyper2", "sdq_hyper3", "sdq_hyper4", "sdq_hyper5",
          "sdq_emot1", "sdq_emot2", "sdq_emot3", "sdq_emot4", "sdq_emot5",
          "sdq_comport1", "sdq_comport2", "sdq_comport3", "sdq_comport4")

# Définir les variables ordinales
ordinal_vars <- c("sdq_hyper1", "sdq_hyper2", "sdq_hyper3", "sdq_hyper4", "sdq_hyper5",
                  "sdq_emot1", "sdq_emot2", "sdq_emot3", "sdq_emot4", "sdq_emot5",
                  "sdq_comport1", "sdq_comport2", "sdq_comport3", "sdq_comport4")

# S'assurer qu'AcVC_yn est un facteur
data$AcVC_yn <- factor(data$AcVC_yn, levels = c("Non", "Oui"))

# Fonction pour calculer médiane [IQR] et p-value
analyze_var <- function(var){
  x <- data[[var]]
  grp <- data$AcVC_yn
  
  med_Non <- median(x[grp == "Non"], na.rm = TRUE)
  med_Oui <- median(x[grp == "Oui"], na.rm = TRUE)
  IQR_Non <- IQR(x[grp == "Non"], na.rm = TRUE)
  IQR_Oui <- IQR(x[grp == "Oui"], na.rm = TRUE)
  
  # Format "med [IQR]" avec 1 décimale
  value_Non <- sprintf("%.1f [%.1f]", med_Non, IQR_Non)
  value_Oui <- sprintf("%.1f [%.1f]", med_Oui, IQR_Oui)
  
  # Choisir le test : Wilcoxon pour ordinales, t-test sinon
  if(var %in% ordinal_vars){
    p <- wilcox.test(x ~ grp)$p.value
  } else {
    p <- t.test(x ~ grp)$p.value
  }
  
  data.frame(variable = var,
             Non = value_Non,
             Oui = value_Oui,
             p_value = p)
}

# Appliquer à toutes les variables
results_df <- do.call(rbind, lapply(vars, analyze_var))

# Afficher le résultat
results_df
