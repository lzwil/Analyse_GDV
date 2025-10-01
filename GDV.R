library(dplyr)
library(readxl)
library(writexl)
library(janitor)
library(ggplot2)
library(tidyr)
library(stringr)
library(MASS)       
library(car)        
library(broom)

setwd("C:/Users/leozw/Documents/Analyse_GDV")

data_final <- read_xlsx("data_final.xlsx")

# --- Filtrer enfants et convertir heures ---
data_final <- data_final %>%
  filter(!is.na(id_data_enf_x)) %>%
  mutate(
    H_coucher_sem_heures = H_coucher_sem / 3600,
    H_coucher_sem_heures = ifelse(H_coucher_sem_heures >= 7 & H_coucher_sem_heures <= 16, NA_real_, H_coucher_sem_heures),
    H_coucher_sem = ifelse(
      is.na(H_coucher_sem_heures),
      NA_character_,
      sprintf("%02d:%02d", floor(H_coucher_sem_heures), round((H_coucher_sem_heures %% 1) * 60))
    )
  ) %>%
  mutate(across(where(is.character), as.factor))

glimpse(data_final)

# --- Variables ordinales  ---
vars_ordinales <- c(
  "sdq_relat1_p1", "sdq_relat2_p1", "sdq_relat3_p1", "sdq_relat4_p1", "sdq_relat5_p1",
  "sdq_comport1", "sdq_comport2", "sdq_comport3", "sdq_comport4",
  "sdq_emot1", "sdq_emot2", "sdq_emot3", "sdq_emot4", "sdq_emot5",
  "sdq_hyper1", "sdq_hyper2", "sdq_hyper3", "sdq_hyper4"
)

# --- Variables numériques / continues ---
vars_numeriques <- c(
  "age_enf", "nb_consult_med_urg", "nb_AcVC_total", "nb_AcVC_3mois"
)

# --- Variables qualitatives nominales / facteurs ---
vars_facteurs <- c(
  "sexe_enf", "scolarise", "H_coucher_sem", "sejour_hospit_yn",
  "consult_med_urg_yn", "motif_consult_med_urg", "AcVC_yn", 
  "lieu_last_AcVC", "type_last_AcVC", "type_last_AcVC_autre",
  "type_hab2", "type_hab", "type_hab2_autre", "acces_eau_yn", 
  "comp_elec_yn", "elec_conform_yn", "cuisin", "mode_chauf", 
  "mode_chauf_autre", "act_ferraillage", "cloture_yn",
  "contig_route_yn", "contig_voie_fer_yn", "contig_eau_yn",
  "proxi_routes", "proxi_dechett", "soins_last_AcVC",
  "type_ldv", "statu_occup", "statu_occup_autre",
  "nb_habitats_empl"
)

# --- Combinaison de tous les vecteurs pour le select ---
all_vars <- c(vars_ordinales, vars_numeriques, vars_facteurs)


# --- Sélection finale des colonnes dans le dataframe ---
df <- data_final %>%
  select(all_of(all_vars)) %>%
  # --- Regroupement variables ordinales ---
  mutate(across(
    all_of(vars_ordinales),
    ~ case_when(.x %in% c(1,2) ~ 1, .x == 0 ~ 0, TRUE ~ NA_real_)
  )) %>%
  mutate(across(all_of(vars_ordinales), ~ factor(.x, levels = c(0,1), ordered = TRUE))) %>%
  
  # --- Recodage variables qualitatives ---
  mutate(
    # remplacer Autres par valeurs dans bonne categorie
    type_hab2 = case_when(
      type_hab2 != "Autre" ~ as.character(type_hab2),
      grepl("caravane|mobil home", type_hab2_autre, ignore.case = TRUE) ~ "Habitat mobile (caravane)",
      grepl("maison|appartement|logement|immeuble|chalet|construction", type_hab2_autre, ignore.case = TRUE) ~ "Construction ou assimilé",
      TRUE ~ "Habitat mixte (caravane et constructions ou assimilés)"
    ),
    #Recoder les modes de chauffage avec les infos dans mode_chauf_autre
    mode_chauf = case_when( 
      !is.na(mode_chauf) & mode_chauf != "Autre" ~ mode_chauf,
      grepl("bois|granul", mode_chauf_autre, ignore.case = TRUE) ~ "Bois",
      grepl("fioul|mazout|pétrole|petrole", mode_chauf_autre, ignore.case = TRUE) ~ "Mazout",
      grepl("gaz", mode_chauf_autre, ignore.case = TRUE) ~ "Gaz",
      grepl("électriq|electric|chauffage au sol|pompe|pac", mode_chauf_autre, ignore.case = TRUE) ~ "Electrique",
      TRUE ~ NA_character_
    ),
    
    # Recoder ferraillage: Est sur le lieu et participe comptés comme "En contact"
    act_ferraillage = ifelse(act_ferraillage %in% c("Est sur le lieu", "Participe"), "En contact", "Pas de contact"),
    across(c(contig_route_yn, contig_voie_fer_yn, contig_eau_yn, proxi_dechett, cloture_yn),
           ~ factor(na_if(as.character(.), "NSP"))),
    
    # Recodage du type d'accident de Autre vers TC ou hématome
    type_last_AcVC = case_when(
      !is.na(type_last_AcVC) & !grepl("^Autre", type_last_AcVC) ~ type_last_AcVC,
      !is.na(type_last_AcVC_autre) & grepl("hématome|Perte connaissance", type_last_AcVC_autre, ignore.case = TRUE) ~ "TC ou hématome",
      type_last_AcVC %in% c("Entorse, luxation", "Fracture") ~ "Entorse_luxation_fracture",
      TRUE ~ NA_character_
    ),
    # # Regroupement des lieux d'accidents dans Autre 
    lieu_last_AcVC = ifelse(lieu_last_AcVC %in% c("Autre (précisez)", "Sur un lieu de loisirs", "Au cours d’une activité sportive"), "Autre", lieu_last_AcVC),
    
    # # Regroupement des terrains familiaux et locatifs
    type_ldv = ifelse(type_ldv %in% c("Terrain familial", "Terrain locatif"), "Terrain familial/locatif", type_ldv),
    
    # Regroupement des statuts d'occupation en 5 catégories principales
    statu_occup = case_when(
      str_detect(statu_occup, "Propriétaire") ~ "Propriétaire",
      str_detect(statu_occup, "Locataire") ~ "Locataire",
      str_detect(statu_occup, "Autre") & !is.na(statu_occup_autre) & 
        str_detect(statu_occup_autre, "Hébergé|Elle es hébergée|Chez des parents|Mme loge|De passage|Pret gracieux") ~ "Hebergés",
      str_detect(statu_occup_autre, "Aire d'accueil|Resident aire d'accueil|Séjournant sur aire d'accueil|Stationnement") ~ "Resident d'aire acceuil",
      str_detect(statu_occup, "Occupation illicite|illicite|Totalement illicite") |
        str_detect(statu_occup_autre, "illicite|Occupation") ~ "Occupation illégale",
      TRUE ~ NA_character_
    ),
    statu_occup = factor(statu_occup, 
                         levels = c("Propriétaire", "Locataire", "Hebergés", "Occupation illégale", "Resident d'aire acceuil"))
  )

# --- Tableau de fréquences ---
tableau_freq <- df %>%
  select(all_of(vars_ordinales)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "valeur") %>%
  group_by(variable, valeur) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(variable) %>%
  mutate(pct = round(100 * n / sum(n, na.rm = TRUE), 1)) %>%
  arrange(variable, as.numeric(valeur))


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


#############################################################################

# --- 1. Vérifier la distribution de nb_AcVC_total ---
summary(df$nb_AcVC_total)
ggplot(df, aes(x = nb_AcVC_total)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution de nb_AcVC_total", x = "Nombre d'accidents", y = "Effectif")

# --- 2. Vérifier surdispersion ---
mean_nb <- mean(df$nb_AcVC_total, na.rm = TRUE)
var_nb <- var(df$nb_AcVC_total, na.rm = TRUE)
overdispersion_ratio <- var_nb / mean_nb
overdispersion_ratio  # > 1 → surdispersion → Poisson pas adapté

# --- 3. Modèle de Poisson ---
mod_pois <- glm(nb_AcVC_total ~ age_enf + sexe_enf + statu_occup + scolarise,
                data = df, family = poisson(link = "log"))

# Vérifier surdispersion
dispersion <- sum(residuals(mod_pois, type="pearson")^2) / mod_pois$df.residual
dispersion  # >1 → surdispersion → utiliser binomiale négative

# --- 4. Modèle binomial négative si surdispersion ---
mod_nb <- glm.nb(nb_AcVC_total ~ age_enf + sexe_enf + statu_occup + scolarise, data = df)

# --- 5. Résumé et diagnostics ---
summary(mod_nb)
Anova(mod_nb, type = "III")  # Tests de type III
tidy(mod_nb, conf.int = TRUE, exponentiate = TRUE)  # RRs (exp(beta))

# --- 6. Visualisation des effets ---
library(effects)
plot(allEffects(mod_nb), multiline = TRUE, main="Effets des variables sur nb_AcVC_total")





