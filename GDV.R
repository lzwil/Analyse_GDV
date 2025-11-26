library(dplyr)
library(readxl)
library(writexl)
library(janitor)
library(ggplot2)
library(tidyr)
library(stringr)
library(MASS)       
library(broom)
library(ggplot2)
library(MASS)
library(purrr)
library(survey)
select <- dplyr::select

setwd("C:/Users/leozw/Documents/Analyse_GDV")
data_final <- read_xlsx("data_final.xlsx")

# Charger la base RDS si ce n'est pas déjà fait
GDVf_enf <- readRDS("C:/Users/leozw/Documents/Analyse_GDV/Transmission_GDV/BDD_dico variables/20221213_GDVf_enf.rds")

# Sélectionner les colonnes de pondération dans GDVf_enf
poids_vars <- GDVf_enf %>% select(id_data_enf, strate, poids_enf, fpc)

# Fusionner avec data_final en gardant toutes les colonnes actuelles
data_final <- data_final %>%
  left_join(poids_vars, by = c("id_data_enf_x" = "id_data_enf"))


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


# --- Variables ordinales  ---
vars_ordinales <- c(
  "sdq_relat1_p1", "sdq_relat2_p1", "sdq_relat3_p1", "sdq_relat4_p1", "sdq_relat5_p1",
  "sdq_comport1", "sdq_comport3", "sdq_comport4",
  "sdq_emot2", "sdq_emot4", "sdq_emot5", "sdq_hyper1", "sdq_hyper2", 
  "sdq_hyper3", "sdq_hyper4", "sdq_hyper5"
)

# --- Variables numériques / continues ---
vars_numeriques <- c(
  "age_enf", "H_coucher_sem_heures", "poids_enf", "id_data_enf_x", "strate", "fpc"
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
  "nb_habitats_empl", "act_fer", "nb_consult_med_urg",
  "nb_AcVC_total", "nb_AcVC_3mois"
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
    
    # Regroupement des lieux d'accidents
    lieu_last_AcVC = case_when(
      lieu_last_AcVC %in% c("Autre (précisez)", "Sur un lieu de loisirs", "Au cours d’une activité sportive") ~ "Autre",
      TRUE ~ as.character(lieu_last_AcVC)
    ),
    
    # # Regroupement des terrains familiaux et locatifs
    type_ldv = case_when(
      !is.na(type_ldv) & type_ldv %in% c("Terrain familial", "Terrain locatif") ~ "Terrain familial/locatif",
      TRUE ~ type_ldv
    ),
    
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
  ) %>% # Renommer cloture_yn à "absence_cloture" 
  mutate(absence_cloture = as.factor(ifelse(cloture_yn == "Oui", "Non",
                                            ifelse(cloture_yn == "Non", "Oui", cloture_yn))))%>%
  # Regrouper en classes act_fer
  mutate(act_fer = case_when(
    str_detect(act_fer, "Décapage") ~ "Décapage",
    str_detect(act_fer, "Découpage|Fonte de métaux") ~ "Ferraille / Métaux",
    str_detect(act_fer, "Récupération") ~ "Récupération",
    str_detect(act_fer, "Stockage") ~ "Stockage",
    str_detect(act_fer, "Démontage de voitures") ~ "Démontage véhicules",
    str_detect(act_fer, "Ravalement") ~ "Ravalement",
    TRUE ~ NA
  )) 


write_xlsx(df, "tableau_final_corrige.xlsx")

############################# Tableaux des fréquences ###################################


# --- Tableau de fréquences pour variables ordinales et facteurs ---
freq_ordinales <- df %>%
  select(all_of(vars_ordinales)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "valeur") %>%
  group_by(variable, valeur) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(variable) %>%
  mutate(pct = round(100 * n / sum(n, na.rm = TRUE), 1))

vars_facteurs_sans_H <- setdiff(vars_facteurs, "H_coucher_sem")
freq_facteurs <- df %>%
  select(all_of(vars_facteurs_sans_H)) %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "valeur") %>%
  group_by(variable, valeur) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(variable) %>%
  mutate(pct = round(100 * n / sum(n, na.rm = TRUE), 1))


# --- Statistiques descriptives pour variables numériques ---
stats_numeriques <- df %>%
  select(all_of(vars_numeriques)) %>%
  summarise(across(everything(),
                   list(
                     n = ~sum(!is.na(.)),
                     n_na = ~sum(is.na(.)),
                     mean = ~mean(., na.rm = TRUE),
                     sd = ~sd(., na.rm = TRUE),
                     min = ~min(., na.rm = TRUE),
                     max = ~max(., na.rm = TRUE)
                   ),
                   .names = "{.col}_{.fn}")) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("variable", "stat"),
    names_pattern = "^(.*)_(n|n_na|mean|sd|min|max)$",
    values_to = "valeur"
  ) %>%
  arrange(variable, stat)

# Créer une liste avec les trois tableaux
liste_onglets <- list(
  freq_ordinales = freq_ordinales,
  freq_facteurs = freq_facteurs,
  stats_numeriques = stats_numeriques
)

# Écrire le fichier Excel avec plusieurs onglets
write_xlsx(liste_onglets, path = "frequences.xlsx")




###################### Distribution nb_AcVC_total ######################

#  Ajustement Quasi-Poisson
quasi_model <- glm(nb_AcVC_total ~ 1, data = df, family = quasipoisson)
lambda_quasi <- exp(coef(quasi_model))       # moyenne
disp_quasi <- summary(quasi_model)$dispersion # dispersion

# Ajustement Binomiale négative
nb_model <- glm.nb(nb_AcVC_total ~ 1, data = df)

#  Taille de l'échantillon et valeurs
n_total <- sum(!is.na(df$nb_AcVC_total))
valeurs <- c(1, 2, 3, 5)

#  Tableau comparatif avec probabilités
df_tab <- tibble(
  valeur = valeurs,
  n_observe = as.numeric(table(factor(df$nb_AcVC_total, levels = valeurs))),
  prob_quasi = dpois(valeurs, lambda_quasi),          # approx Poisson pour la probabilité
  nb_attendu_quasi = prob_quasi * n_total,
  prob_nb = dnbinom(valeurs, size = nb_model$theta, mu = exp(coef(nb_model))),
  nb_attendu_nb = prob_nb * n_total
)

print(df_tab)

#  Préparer pour ggplot
df_long <- df_tab %>%
  select(valeur, n_observe, nb_attendu_quasi, nb_attendu_nb) %>%
  pivot_longer(cols = -valeur, names_to = "type", values_to = "count")

#  Graphique comparatif
ggplot(df_long, aes(x = factor(valeur), y = count, fill = type)) +
  geom_col(position = "dodge") +
  labs(x = "Nombre d'événements", y = "Nombre d'observations",
       fill = "Type", title = "Comparaison Observé vs Quasi-Poisson & Binomiale négative") +
  scale_fill_manual(values = c("n_observe" = "steelblue",
                               "nb_attendu_quasi" = "orange",
                               "nb_attendu_nb" = "darkgreen"),
                    labels = c("Observé", "Attendu Quasi-Poisson", "Attendu Binomiale négative")) +
  theme_minimal()



var(df$nb_AcVC_total, na.rm=TRUE)



###################### STATS ##############################

# --- Filtrer uniquement les observations non manquantes pour nb_AcVC_total ---
df_report <- df %>%
  mutate(across(all_of(vars_ordinales),
                ~ factor(.x, levels = c(0,1), labels = c("Non", "Oui")))) %>%
  # Recoder les ordinales 0/1 en facteurs "Non"/"Oui" pour affichage
  mutate(across(all_of(vars_facteurs), ~ factor(.x))) # convertir toutes les variables factorielles de string à facteur 

# --- Combinaison de toutes les variables explicatives ---
vars_explicatives <- setdiff(c(vars_facteurs, vars_ordinales, vars_numeriques),
                             c("nb_AcVC_total", "AcVC_yn", "mode_chauf_autre",
                               "H_coucher_sem_heures", "H_coucher_sem"))


# --- Nouvelle version de la fonction qui retourne un tibble "long" ---
test_quasipoisson_long_expand <- function(var_name, data) {
  
  # Filtrer les NA et convertir nb_AcVC_total en numérique
  data_var <- data %>%
    filter(!is.na(.data[[var_name]]), !is.na(nb_AcVC_total)) %>%
    mutate(nb_AcVC_total = as.numeric(as.character(nb_AcVC_total)))
  
  # Vérifier nombre d'observations et de modalités
  if(nrow(data_var) < 2 || length(unique(data_var[[var_name]])) < 2) {
    return(tibble(
      variable = var_name,
      n_obs = nrow(data_var),
      F_value = NA_real_,
      p_value = NA_real_,
      modalite = NA_character_,
      OR = NA_real_,
      OR_low = NA_real_,
      OR_high = NA_real_
    ))
  }
  
  safe_result <- tryCatch({
    # Ajuster le modèle quasi-Poisson
    model <- glm(as.formula(paste("nb_AcVC_total ~", var_name)),
                 data = data_var, family = quasipoisson)
    
    # ANOVA F test
    anova_res <- tryCatch(car::Anova(model, test = "F"), error = function(e) NULL)
    F_val <- if(!is.null(anova_res)) anova_res$`F value`[1] else NA_real_
    p_val <- if(!is.null(anova_res)) anova_res$`Pr(>F)`[1] else NA_real_
    
    # Coefficients exponentiés avec IC 95%
    coef_tbl <- broom::tidy(model, exponentiate = TRUE, conf.int = TRUE)
    if(nrow(coef_tbl) == 0) return(NULL)
    
    # Modalité de référence
    ref_mod <- levels(data_var[[var_name]])[1]
    
    # Nettoyer les noms de modalités et remplacer l’Intercept par la référence
    coef_tbl <- coef_tbl %>%
      mutate(modalite = ifelse(term == "(Intercept)", ref_mod,
                               gsub(paste0("^", var_name), "", term)))
    
    # Construire le tibble final
    tibble(
      variable = var_name,
      n_obs = nrow(data_var),
      F_value = F_val,
      p_value = p_val,
      modalite = coef_tbl$modalite,
      OR = round(coef_tbl$estimate, 3),
      OR_low = round(coef_tbl$conf.low, 3),
      OR_high = round(coef_tbl$conf.high, 3)
    )
    
  }, error = function(e) {
    tibble(
      variable = var_name,
      n_obs = nrow(data_var),
      F_value = NA_real_,
      p_value = NA_real_,
      modalite = NA_character_,
      OR = NA_real_,
      OR_low = NA_real_,
      OR_high = NA_real_
    )
  })
  
  return(safe_result)
}

# --- Appliquer à toutes les variables ---
results_long_expanded <- map_dfr(vars_explicatives, ~test_quasipoisson_long_expand(.x, df_report)) %>%
  mutate(OR_CI = paste0("[", OR_low, "; ", OR_high, "]")) %>%
  select(variable, n_obs, F_value, p_value, modalite, OR, OR_CI)

# --- Afficher le résultat ---
results_long_expanded



# Exporter le tableau en Excel
write_xlsx(results_long_expanded, path = "results_quasipoisson.xlsx")





########################################################

# --- Définir le design pondéré ---
ENFp <- svydesign(
  ids = ~id_data_enf_x,
  strata = ~strate,
  weights = ~poids_enf,
  fpc = ~fpc,
  data = data_final
)
options(survey.lonely.psu = "remove")

# --- Fonction quasi-Poisson pondérée ---
# Fonction quasi-Poisson pondérée avec regTermTest()
test_quasipoisson_svy2 <- function(var_name, design) {
  
  data_var <- subset(design, !is.na(design$variables[[var_name]]) & !is.na(design$variables$nb_AcVC_total))
  
  if(length(unique(data_var$variables[[var_name]])) < 2 || nrow(data_var$variables) < 2) {
    return(tibble(
      variable = var_name,
      n_obs = nrow(data_var$variables),
      F_value = NA_real_,
      p_value = NA_real_,
      modalite = NA_character_,
      OR = NA_real_,
      OR_low = NA_real_,
      OR_high = NA_real_
    ))
  }
  
  safe_result <- tryCatch({
    
    model <- svyglm(
      as.formula(paste("nb_AcVC_total ~", var_name)),
      design = data_var,
      family = quasipoisson()
    )
    
    # Test global F pour la variable
    test_res <- regTermTest(model, as.formula(paste("~", var_name)))
    F_val <- test_res$F[1]
    p_val <- test_res$p[1]
    
    # Coefficients exponentiés
    coef_tbl <- broom::tidy(model, exponentiate = TRUE, conf.int = TRUE)
    
    ref_mod <- levels(data_var$variables[[var_name]])[1]
    coef_tbl <- coef_tbl %>%
      mutate(modalite = ifelse(term == "(Intercept)", ref_mod,
                               gsub(paste0("^", var_name), "", term)))
    
    tibble(
      variable = var_name,
      n_obs = nrow(data_var$variables),
      F_value = F_val,
      p_value = p_val,
      modalite = coef_tbl$modalite,
      OR = round(coef_tbl$estimate, 3),
      OR_low = round(coef_tbl$conf.low, 3),
      OR_high = round(coef_tbl$conf.high, 3)
    )
    
  }, error = function(e) {
    tibble(
      variable = var_name,
      n_obs = nrow(data_var$variables),
      F_value = NA_real_,
      p_value = NA_real_,
      modalite = NA_character_,
      OR = NA_real_,
      OR_low = NA_real_,
      OR_high = NA_real_
    )
  })
  
  return(safe_result)
}

# --- Appliquer à toutes les variables explicatives ---
vars_explicatives <- setdiff(c(vars_facteurs, vars_ordinales, vars_numeriques),
                             c("nb_AcVC_total", "AcVC_yn", "mode_chauf_autre",
                               "H_coucher_sem_heures", "H_coucher_sem"))

results_svy <- map_dfr(vars_explicatives, ~ test_quasipoisson_svy(.x, ENFp)) %>%
  mutate(OR_CI = paste0("[", OR_low, "; ", OR_high, "]")) %>%
  select(variable, n_obs, F_value, p_value, modalite, OR, OR_CI)
results_svy





