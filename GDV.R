library(dplyr)
library(readxl)
library(writexl)
library(janitor)
library(ggplot2)

setwd("C:\\Users\\A159692\\Documents\\these_Axelle")

data_final <- read_xlsx("data_final.xlsx")

# rendre heures format lisible 
df <- data_final %>%
  mutate(
    # Conversion en heures décimales et remplacement des aberrations par NA
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
  )


# caractères en facteur
df <- df %>%
  mutate(across(where(is.character), as.factor))

# résumer tableau 
glimpse(df)

# equart type dela variable heure de coucher
sd(df$H_coucher_sem_heures, na.rm = TRUE)


write_xlsx(df, "tableau_final_corrige.xlsx")


########################### Analyse stat #######################


# Partir de ce tableau pour garder l'original
data <- df

# Colonnes à analyser
vars <- c("H_coucher_sem_heures", "age_enf", "poidskg_enf", "taille_enf", 
          "sdq_hyper1", "sdq_hyper2", "sdq_hyper3", "sdq_hyper4", "sdq_hyper5",
          "sdq_emot1", "sdq_emot2", "sdq_emot3", "sdq_emot4", "sdq_emot5",
          "sdq_comport1", "sdq_comport2", "sdq_comport3", "sdq_comport4")

# Variables continues vs ordinales
vars_continuous <- c("H_coucher_sem_heures", "age_enf", "poidskg_enf", "taille_enf")
vars_ordinal <- setdiff(vars, vars_continuous)

# S'assurer qu'AcVC_yn est un facteur
data$AcVC_yn <- factor(data$AcVC_yn, levels = c("Non", "Oui"))

# Résumé par groupe
library(dplyr)
data %>%
  select(all_of(vars), AcVC_yn) %>%
  group_by(AcVC_yn) %>%
  summarise(across(everything(), list(mean = ~mean(.x, na.rm = TRUE),
                                      sd = ~sd(.x, na.rm = TRUE))))

# Calcul des tests
results <- lapply(vars, function(v) {
  if (v %in% vars_continuous) {
    # Test t pour variables continues
    test <- t.test(data[[v]] ~ data$AcVC_yn)
  } else {
    # Test de Wilcoxon pour variables ordinales
    test <- wilcox.test(data[[v]] ~ data$AcVC_yn)
  }
  data.frame(
    variable = v,
    test_used = ifelse(v %in% vars_continuous, "t-test", "Wilcoxon"),
    p_value = test$p.value,
    mean_Non = mean(data[[v]][data$AcVC_yn == "Non"], na.rm = TRUE),
    mean_Oui = mean(data[[v]][data$AcVC_yn == "Oui"], na.rm = TRUE)
  )
})

# Combine tous les résultats en un seul data.frame
results_df <- do.call(rbind, results)
rownames(results_df) <- NULL

# Affiche le tableau final
results_df





