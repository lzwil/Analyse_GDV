library(dplyr)
library(readxl)
library(writexl)
library(janitor)
library(ggplot2)

setwd("C:/Users/leozw/Documents/Analyse_GDV")

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
