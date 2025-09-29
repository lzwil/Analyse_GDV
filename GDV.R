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

# Variables continues vs ordinales
vars_continuous <- c("H_coucher_sem_heures", "age_enf", "poidskg_enf", "taille_enf")
vars_ordinal <- setdiff(vars, vars_continuous)

results_pub <- lapply(vars, function(v) {
  if (v %in% vars_continuous) {
    # Moyenne et SD
    mean_Non <- mean(data[[v]][data$AcVC_yn == "Non"], na.rm = TRUE)
    sd_Non <- sd(data[[v]][data$AcVC_yn == "Non"], na.rm = TRUE)
    mean_Oui <- mean(data[[v]][data$AcVC_yn == "Oui"], na.rm = TRUE)
    sd_Oui <- sd(data[[v]][data$AcVC_yn == "Oui"], na.rm = TRUE)
    
    # Test t
    test <- t.test(data[[v]] ~ data$AcVC_yn)
    
    value_Non <- sprintf("%.1f [%.1f]", med_Non, IQR_Non)
    value_Oui <- sprintf("%.1f [%.1f]", med_Oui, IQR_Oui)
    test_used <- "t-test"
    
  } else {
    # Médiane et IQR
    med_Non <- median(data[[v]][data$AcVC_yn == "Non"], na.rm = TRUE)
    IQR_Non <- IQR(data[[v]][data$AcVC_yn == "Non"], na.rm = TRUE)
    med_Oui <- median(data[[v]][data$AcVC_yn == "Oui"], na.rm = TRUE)
    IQR_Oui <- IQR(data[[v]][data$AcVC_yn == "Oui"], na.rm = TRUE)
    
    # Test de Wilcoxon
    test <- wilcox.test(data[[v]] ~ data$AcVC_yn)
    
    value_Non <- sprintf("%d [%d]", med_Non, IQR_Non)
    value_Oui <- sprintf("%d [%d]", med_Oui, IQR_Oui)
    test_used <- "Wilcoxon"
  }
  
  data.frame(
    variable = v,
    test = test_used,
    p_value = test$p.value,
    `Non` = value_Non,
    `Oui` = value_Oui
  )
})

results_pub_df <- do.call(rbind, results_pub)
rownames(results_pub_df) <- NULL

# Affiche le tableau final prêt pour publication
results_pub_df







