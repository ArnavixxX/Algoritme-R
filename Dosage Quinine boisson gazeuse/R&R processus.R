library(stats)
library(readxl)
library(dplyr)
library(tidyr)
library(outliers)
# génération des données

data <- "Analyse metrologique.xlsx"
data<- read_excel(data, sheet = "Datas")
data <- data[,-5 ] 
data <- data %>%
  mutate(Intensite = as.numeric(Intensite), 
         Concentration = as.numeric(Concentration),
         Operateur = as.factor(Operateur))
str(data)


# Vérifier les valeurs uniques de concentration
unique_concentrations <- unique(data$Concentration)
length(unique_concentrations)


# Initialiser une liste pour stocker les data frames
concentration_dfs <- list()

# Boucle pour créer un data frame pour chaque concentration, incluant les NA
for (concentration in unique_concentrations) {
  # Gérer les NA
  if (is.na(concentration)) {
    subset_df <- data %>% filter(is.na(Concentration))
    concentration_name <- "Concentration_NA"
  } else {
    subset_df <- data %>% filter(Concentration == concentration)
    concentration_name <- paste0("Concentration_", concentration)
  }
  
  # Ajouter le data frame à la liste
  concentration_dfs[[concentration_name]] <- subset_df
  
  # Optionnel : Sauvegarder chaque data frame dans l'environnement global
  assign(concentration_name, subset_df)
}

# Vérifier les data frames créés
sapply(concentration_dfs, nrow)

# Exemple : Afficher les premières lignes du data frame pour NA
head(concentration_dfs$Concentration_NA)


#test absence variance excessive COCHRAN
# Initialiser une liste pour stocker les résultats
cochran_results <- data.frame(
  Concentration = character(),
  P_value_Cochran = numeric(),
  Analyse = character(),
  stringsAsFactors = FALSE
)

# Effectuer le test de Cochran pour chaque data frame de concentration
for (name in names(concentration_dfs)) {
  # Extraire le data frame correspondant
  df <- concentration_dfs[[name]]
  
  # Vérifier qu'il y a des données (éviter les erreurs avec Concentration_NA si vide)
  if (nrow(df) > 0) {
    # Effectuer le test de Cochran
    cochran_test <- cochran.test(Intensite ~ Operateur, data = df)
    
    # Ajouter les résultats au tableau récapitulatif
    cochran_results <- rbind(
      cochran_results,
      data.frame(
        Concentration = name,
        P_value_Cochran = cochran_test$p.value,
        Analyse = ifelse(cochran_test$p.value > 0.05, "Variance homogène", "Variance excessive"),
        stringsAsFactors = FALSE
      )
    )
  }
}

# Afficher le tableau récapitulatif
print(cochran_results)
#hypothese il n'y a pas de variance execible H0 (seuil a 0.05) 
#si < on rejete H0 
#si > il ne se passe rien donc on peut pas rejeter H0 donc pas de variance execive


#Tests Valeurs Aberrantes - test de grubbs
# Initialiser une liste pour stocker les résultats
grubbs_results <- data.frame(
  Concentration = character(),
  P_value_Grubbs = numeric(),
  Analyse = character(),
  stringsAsFactors = FALSE
)

# Boucle pour effectuer le test de Grubbs sur chaque concentration
for (name in names(concentration_dfs)) {
  # Extraire le data frame correspondant
  df <- concentration_dfs[[name]]
  
  # Vérifier qu'il y a des données
  if (nrow(df) > 0) {
    # Calculer les moyennes par opérateur
    moyennes_concentration <- tapply(df$Intensite, df$Operateur, mean, na.rm = TRUE)
    
    # Effectuer le test de Grubbs
    grubbs_test <- grubbs.test(x = moyennes_concentration)
    
    # Ajouter les résultats au tableau récapitulatif
    grubbs_results <- rbind(
      grubbs_results,
      data.frame(
        Concentration = name,
        P_value_Grubbs = grubbs_test$p.value,
        Analyse = ifelse(grubbs_test$p.value > 0.05, "Pas de valeurs aberrantes", "Valeurs aberrantes détectées"),
        stringsAsFactors = FALSE
      )
    )
  }
}

# Afficher le tableau récapitulatif
print(grubbs_results)
#H0 il n'y a pas de valeur aberrante
#p value superieure à 0.05 donc H0 pas rejeter
#pas de valeur aberrante




#Test variance sur la moyenne - ANOVA
# Initialiser une liste pour stocker les résultats
anova_results <- data.frame(
  Concentration = character(),
  P_value_ANOVA = numeric(),
  Analyse = character(),
  stringsAsFactors = FALSE
)

# Boucle pour effectuer l'ANOVA pour chaque concentration
for (name in names(concentration_dfs)) {
  # Extraire le data frame correspondant
  df <- concentration_dfs[[name]]
  
  # Vérifier qu'il y a des données
  if (nrow(df) > 0) {
    # Appliquer le modèle ANOVA
    anova_model <- aov(Intensite ~ Operateur, data = df)
    
    # Extraire les résultats de l'ANOVA
    anova_summary <- summary(anova_model)
    p_value <- anova_summary[[1]]["Operateur", "Pr(>F)"]
    
    # Ajouter les résultats au tableau récapitulatif
    anova_results <- rbind(
      anova_results,
      data.frame(
        Concentration = name,
        P_value_ANOVA = p_value,
        Analyse = ifelse(p_value > 0.05, "Pas de différence significative", "Différence significative"),
        stringsAsFactors = FALSE
      )
    )
  }
}

# Afficher le tableau récapitulatif
print(anova_results)




# Charger les bibliothèques nécessaires
library(lmtest)


# Initialiser un tableau récapitulatif
anova_validation_results <- data.frame(
  Concentration = character(),
  P_value_DW = numeric(),
  Conclusion_DW = character(),
  P_value_Shapiro = numeric(),
  Conclusion_Shapiro = character(),
  P_value_Bartlett = numeric(),
  Conclusion_Bartlett = character(),
  stringsAsFactors = FALSE
)

# Boucle pour valider les hypothèses pour chaque concentration
for (name in names(concentration_dfs)) {
  # Extraire le data frame correspondant
  df <- concentration_dfs[[name]]
  
  # Vérifier qu'il y a des données
  if (nrow(df) > 0) {
    # Appliquer le modèle linéaire
    lm_model <- lm(Intensite ~ Operateur, data = df)
    
    # 1. Test Durbin-Watson (indépendance des résidus)
    dw_test <- dwtest(lm_model)
    dw_pvalue <- dw_test$p.value
    dw_conclusion <- ifelse(dw_pvalue > 0.05, "Indépendants", "Dépendants")
    
    # 2. Test de normalité des résidus (Shapiro-Wilk)
    residus <- residuals(lm_model)
    shapiro_test <- shapiro.test(residus)
    shapiro_pvalue <- shapiro_test$p.value
    shapiro_conclusion <- ifelse(shapiro_pvalue > 0.05, "Normaux", "Non normaux")
    
    # 3. Test de Bartlett (homogénéité des variances)
    bartlett_test <- bartlett.test(Intensite ~ Operateur, data = df)
    bartlett_pvalue <- bartlett_test$p.value
    bartlett_conclusion <- ifelse(bartlett_pvalue > 0.05, "Homogènes", "Non homogènes")
    
    # Ajouter les résultats au tableau récapitulatif
    anova_validation_results <- rbind(
      anova_validation_results,
      data.frame(
        Concentration = name,
        P_value_DW = dw_pvalue,
        Conclusion_DW = dw_conclusion,
        P_value_Shapiro = shapiro_pvalue,
        Conclusion_Shapiro = shapiro_conclusion,
        P_value_Bartlett = bartlett_pvalue,
        Conclusion_Bartlett = bartlett_conclusion,
        stringsAsFactors = FALSE
      )
    )
  }
}

# Afficher le tableau récapitulatif
print(anova_validation_results)




library(ggplot2)

# Fonction pour créer un graphique individuel pour chaque concentration
create_plot <- function(data, concentration) {
  subset_data <- data %>% filter(Concentration == concentration)  # Filtrer les données pour la concentration spécifique
  ggplot(subset_data, aes(x = Operateur, y = Intensite, fill = Operateur)) +
    geom_boxplot(outlier.colour = "black", outlier.shape = 16, outlier.size = 2, alpha = 0.7) +  # Box plot
    geom_jitter(width = 0.2, alpha = 0.4, size = 2, color = "black") +  # Points légèrement transparents
    labs(
      title = paste("Comparaison des Intensités selon chaque opérateur:", concentration, "mg/L"),
      x = "Opérateur",
      y = "Intensité (Unité arbitraire)"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 12),
      legend.position = "none"
    )
}

# Liste des concentrations uniques
unique_concentrations <- unique(data$Concentration)

# Générer un graphique pour chaque concentration
plots <- lapply(unique_concentrations, function(conc) create_plot(data, conc))

# Afficher un graphique spécifique (exemple : pour la concentration de 10.25 mg/L)
print(plots[[which(unique_concentrations == 10.25)]])
print(plots[[which(unique_concentrations == 8.2)]])
print(plots[[which(unique_concentrations == 6.15)]])
print(plots[[which(unique_concentrations == 4.1)]])
print(plots[[which(unique_concentrations == 2.05)]])
print(plots[[which(unique_concentrations == 0)]])
print(plots[[which(unique_concentrations == )]])

#test 6 sigma
library(SixSigma)

data_clean <- data %>%
  mutate(Intensite = as.numeric(Intensite), 
         Concentration = as.numeric(Concentration),
         Operateur = as.factor(Operateur)) %>%
  filter(!is.na(Concentration))
# Visualisation des données nettoyées
print(data_clean)


# Appliquer le test Gage R&R sur le data frame global
gage_rr <- ss.rr(
  var = "Intensite",
  part = "Concentration",
  appr = "Operateur",
  data = data_clean,
  main = "Six Sigma Gage R&R - Global",
  sub = "Gage R&R Analysis"
)

# Afficher les résultats
print(gage_rr)


