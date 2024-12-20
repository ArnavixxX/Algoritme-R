library(readxl)
library(dplyr)
library(MethComp)
library(deming)
library(ggplot2)

data <- "Analyse metrologique.xlsx"
data<- read_excel(data, sheet = "Datas")
data <- data[,-5 ] 
data <- data %>%
  mutate(Intensite = as.numeric(Intensite), 
         Concentration = as.numeric(Concentration),
         Operateur = as.factor(Operateur))
str(data)

# Calcul des moyennes, incertitudes, et nettoyage
moyenne <- data %>%
  group_by(Concentration) %>%
  summarise(
    Moyenne = mean(Intensite, na.rm = TRUE),
    Sigma = sd(Intensite, na.rm = TRUE) / sqrt(20)
  ) %>% arrange(Moyenne)

# Identifier et extraire les données pour la solution inconnue
ligne_na <- moyenne %>% filter(is.na(Concentration))
Intensite_inconnue <- ligne_na$Moyenne
Uncertainty_unknown_solution <- 0.67
moyenne <-  moyenne %>% filter(!is.na(Concentration)) %>%
  mutate(
    Concentration = as.numeric(Concentration),
    Ux = c(0.000001, 0.0017, 0.0032, 0.0060, 0.010, 0.014),
    Uy = c(0.0007, 0.21, 0.33, 0.80, 1.00, 1.48))

# Étape 2 : Régression de Deming
modele_deming <- deming(Moyenne ~ Concentration, data = moyenne, xstd = moyenne$Ux, ystd = moyenne$Uy)
Intercept <- modele_deming$coefficients[1]  # Intercept
Pente <- modele_deming$coefficients[2]  # Pente

# Calcul de x_inconnue
Concentration_inconnue <- (Intensite_inconnue - Intercept) / Pente

# Étape 3 : Calculs pour l'incertitude
sigma2 <- sum((moyenne$Moyenne - (Intercept + Pente * moyenne$Concentration))^2) / (length(moyenne$Concentration) - 2)
moyenne_concentration_inconnue <- mean(moyenne$Concentration)

# Contributions à l'incertitude
U_x_y <- Uncertainty_unknown_solution / Pente
U_x_Intercept <- sqrt(sigma2 / sum((moyenne$Concentration - moyenne_concentration_inconnue)^2)) / Pente  
U_x_Pente <- ((Intensite_inconnue - Intercept) * sqrt(sigma2 / sum((moyenne$Concentration - moyenne_concentration_inconnue)^2))) / (Pente^2)
U_dilution <- (Concentration_inconnue * (0.00001 / sqrt(3))) / 0.001

# Calcul de l'incertitude totale
U_x_totale <- sqrt(U_x_y^2 + U_x_Intercept^2 + U_x_Pente^2 + U_dilution^2)

# Résultats finaux
cat("Equation droite étalonnage issue de la méthode GGMR :",Pente,"C +",Intercept,
    "\nConcentration finale :", Concentration_inconnue * 10, "mg/L de Quinine", "à ~", U_x_totale * 10, "mg/L", "\n")



# Préparation des données pour le tracé
data_plot <- moyenne %>%
  mutate(
    y_pred = Intercept + Pente * Concentration  # Valeurs prédites par la droite de Deming
  )

# Tracé
plot <- ggplot(data_plot, aes(x = Concentration, y = Moyenne)) +
  # Points avec barres d'erreur
  geom_point(size = 3, shape = 21, fill = "blue", color = "black") +
  geom_errorbar(aes(ymin = Moyenne - Uy, ymax = Moyenne + Uy), width = 0.1, color = "black") +
  geom_errorbarh(aes(xmin = Concentration - Ux, xmax = Concentration + Ux), height = 0.1, color = "black") +
  
  # Droite d'étalonnage
  geom_abline(intercept = Intercept, slope = Pente, color = "red", linewidth = 1, linetype = "solid") +
  
  # Ajustements esthétiques
  labs(
    title = "Régression de Deming pour l'étalonnage",
    x = "Concentration (mg/L)",
    y = "Intensité moyenne (Unité arbitraire)"
  ) +
  theme_minimal() +  # Thème épuré
  
  # Ajout d'un style inspiré des articles scientifiques
  theme(
    text = element_text(size = 14, family = "serif"),  # Texte en police serif pour un style académique
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Titre centré et en gras
    axis.title.x = element_text(size = 14, face = "bold"),  # Axe X
    axis.title.y = element_text(size = 14, face = "bold"),  # Axe Y
    axis.text = element_text(size = 12),  # Texte des axes
    panel.grid = element_line(color = "grey90"),  # Grille légère
    panel.background = element_rect(fill = "white"),  # Arrière-plan blanc
    plot.background = element_rect(fill = "white"),  # Fond du tracé
    legend.position = "none"  # Pas de légende pour simplifier
  )

# Afficher le graphique
print(plot)

