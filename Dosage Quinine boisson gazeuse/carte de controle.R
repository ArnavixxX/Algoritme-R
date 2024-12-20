# Charger les bibliothèques nécessaires
library(readxl)
library(dplyr)
library(ggplot2)
library(deming)

# Importer les données
data <- "Analyse metrologique.xlsx"
data<- read_excel(data, sheet = "Datas")
data <- data[,-5 ]  
data <- data %>%
  mutate(
    Intensite = as.numeric(Intensite),
    Concentration = as.numeric(Concentration),
    Operateur = as.factor(Operateur),
    Date = as.Date(Date)  # Conversion en type Date
  )
str(data)

# Séparer les données en standards et inconnues (NA)
standards <- data %>% filter(!is.na(Concentration))
unknown <- data %>% filter(is.na(Concentration))

# Grouper les données standards par Concentration, Date et Operateur
standards_grouped <- standards %>%
  group_by(Concentration, Date, Operateur) %>%
  summarize(Moyenne_Intensite = mean(Intensite, na.rm = TRUE), .groups = 'drop')

# Grouper les données inconnues par Date et Operateur
unknown_grouped <- unknown %>%
  group_by(Date, Operateur) %>%
  summarize(Moyenne_Intensite = mean(Intensite, na.rm = TRUE), .groups = 'drop')

# Stocker les graphiques dans des variables
plots <- list() # Liste pour stocker les graphiques

unique_concentrations <- unique(standards_grouped$Concentration)

for (concentration in unique_concentrations) {
  # Sous-ensemble des données pour la concentration actuelle
  subset <- standards_grouped %>% filter(Concentration == concentration)
  
  # Calcul des limites de contrôle
  moyenne <- mean(subset$Moyenne_Intensite, na.rm = TRUE)
  ecart_type <- sd(subset$Moyenne_Intensite, na.rm = TRUE)
  LCL <- moyenne - 3 * ecart_type
  UCL <- moyenne + 3 * ecart_type
  
  p <- ggplot(subset, aes(x = interaction(Date, Operateur), y = Moyenne_Intensite)) +
    # Points colorés selon leur position par rapport aux limites
    geom_point(aes(color = ifelse(Moyenne_Intensite < LCL | Moyenne_Intensite > UCL, "Hors limites", "Dans les limites")), 
               size = 4, shape = 16) +
    geom_line(group = 1, color = "grey50", linetype = "solid") +
    
    # Lignes de contrôle
    geom_hline(yintercept = moyenne, color = "darkgreen", linetype = "dashed", size = 1.2) +
    geom_hline(yintercept = LCL, color = "red", linetype = "dashed", size = 1.2) +
    geom_hline(yintercept = UCL, color = "blue", linetype = "dashed", size = 1.2) +
    
    # Labels
    labs(
      title = paste("Carte de Contrôle - Concentration", concentration),
      x = "Date - Opérateur",
      y = "Moyenne Intensité",
      color = "État du point"
    ) +
    
    # Thème personnalisé
    theme_bw() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y = element_text(size = 12),
      axis.title = element_text(size = 14, face = "bold"),
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      legend.position = "bottom",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10)
    )
  
  # Stocker le graphique dans une liste
  plots[[paste0("Concentration_", concentration)]] <- p
}

# Carte de contrôle pour la solution inconnue
moyenne_unknown <- mean(unknown_grouped$Moyenne_Intensite, na.rm = TRUE)
ecart_type_unknown <- sd(unknown_grouped$Moyenne_Intensite, na.rm = TRUE)
LCL_unknown <- moyenne_unknown - 3 * ecart_type_unknown
UCL_unknown <- moyenne_unknown + 3 * ecart_type_unknown

plot_unknown <- ggplot(unknown_grouped, aes(x = interaction(Date, Operateur), y = Moyenne_Intensite)) +
  geom_point(size = 3) +
  geom_line(group = 1) +
  geom_hline(yintercept = moyenne_unknown, color = "green", linetype = "dashed") +
  geom_hline(yintercept = LCL_unknown, color = "red", linetype = "dashed") +
  geom_hline(yintercept = UCL_unknown, color = "blue", linetype = "dashed") +
  labs(
    title = "Carte de Contrôle - Solution Inconnue",
    x = "Date - Opérateur",
    y = "Moyenne Intensité"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14),
    plot.title = element_text(size = 16)
  )

# Stocker le graphique inconnu dans la liste
plots[["Solution_Inconnue"]] <- plot_unknown

# Afficher un graphique spécifique
# Exemple : print(plots[["Concentration_10.25"]]) ou print(plots[["Solution_Inconnue"]])
print(plots[["Concentration_10.25"]])
print(plots[["Concentration_8.2"]])
print(plots[["Concentration_6.15"]])
print(plots[["Concentration_4.1"]])
print(plots[["Concentration_2.05"]])
print(plots[["Concentration_0"]])

print(plots[["Solution_Inconnue"]])






library(readxl)
library(dplyr)
library(MethComp)
library(deming)
library(ggplot2)

# Étape 1 : Importation des données
data <- read_excel("Analyse metrologique.xlsx", sheet = "Datas")

# Nettoyage et conversion des colonnes
data <- data %>%
  mutate(
    Intensite = as.numeric(Intensite),
    Concentration = as.numeric(Concentration),
    Operateur = as.factor(Operateur),
    Date = as.Date(Date)  # Conversion en type Date
  )

# Étape 2 : Calcul des Moyennes et Écart-types par Concentration et Gamme
moyenne <- data %>%
  group_by(Date, Operateur, Concentration) %>%
  summarise(
    Moyenne_Intensite = mean(Intensite, na.rm = TRUE),
    Ecart_Type = sd(Intensite, na.rm = TRUE) / sqrt(20)  # Écart-type sur la moyenne
  ) %>%
  ungroup()

# Filtrer les lignes où Concentration est NA
ecart_type_na <- moyenne %>%
  filter(is.na(Concentration)) %>%
  select(Date, Operateur, Moyenne_Intensite, Ecart_Type)

# Afficher le nouveau DataFrame
print(ecart_type_na)

# Étape 3 : Fonction pour calculer Uy
calculer_uy <- function(ecart_type) {
  # Répétabilité (type A)
  a_repetabilite <- ecart_type
  k_repetabilite <- 2
  u_repetabilite <- a_repetabilite / k_repetabilite
  contribution_repetabilite <- (u_repetabilite * 1)^2
  
  # Spectrofluomètre (type B)
  a_spectro <- 0.005
  k_spectro <- sqrt(3)
  u_spectro <- a_spectro / k_spectro
  contribution_spectro <- (u_spectro * 1)^2
  
  # Incertitude totale
  U_tot <- sqrt(contribution_repetabilite + contribution_spectro)
  
  return(U_tot)
}

# Ajout de Uy au tableau des moyennes
moyenne <- moyenne %>%
  mutate(Uy = mapply(calculer_uy, Ecart_Type))

# Ux fixé pour chaque concentration
Ux <- c(0.000001, 0.0017, 0.0032, 0.0060, 0.010, 0.014)  # Exemple de valeurs fixes pour Ux

# Étape 4 : Régression de Deming et Calcul des Concentrations Inconnues
resultats <- moyenne %>%
  group_by(Date, Operateur) %>%
  group_split() %>%
  lapply(function(gamme) {
    # Filtrer les concentrations connues
    donnees_connues <- gamme %>% filter(!is.na(Concentration))
    
    # Vérifiez que les données connues ne sont pas vides
    if (nrow(donnees_connues) > 0) {
      # Régression de Deming
      modele <- deming(Moyenne_Intensite ~ Concentration, 
                       data = donnees_connues, 
                       xstd = Ux[1:nrow(donnees_connues)], 
                       ystd = donnees_connues$Uy)
      
      # Extraire les coefficients
      intercept <- modele$coefficients[1]
      pente <- modele$coefficients[2]
      # Calculer les incertitudes associées
      se_intercept <- sqrt(modele$variance[1,1])  # Incertitude sur l'intercept
      se_pente <- sqrt(modele$variance[2,2])      # Incertitude sur la pente
      
      # Calcul de la concentration inconnue
      inconnue <- gamme %>% filter(is.na(Concentration))
      if (nrow(inconnue) > 0) {
        Intensite_inconnue <- inconnue$Moyenne_Intensite
        Concentration_inconnue <- (Intensite_inconnue - intercept) / pente * 10
        
        
        # Ajouter les incertitudes moyennes Ux et Uy au résultat
        ux_moyenne <- mean(Ux[1:nrow(donnees_connues)], na.rm = TRUE)
        uy_moyenne <- mean(donnees_connues$Uy, na.rm = TRUE)
        
        # Retourner les résultats pour cette gamme
        return(data.frame(
          Date = unique(gamme$Date),
          Operateur = unique(gamme$Operateur),
          Concentration_inconnue = Concentration_inconnue,
          Pente = pente,
          Intercept = intercept,
          U_Pente = se_pente,
          U_Intercept = se_intercept,
          Dilution = 10,
          U_Dilution = 0.0000067
          
          
  
        ))
      }
    }
    return(NULL)  # Retourner NULL si pas de données connues
  }) %>%
  bind_rows()

# Étape 5 : Calcul des Limites de Contrôle pour la Carte
moyenne_concentration <- mean(resultats$Concentration_inconnue, na.rm = TRUE)*1.1
ecart_type_concentration <- sd(resultats$Concentration_inconnue, na.rm = TRUE)
LCL <- moyenne_concentration - 3 * ecart_type_concentration
UCL <- moyenne_concentration + 3 * ecart_type_concentration





ggplot(resultats, aes(x = interaction(Date, Operateur), y = Concentration_inconnue*1.1)) +
  # Points colorés selon leur position par rapport aux limites
  geom_point(aes(color = ifelse(Concentration_inconnue < LCL | Concentration_inconnue > UCL, 
                                "Hors limites", "Dans les limites")), size = 4, shape = 16) +
  
  # Ligne de tendance
  geom_line(group = 1, color = "grey50", linetype = "solid") +
  
  # Lignes de contrôle
  geom_hline(yintercept = moyenne_concentration, color = "darkgreen", linetype = "dashed", size = 1.2) +
  geom_hline(yintercept = LCL, color = "red", linetype = "dashed", size = 1.2) +
  geom_hline(yintercept = UCL, color = "blue", linetype = "dashed", size = 1.2) +
  
  # Labels
  labs(
    title = "Carte de Contrôle - Concentration Inconnue",
    x = "Date - Opérateur",
    y = "Concentration (mg/L)",
    color = "État du point"
  ) +
  
  # Thème personnalisé
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(size = 14, face = "bold"),
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    legend.position = "bottom",
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  )






# Affichage du DataFrame résultat avec Ux et Uy
print(resultats)

# Jointure avec le DataFrame 'resultats' pour ajouter Ecart_Type
resultats <- resultats %>%
  left_join(ecart_type_na, by = c("Date", "Operateur"))

# Afficher le DataFrame 'resultats' mis à jour
print(resultats)





















