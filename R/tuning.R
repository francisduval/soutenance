# Chargement des bibliothèques
library(randomForest)
library(ggplot2)

# Génération de données synthétiques avec plus de variables
set.seed(123)
n <- 100
num_features <- 5  # Nombre de variables/features
x <- matrix(runif(n * num_features), ncol = num_features)
y <- apply(x, 1, function(row) sum(row) + rnorm(1, mean = 0, sd = 0.2))

# Création du data frame
data <- data.frame(x, y)

# Hyperparamètres à régler
mtry_values <- c(1, 2, 3)         # Nombre de variables candidates à chaque split
max_depth_values <- c(5, 10, 15)  # Profondeur maximale d'un arbre

# Initialisation des variables pour stocker les résultats de la recherche
results <- data.frame()
for (mtry in mtry_values) {
  for (max_depth in max_depth_values) {
    rf_model <- randomForest(y ~ ., data = data, mtry = mtry, maxdepth = max_depth)
    predictions <- predict(rf_model, newdata = data)
    mse <- mean((predictions - data$y)^2)
    results <- rbind(results, data.frame(mtry = mtry, max_depth = max_depth, mse = mse))
  }
}

# Graphique pour visualiser les résultats de la recherche par grille
ggplot(results, aes(x = factor(mtry), y = factor(max_depth), fill = mse)) +
  geom_tile() +
  scale_fill_gradient() +
  labs(title = "Exemple de recherche par grille",
       x = "Nombre de variables candidates (mtry)",
       y = "Profondeur maximale d'un arbre (max_depth)",
       fill = "Erreur de validation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
