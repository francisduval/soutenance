# Chargement des bibliothèques
library(ggplot2)
library(gridExtra)  
library(neuralnet)
theme_set(theme_minimal())

# Définition de la vraie relation sous-jacente
true_function <- function(x) sin(2 * pi * x)

# Génération de données synthétiques
set.seed(123)
n <- 100
x <- sort(runif(n))
y <- true_function(x) + rnorm(n, mean = 0, sd = 0.2)

# Création du data frame
data <- data.frame(x = x, y = y)

# Définition de la fonction pour ajuster un modèle polynomial
fit_polynomial <- function(data, degree) {
  model <- lm(y ~ poly(x, degree), data = data)
  return(model)
}

# Ajustement de modèles de différents degrés de polynômes
models <- list()
models[[1]] <- fit_polynomial(data, 1)  # Modèle sous-ajusté
models[[2]] <- fit_polynomial(data, 4)  # Modèle bon ajustement
models[[3]] <- fit_polynomial(data, 20) # Modèle surajusté


# Création d'un ensemble de données pour les prédictions
new_data <- data.frame(x = seq(0, 1, by = 0.01))


# Calcul des prédictions pour chaque modèle
predictions <- matrix(NA, length(new_data$x), length(models))
for (i in 1:length(models)) {
  predictions[, i] <- predict(models[[i]], newdata = new_data)
}

# Création d'un data frame pour les prédictions
predictions_df <- data.frame(new_data, predictions)


p1 <- 
  ggplot(data, aes(x = x, y = y)) +
  geom_point(size = 0.5) +
  geom_line(data = predictions_df, aes(x = x, y = predictions_df[ , 2]), color = "red") + 
  xlab("Sous-ajustement") +
  ylab(NULL)


p2 <- 
  ggplot(data, aes(x = x, y = y)) +
  geom_point(size = 0.5) +
  geom_line(data = predictions_df, aes(x = x, y = predictions_df[ , 3]), color = "green") + 
  xlab("Bon modèle") +
  ylab(NULL)

p3 <- 
  ggplot(data, aes(x = x, y = y)) +
  geom_point(size = 0.5) +
  geom_line(data = predictions_df, aes(x = x, y = predictions_df[ , 4]), color = "red") + 
  xlab("Surajustement") +
  ylab(NULL)

grid.arrange(p1, p2, p3, ncol = 3)
