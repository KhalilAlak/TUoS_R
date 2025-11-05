library(tidyverse)
library(MASS)

data("iris")
iris[1:5, ]

iris <- iris[sample(1:nrow(iris)),]
iris[1:5,]

train_size = 0.7
iris_train <- iris[1:(train_size*nrow(iris)),]
iris_test <- iris[(nrow(iris_train)+1):nrow(iris),]

colors <- c('#1b9e77', '#d95f02', '#7570b3')
iris_train_colors <- colors[as.numeric(iris_train$Species)]
shapes <- c('o', '+', 'x')
iris_train_shapes <- shapes[as.numeric(iris_train$Species)]

ggplot(data=iris_train, aes(x=Sepal.Length, y=Sepal.Width)) +
  geom_point(color=iris_train_colors, shape=iris_train_shapes, size=5)

binaryColors <- function(data, species) {
  tf_values <- data$Species == species
  color_indices <- as.numeric(tf_values) + 1
  return(colors[color_indices])
}

binaryShapes <- function(data, species) {
  tf_values <- data$Species == species
  shape_indices <- as.numeric(tf_values) + 1
  return(shapes[shape_indices])
}

binarySpecies = 'virginica'
ggplot(data=iris_train, aes(x=Petal.Length, y=Petal.Width)) +
  geom_point(color=binaryColors(iris_train, binarySpecies),
             shape=binaryShapes(iris_train, binarySpecies),
             size=5)

iris_train$binarySpecies <- iris_train$Species == 'virginica'
iris_train$binarySpecies <- iris_train$binarySpecies * 1

iris_binary_model <- glm(
  binarySpecies ~ Petal.Width + Petal.Length,
  family = binomial(link = 'logit'),
  data = iris_train
)

binomial_probabilities <- predict(
  iris_binary_model,
  newdata = iris_test,
  type = 'response'
)

binomial_predictions <- ifelse(
  binomial_probabilities > 0.5,
  1,
  0
)

iris_test$binarySpecies <- iris_test$Species == 'virginica'
iris_test$binarySpecies <- iris_test$binarySpecies * 1

binomial_classification_error <- mean(
  binomial_predictions != iris_test$binarySpecies
)

print(paste('Accuracy', 1 - binomial_classification_error))

library(rpart)
install.packages("caTools")
library(caTools)
library(ggplot2)
library(dplyr)

decision_tree_model <- rpart(
  binarySpecies ~ Petal.Width + Petal.Length,
  data = iris_train,
  method = "class"
)

plot(decision_tree_model)
text(decision_tree_model)

library(nnet)

iris_multinomial_model <- multinom(
  Species ~ Sepal.Length + Sepal.Width + Petal.Width + Petal.Length,
  data = iris_train
)

decision_tree_predictions <- predict(
  decision_tree_model,
  newdata = iris_test,
  type = "class"
)

dt_classification_error <- mean(
  decision_tree_predictions != iris_test$binarySpecies
)
print(paste('Accuracy', 1 - dt_classification_error))

confusion_matrix_dt <- table(iris_test$binarySpecies, decision_tree_predictions)
print(confusion_matrix_dt)

confusion_matrix_dt_df <- as.data.frame(confusion_matrix_dt)
colnames(confusion_matrix_dt_df) <- c("Actual", "Predicted", "Freq")

ggplot(confusion_matrix_dt_df, aes(x = Predicted, y = Actual)) +
  geom_tile(aes(fill = Freq), color = "white") +
  geom_text(aes(label = Freq), vjust = 0.5, fontface = "bold", size = 5) +
  scale_fill_gradient(low = "lightblue", high = "steelblue") +
  labs(title = "Confusion Matrix", x = "Predicted Label", y = "Actual Label") 
+
  theme_minimal()


library(nnet)

iris_multinomial_model <- multinom(
  Species ~ Sepal.Length + Sepal.Width + Petal.Width + Petal.Length,
  data = iris_train
)

mlr_predictions <- predict(
  iris_multinomial_model,
  newdata = iris_test,
  method = "class"
)
print(mlr_predictions)

confusion_matrix_mlr <- table(iris_test$Species, mlr_predictions)
print(confusion_matrix_mlr)

acc_mlr <- sum(diag(confusion_matrix_mlr) / sum(confusion_matrix_mlr))
print(paste('Accuracy', acc_mlr))

confusion_matrix_mlr_df <- as.data.frame(confusion_matrix_mlr)
colnames(confusion_matrix_mlr_df) <- c("Actual", "Predicted", "Freq")

ggplot(confusion_matrix_mlr_df, aes(x = Predicted, y = Actual)) +
  geom_tile(aes(fill = Freq), color = "white") +
  geom_text(aes(label = Freq), vjust = 0.5, fontface = "bold", size = 5) +
  scale_fill_gradient(low = "lightblue", high = "steelblue") +
  labs(title = "Confusion Matrix", x = "Predicted Label", y = "Actual Label") +
  theme_minimal()
#Execise 1
binarySpecies <- 'setosa'

ggplot(
  data = iris_train,
  aes(x = Petal.Length, y = Petal.Width)
) +
  geom_point(
    color = binaryColors(iris_train, binarySpecies),
    shape = binaryShapes(iris_train, binarySpecies),
    size = 5
  )
binarySpecies <- 'versicolor'

ggplot(
  data = iris_train,
  aes(x = Petal.Length, y = Petal.Width)
) +
  geom_point(
    color = binaryColors(iris_train, binarySpecies),
    shape = binaryShapes(iris_train, binarySpecies),
    size = 5
  )

#Execise 2
iris_train$binarySpecies <- iris_train$Species == 'virginica'
iris_train$binarySpecies <- iris_train$binarySpecies * 1
iris_binary_model_sepal <- glm(
  binarySpecies ~ Sepal.Length + Sepal.Width,
  family = binomial(link = 'logit'),
  data = iris_train
)

binomial_probabilities_sepal <- predict(
  iris_binary_model_sepal,
  newdata = iris_test,
  type = 'response'
)

binomial_predictions_sepal <- ifelse(
  binomial_probabilities_sepal > 0.5,
  1,
  0
)

iris_test$binarySpecies <- iris_test$Species == 'virginica'
iris_test$binarySpecies <- iris_test$binarySpecies * 1

error_sepal <- mean(binomial_predictions_sepal != iris_test$binarySpecies)
print(paste('Accuracy (Sepal features):', 1 - error_sepal))

#Execise 3
decision_tree_sepal_model <- rpart(
  binarySpecies ~ Sepal.Length + Sepal.Width,
  data = iris_train,
  method = "class"
)

decision_tree_sepal_predictions <- predict(
  decision_tree_sepal_model,
  newdata = iris_test,
  type = "class"
)

sepal_classification_error <- mean(
  decision_tree_sepal_predictions != iris_test$binarySpecies
)
print(paste('Accuracy', 1 - sepal_classification_error))

confusion_matrix_sepal <- table(iris_test$binarySpecies, decision_tree_sepal_predictions)
print(confusion_matrix_sepal)

confusion_matrix_sepal_df <- as.data.frame(confusion_matrix_sepal)
colnames(confusion_matrix_sepal_df) <- c("Actual", "Predicted", "Freq")

ggplot(confusion_matrix_sepal_df, aes(x = Predicted, y = Actual)) +
  geom_tile(aes(fill = Freq), color = "white") +
  geom_text(aes(label = Freq), vjust = 0.5, fontface = "bold", size = 5) +
  scale_fill_gradient(low = "lightblue", high = "steelblue") +
  labs(title = "Confusion Matrix (Sepal Features)", x = "Predicted Label", y = "Actual Label") +
  theme_minimal()

#Execise 4

iris_train$binarySpecies <- iris_train$Species == 'setosa'
iris_train$binarySpecies <- iris_train$binarySpecies * 1

decision_tree_setosa_model <- rpart(
  binarySpecies ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
  data = iris_train,
  method = "class"
)

decision_tree_setosa_predictions <- predict(
  decision_tree_setosa_model,
  newdata = iris_test,
  type = "class"
)

iris_test$binarySpecies <- iris_test$Species == 'setosa'
iris_test$binarySpecies <- iris_test$binarySpecies * 1

setosa_error <- mean(decision_tree_setosa_predictions != iris_test$binarySpecies)
print(paste('Setosa Accuracy:', 1 - setosa_error))

iris_train$binarySpecies <- iris_train$Species == 'versicolor'
iris_train$binarySpecies <- iris_train$binarySpecies * 1

decision_tree_versicolor_model <- rpart(
  binarySpecies ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
  data = iris_train,
  method = "class"
)

decision_tree_versicolor_predictions <- predict(
  decision_tree_versicolor_model,
  newdata = iris_test,
  type = "class"
)

iris_test$binarySpecies <- iris_test$Species == 'versicolor'
iris_test$binarySpecies <- iris_test$binarySpecies * 1

versicolor_error <- mean(decision_tree_versicolor_predictions != iris_test$binarySpecies)
print(paste('Versicolor Accuracy:', 1 - versicolor_error))

#End of Execise