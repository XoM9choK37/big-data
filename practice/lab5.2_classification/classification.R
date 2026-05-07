set.seed(1234)

library(e1071)
library(party)
library(randomForest)
library(caret)
library(tidyverse)

df <- read.csv("titanic.csv", stringsAsFactors = TRUE)

df_model <- df %>%
  select(cluster, Pclass, Sex, Age, SibSp, Parch, Fare, Embarked) %>%
  mutate(
    Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age),
    Embarked = ifelse(is.na(Embarked) | Embarked == "", "S", Embarked),
    cluster = factor(cluster, levels = c("Бедные", "Средние", "Богатые")),
    Pclass = factor(Pclass),
    Sex = factor(Sex),
    Embarked = factor(Embarked)
  ) %>%
  na.omit()

df_model$Embarked <- droplevels(df_model$Embarked)

cat("РАЗМЕР ДАННЫХ:", dim(df_model), "\n")
print(table(df_model$cluster))

ind <- sample(2, nrow(df_model), replace = TRUE, prob = c(0.7, 0.3))
trainData <- df_model[ind == 1, ]
testData  <- df_model[ind == 2, ]

cat("\nTRAIN:", nrow(trainData), " | TEST:", nrow(testData), "\n")

cat("\nНАИВНЫЙ БАЙЕС:\n")

nb_model <- naiveBayes(cluster ~ ., data = trainData)

nb_train_pred <- predict(nb_model, newdata = trainData)
cat("МАТРИЦА ОШИБОК НА TRAIN (НАИВНЫЙ БАЙЕС):\n")
print(table(Факт = trainData$cluster, Прогноз = nb_train_pred))
cat("ТОЧНОСТЬ НА TRAIN:", round(mean(nb_train_pred == trainData$cluster) * 100, 2), "%\n")

nb_pred <- predict(nb_model, newdata = testData)
cat("\nМАТРИЦА ОШИБОК НА TEST (НАИВНЫЙ БАЙЕС):\n")
print(confusionMatrix(nb_pred, testData$cluster))

nb_acc <- mean(nb_pred == testData$cluster)
cat("ТОЧНОСТЬ НАИВНОГО БАЙЕСА (TEST):", round(nb_acc * 100, 2), "%\n")

nb_tables <- nb_model$tables

par(mfrow = c(2, 2))

for (var in c("Age", "Fare", "SibSp", "Parch")) {
  means <- nb_tables[[var]][, 1]
  sds   <- nb_tables[[var]][, 2]
  
  x_range <- range(df_model[[var]], na.rm = TRUE)
  x_vals  <- seq(x_range[1], x_range[2], length.out = 300)
  
  y_max <- max(sapply(1:3, function(i) dnorm(means[i], means[i], sds[i]))) * 1.15
  
  plot(NULL, xlim = x_range, ylim = c(0, y_max),
       xlab = var, ylab = "Плотность вероятности",
       main = paste("Распределение:", var))
  
  colors <- c("#FC4E07", "#E7B800", "#2E9FDF")
  for (i in 1:3) {
    lines(x_vals, dnorm(x_vals, means[i], sds[i]), 
          col = colors[i], lwd = 2.5)
  }
  legend("topright", 
         legend = c("Бедные", "Средние", "Богатые"),
         col = colors, lwd = 2.5, cex = 0.5, bty = "n")
}

par(mfrow = c(1, 1))

cat("\nДЕРЕВО РЕШЕНИЙ:\n")

ctree_model <- ctree(cluster ~ ., data = trainData)

plot(ctree_model, main = "Дерево решений")

cat("МАТРИЦА ОШИБОК НА TRAIN:\n")
print(table(Прогноз = predict(ctree_model), Факт = trainData$cluster))

ctree_pred <- predict(ctree_model, newdata = testData)
cat("\nМАТРИЦА ОШИБОК НА TEST:\n")
print(confusionMatrix(ctree_pred, testData$cluster))

ctree_acc <- mean(ctree_pred == testData$cluster)
cat("ТОЧНОСТЬ ДЕРЕВА РЕШЕНИЙ:", round(ctree_acc * 100, 2), "%\n")

cat("\nСЛУЧАЙНЫЙ ЛЕС:\n")

rf_model <- randomForest(cluster ~ ., data = trainData, ntree = 200, 
                         proximity = TRUE, importance = TRUE)

print(rf_model)

rf_pred <- predict(rf_model, newdata = testData)
cat("\nМАТРИЦА ОШИБОК НА TEST:\n")
print(confusionMatrix(rf_pred, testData$cluster))

rf_acc <- mean(rf_pred == testData$cluster)
cat("ТОЧНОСТЬ СЛУЧАЙНОГО ЛЕСА:", round(rf_acc * 100, 2), "%\n")

cat("\nВАЖНОСТЬ ПЕРЕМЕННЫХ:\n")
print(importance(rf_model))
varImpPlot(rf_model, main = "Важность признаков в случайном лесу")

cat("\nИТОГОВАЯ СВОДКА:\n")
results <- data.frame(
  Model = c("Naive Bayes", "Decision Tree", "Random Forest"),
  Accuracy_Test = round(c(nb_acc, ctree_acc, rf_acc) * 100, 2)
)
print(results)