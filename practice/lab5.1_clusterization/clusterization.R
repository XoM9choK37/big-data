library(titanic)
library(tidyverse)
library(factoextra)
library(cluster)
library(scatterplot3d)
library(parameters)
library(NbClust)

set.seed(41)

data("titanic_train")

cat("Первые 10 строк:\n")
print(head(titanic_train, 10))

cat("\nСтатистическая сводка:\n")
print(summary(titanic_train))

sapply(titanic_train, function(x) mean(is.na(x)) * 100)

par(mfrow = c(2, 2))
hist(titanic_train$Age, main = "Возраст", col = "steelblue", xlab = "Возраст")
hist(titanic_train$Fare, main = "Стоимость билета", col = "steelblue", xlab = "Fare")
hist(titanic_train$SibSp, main = "Братья/сёстры/супруги", col = "steelblue", xlab = "SibSp")
hist(titanic_train$Parch, main = "Родители/дети", col = "steelblue", xlab = "Parch")
par(mfrow = c(1, 1))

df_clust <- titanic_train %>%
  select(Pclass, Age, SibSp, Parch, Fare, Embarked) %>%
  mutate(
    Embarked = replace_na(Embarked, "S"),
    Age      = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age)
  ) %>%
  na.omit()

dummy_vars <- model.matrix(~ Pclass + Embarked - 1, data = df_clust)
df_num <- cbind(df_clust %>% select(Age, SibSp, Parch, Fare), dummy_vars)

maxs <- apply(df_num, 2, max)
mins <- apply(df_num, 2, min)
df_scaled <- as.data.frame(scale(df_num, center = mins, scale = maxs - mins))

cat("\nРазмер подготовленных данных:", dim(df_scaled), "\n")

dist_mat <- dist(df_scaled, method = "euclidean")
hc <- hclust(dist_mat, "ward.D2")

p1 <- fviz_nbclust(df_scaled, kmeans, method = "wss") +
  labs(title = "Метод локтя", x = "Число кластеров k", y = "Сумма внутрикластерных квадратов (WSS)")
print(p1)

p2 <- fviz_nbclust(df_scaled, kmeans, method = "silhouette") +
  labs(title = "Метод силуэта", x = "Число кластеров k", y = "Средняя ширина силуэта")
print(p2)

gap_stat <- clusGap(df_scaled, FUN = kmeans, nstart = 25, K.max = 10, B = 50, iter.max = 20)
p3 <- fviz_gap_stat(gap_stat) + labs(title = "Gap-статистика")
print(p3)

n_clust <- n_clusters(df_scaled, package = c("easystats", "NbClust"), standardize = FALSE)
print(n_clust)
plot(n_clust)

cat("\nОптимальное число кластеров k = 3\n")

plot(hc, labels = FALSE, hang = -1, main = "Дендрограмма (метод Варда, k = 3)")
rect.hclust(hc, k = 3, border = c("#FC4E07", "#E7B800", "#2E9FDF"))

groups_orig <- cutree(hc, k = 3)

titanic_clust <- df_clust
titanic_clust$cluster_orig <- factor(groups_orig)
titanic_clust$Survived <- titanic_train$Survived[1:nrow(titanic_clust)]

fare_by_cluster <- tapply(titanic_clust$Fare, titanic_clust$cluster_orig, mean)
cluster_order <- names(sort(fare_by_cluster))
old_to_new <- setNames(c("1", "2", "3"), cluster_order)
titanic_clust$cluster <- factor(old_to_new[as.character(titanic_clust$cluster_orig)], 
                                levels = c("1", "2", "3"))

cluster_colors <- c("1" = "#FC4E07", "2" = "#E7B800", "3" = "#2E9FDF")

titanic_clust$Pclass_label <- paste(titanic_clust$Pclass, "класс")
titanic_clust$Survived_label <- ifelse(titanic_clust$Survived == 1, "Выжил", "Погиб")
titanic_clust$cluster_label <- factor(titanic_clust$cluster, 
                                      levels = c("1", "2", "3"),
                                      labels = c("Бедные", "Средние", "Богатые"))

p4 <- ggplot(titanic_clust, aes(x = cluster_label, fill = Pclass_label)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = function(x) paste0(round(x * 100), "%")) +
  scale_fill_manual(values = c("1 класс" = "gold", "2 класс" = "grey70", "3 класс" = "sienna4"), name = "Класс билета") +
  labs(title = "Распределение классов билетов по кластерам",
       x = "Кластер", y = "Доля") +
  theme_minimal(base_size = 14)
print(p4)

p5 <- ggplot(titanic_clust, aes(x = cluster_label, fill = Survived_label)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = function(x) paste0(round(x * 100), "%")) +
  scale_fill_manual(values = c("steelblue", "tomato"), name = "Исход") +
  labs(title = "Выживаемость по кластерам",
       x = "Кластер", y = "Доля") +
  theme_minimal(base_size = 14)
print(p5)

par(mfrow = c(2, 2))

boxplot(Age ~ cluster_label, data = titanic_clust, 
        main = "Возраст по кластерам", 
        col = cluster_colors,
        xlab = "Кластер", ylab = "Возраст (лет)")

boxplot(Fare ~ cluster_label, data = titanic_clust, 
        main = "Стоимость билета по кластерам", 
        col = cluster_colors,
        xlab = "Кластер", ylab = "Стоимость билета (Fare)")

boxplot(as.numeric(Pclass) ~ cluster_label, data = titanic_clust, 
        main = "Класс билета по кластерам", 
        col = cluster_colors,
        xlab = "Кластер", ylab = "Класс билета (1-3)")

boxplot(SibSp ~ cluster_label, data = titanic_clust, 
        main = "Семейное положение по кластерам (SibSp)", 
        col = cluster_colors,
        xlab = "Кластер", ylab = "Кол-во братьев/сестёр/супругов")

par(mfrow = c(1, 1))

km <- kmeans(df_scaled, centers = 3, nstart = 25, iter.max = 20)

km_fare <- tapply(titanic_clust$Fare, km$cluster, mean)
km_order <- names(sort(km_fare))
km_old_to_new <- setNames(c(1, 2, 3), km_order)
km_cluster_ordered <- factor(km_old_to_new[as.character(km$cluster)], levels = c("1", "2", "3"))

p6 <- fviz_cluster(list(cluster = as.numeric(as.character(km_cluster_ordered)), 
                        data = df_scaled, 
                        centers = km$centers[as.numeric(km_order), ]), 
                   df_scaled, 
                   ellipse.type = "norm",
                   palette = c("#FC4E07", "#E7B800", "#2E9FDF"),
                   ggtheme = theme_minimal(),
                   main = "Кластеризация k-means (k = 3):\nбедные (красный), средние (жёлтый), богатые (синий)")
print(p6)

cat("\nСравнение иерархической кластеризации и k-means:\n")
print(table(Иерархическая = titanic_clust$cluster_label, 
            Kmeans = factor(km_old_to_new[as.character(km$cluster)], 
                            levels = c("1", "2", "3"),
                            labels = c("Бедные", "Средние", "Богатые"))))

pca <- prcomp(df_scaled, center = TRUE, scale. = FALSE)
scores <- as.data.frame(pca$x[, 1:4])

pairs(scores[, 1:4],
      col = c("#FC4E07", "#E7B800", "#2E9FDF")[as.numeric(as.character(km_cluster_ordered))],
      pch = as.numeric(titanic_clust$Pclass) + 14,
      cex = 0.8,
      main = "Попарные PCA-проекции",
      labels = c("PC1", "PC2", "PC3", "PC4"),
      lower.panel = NULL)

colors_3d <- c("#FC4E07", "#E7B800", "#2E9FDF")[as.numeric(as.character(km_cluster_ordered))]
pch_3d <- as.numeric(titanic_clust$Pclass) + 14
s3d <- scatterplot3d(scores$PC1, scores$PC2, scores$PC3,
                     color = colors_3d, pch = pch_3d,
                     xlab = "PC1", 
                     ylab = "PC2", 
                     zlab = "PC3",
                     main = "3D-визуализация (□ = 1 класс билета, ○ = 2 класс, △ = 3 класс)")
legend(s3d$xyz.convert(-2, -1, 3),
       c("Бедные", "Средние", "Богатые"),
       col = c("#FC4E07", "#E7B800", "#2E9FDF"), pch = 16, bty = "n")

cat("\nОбщая статистика по кластерам:\n")
cluster_stats <- titanic_clust %>%
  group_by(cluster_label) %>%
  summarise(
    `Кол-во пассажиров` = n(),
    `Средний возраст` = round(mean(Age), 1),
    `Средняя стоимость билета` = round(mean(Fare), 1),
    `Средний класс` = round(mean(as.numeric(Pclass)), 2),
    `% 1 класс` = round(mean(Pclass == 1) * 100, 1),
    `% 2 класс` = round(mean(Pclass == 2) * 100, 1),
    `% 3 класс` = round(mean(Pclass == 3) * 100, 1),
    `% выживших` = round(mean(Survived) * 100, 1)
  )
print(cluster_stats)

cat("\nКластеры упорядочены по возрастанию достатка:\n")
cat("  БЕДНЫЕ (красный): самая низкая стоимость билета, 3 класс, выживаемость 39%\n")
cat("  СРЕДНИЕ (жёлтый): средняя стоимость билета, 2-3 класс, выживаемость 33.7%\n")
cat("  БОГАТЫЕ (синий): высокая стоимость билета, 1 класс, выживаемость 55.9%\n\n")

final_df <- titanic_train %>%
  mutate(
    cluster = titanic_clust$cluster_label  # "Бедные", "Средние", "Богатые"
  )

head(final_df)
table(final_df$cluster)