library(ggplot2)
library(tidyr)
library(dplyr)
library(gridExtra)

setwd("~/Documents/kubsu-files/semester6/big-data/practice/lab2_data_preparing")
data <- readxl::read_excel("favourite_videogame.xlsx")

game_names <- colnames(data)[2:ncol(data)]

for (col in game_names) {
  data[[col]] <- as.numeric(data[[col]])
}

data_normalized <- data
data_normalized[, game_names] <- data[, game_names] / 10

str(data_normalized)
cat('\n')

stats_df <- data.frame(
  Game = game_names,
  Max = sapply(data_normalized[, game_names], max, na.rm = TRUE),
  Min = sapply(data_normalized[, game_names], min, na.rm = TRUE),
  Mean = sapply(data_normalized[, game_names], mean, na.rm = TRUE)
)

cat("Статистика по играм (max, min, mean):\n")
print(stats_df)
cat('\n')

count_above_07 <- sapply(data_normalized[, game_names], function(x) {
  sum(x > 0.7, na.rm = TRUE)
})

count_below_03 <- sapply(data_normalized[, game_names], function(x) {
  sum(x < 0.3, na.rm = TRUE)
})

preference_counts <- data.frame(
  Game = game_names,
  Count_Above_0.7 = count_above_07,
  Count_Below_0.3 = count_below_03
)

cat("Количество людей с предпочтениями >0.7 и <0.3:\n")
print(preference_counts)
cat('\n')

rating <- stats_df[order(stats_df$Mean, decreasing = TRUE), c("Game", "Mean")]

cat("Рейтинг игр по убыванию средних оценок:\n")
print(rating)
cat('\n')

cat("Работа с пропущенными данными:\n")

original <- sapply(data_normalized[, game_names], mean, na.rm = TRUE)

data_omit <- na.omit(data_normalized[, game_names])
omit <- sapply(data_omit, mean)

data_mean_fill <- data_normalized[, game_names]
for (col in game_names) {
  data_mean_fill[[col]][is.na(data_mean_fill[[col]])] <- mean(data_normalized[[col]], na.rm = TRUE)
}
mean_fill <- sapply(data_mean_fill, mean)

data_zero_fill <- data_normalized[, game_names]
for (col in game_names) {
  data_zero_fill[[col]][is.na(data_zero_fill[[col]])] <- 0
}
zero_fill <- sapply(data_zero_fill, mean)

comparison <- data.frame(
  Game = game_names,
  Original = round(original, 3),
  Delete_NA = round(omit, 3),
  Mean_Fill = round(mean_fill, 3),
  Zero_Fill = round(zero_fill, 3)
)

comparison$Diff_Delete <- round(comparison$Delete_NA - comparison$Original, 3)
comparison$Diff_Mean <- round(comparison$Mean_Fill - comparison$Original, 3)
comparison$Diff_Zero <- round(comparison$Zero_Fill - comparison$Original, 3)

print(comparison)
cat('\n')

cat("Выбор строк по признаку:\n")

selected_rows <- data_normalized[data_normalized$Minecraft > 0.9, 
                                 c("Отметка времени", game_names)]

cat("Строки с оценкой Minecraft > 0.9:\n")
print(selected_rows)

mean_values <- stats_df$Mean
names(mean_values) <- stats_df$Game

par(mar = c(10, 4, 4, 2))
barplot(mean_values, 
        main = "Средние оценки игр (базовая графика)",
        ylab = "Средняя оценка (нормализованная)",
        col = "skyblue",
        las = 2,
        cex.names = 0.8)
abline(h = 0.5, col = "red", lty = 2)

ggplot_data <- data_normalized %>%
  select(all_of(game_names)) %>%
  pivot_longer(cols = everything(), names_to = "Game", values_to = "Rating") %>%
  group_by(Game) %>%
  summarise(Mean_Rating = mean(Rating, na.rm = TRUE)) %>%
  ungroup()

p <- ggplot(ggplot_data, aes(x = reorder(Game, -Mean_Rating), y = Mean_Rating, fill = Game)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(title = "Средние оценки игр (ggplot2)",
       x = "Игра",
       y = "Средняя оценка (нормализованная)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "red")

print(p)

p_horizontal <- ggplot(ggplot_data, aes(x = Mean_Rating, y = reorder(Game, Mean_Rating), fill = Game)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Средние оценки игр (горизонтальная)",
       x = "Средняя оценка",
       y = "Игра") +
  theme_minimal() +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "red")

print(p_horizontal)

top5_games <- rating$Game[1:5]

boxplot_data <- data_normalized %>%
  select(all_of(top5_games)) %>%
  pivot_longer(cols = everything(), names_to = "Game", values_to = "Rating") %>%
  filter(!is.na(Rating))

p_boxplot <- ggplot(boxplot_data, aes(x = Game, y = Rating, fill = Game)) +
  geom_boxplot(show.legend = FALSE) +
  labs(title = "Распределение оценок топ-5 игр",
       x = "Игра",
       y = "Оценка (нормализованная)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p_boxplot)
