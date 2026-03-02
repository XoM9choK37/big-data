library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(moments)

setwd("/home/xom9chok37/Documents/kubsu-files/semester6/big-data/practice/lab2.2_descriptive_analysis")

cat("=== ИМПОРТ ДАННЫХ ===\n")
df_csv <- read.csv("favourite_videogame.csv", stringsAsFactors = FALSE, na.strings = c("NA", ""), fileEncoding = "UTF-8")
df_xlsx <- read_excel("favourite_videogame.xlsx", sheet = 1, col_names = TRUE, na = c("", "NA"))
cat("CSV:", dim(df_csv), "XLSX:", dim(df_xlsx), "\n")
df <- df_csv

df_num <- df[, -1]
total_rows <- nrow(df_num)

calc_stats <- function(x, col_name) {
  x_nona <- x[!is.na(x)]
  if (length(x_nona) == 0) return(NULL)
  data.frame(
    Игра = col_name,
    N = length(x_nona),
    Среднее = round(mean(x_nona), 2),
    Медиана = median(x_nona),
    Мин = min(x_nona),
    Макс = max(x_nona),
    Ст_откл = round(sd(x_nona), 2),
    Дисперсия = round(var(x_nona), 2),
    Асимметрия = round(skewness(x_nona), 2),
    Эксцесс = round(kurtosis(x_nona), 2),
    Пропущено = sum(is.na(x)),
    Пропущено_проц = round(100 * sum(is.na(x)) / total_rows, 1),
    stringsAsFactors = FALSE
  )
}

stats_list <- lapply(names(df_num), function(col) calc_stats(df_num[[col]], col))
stats_df <- do.call(rbind, stats_list)
rownames(stats_df) <- NULL

View(stats_df, "Статистика")
write.csv(stats_df, "stats.csv", row.names = FALSE)

games_to_plot <- c("Minecraft", "GTA", "Ведьмак", "Half.Life")
for (i in seq_along(games_to_plot)) {
  game <- games_to_plot[i]
  mean_val <- mean(df[[game]], na.rm = TRUE)
  median_val <- median(df[[game]], na.rm = TRUE)
  p <- ggplot(df, aes(x = .data[[game]])) +
    geom_histogram(aes(y = after_stat(density)), binwidth = 1,
                   fill = "skyblue", color = "black", na.rm = TRUE) +
    geom_density(color = "red", linewidth = 1, na.rm = TRUE) +
    geom_vline(xintercept = mean_val, color = "blue", linetype = "dashed", linewidth = 1) +
    geom_vline(xintercept = median_val, color = "green", linetype = "dotted", linewidth = 1) +
    labs(title = paste("Рис.", i, ". Гистограмма и плотность для", game),
         x = "Оценка", y = "Плотность") +
    theme_minimal()
  print(p)
}

cat("\n=== СОРТИРОВКА ===\n")
df_sorted <- df[order(df$Minecraft, decreasing = TRUE), ]
print(df_sorted[1:5, c("Отметка.времени", "Minecraft")])

cat("\n=== ПОДНАБОР ===\n")
subset_mc <- df[df$Minecraft > 7 & !is.na(df$Minecraft), ]
cat("Размер поднабора:", nrow(subset_mc), "x", ncol(subset_mc), "\n")

if (nrow(subset_mc) > 0) {
  sub_num <- subset_mc[, -1]
  print(summary(sub_num))
  
  gta_vals <- na.omit(subset_mc$GTA)
  if (length(unique(gta_vals)) > 1) {
    mean_gta <- mean(gta_vals)
    median_gta <- median(gta_vals)
    p5 <- ggplot(subset_mc, aes(x = GTA)) +
      geom_histogram(aes(y = after_stat(density)), binwidth = 1,
                     fill = "skyblue", color = "black", na.rm = TRUE) +
      geom_density(color = "red", linewidth = 1, na.rm = TRUE) +
      geom_vline(xintercept = mean_gta, color = "blue", linetype = "dashed", linewidth = 1) +
      geom_vline(xintercept = median_gta, color = "green", linetype = "dotted", linewidth = 1) +
      labs(title = "Рис.5. Гистограмма и плотность GTA (поднабор Minecraft > 7)",
           x = "Оценка GTA", y = "Плотность") +
      theme_minimal()
  } else {
    p5 <- ggplot(subset_mc, aes(x = GTA)) +
      geom_histogram(binwidth = 1, fill = "skyblue", color = "black", na.rm = TRUE) +
      labs(title = "Рис.5. Гистограмма GTA (поднабор Minecraft > 7)",
           x = "Оценка GTA", y = "Частота") +
      theme_minimal()
  }
  print(p5)
  
  cat("Рис.6. Ящики с усами для игр (поднабор Minecraft > 7)\n")
  boxplot(sub_num, main = "Рис.6. Ящики с усами для игр (поднабор Minecraft > 7)",
          las = 2, cex.axis = 0.7, col = "lightgray")
}

cat("\n=== ОПЕРАЦИИ ===\n")
df_csv$ID <- 1:nrow(df_csv)
df_xlsx$ID <- 1:nrow(df_xlsx)
df_merged <- merge(df_csv, df_xlsx, by = "ID", suffixes = c(".csv", ".xlsx"))
cat("Слияние по ID ->", dim(df_merged), "\n")
print(df_merged)
cat('\n')

new_row <- data.frame(
  Отметка.времени = "2026-02-17 10:00:00",
  Minecraft = 8, GTA = 9, Mafia = 8, Far.Cry = 7, Need.for.Speed = 6,
  Portal = 9, Detroit..Become.Human = 8, Cyberpunk = 7, Ведьмак = 10,
  L.A..Noire = 5, Half.Life = 8, Frostpunk = 6, FNAF = 4,
  Team.Fortress = 7, Герои = 9, ID = 13,
  stringsAsFactors = FALSE
)
df_csv_aug <- rbind(df_csv, new_row)
cat("Добавлена строка, теперь строк:", nrow(df_csv_aug), "\n")
print(df_csv_aug)
cat('\n')

df_csv_no_fnaf <- select(df_csv, -FNAF)
cat("Удалён FNAF, осталось столбцов:", ncol(df_csv_no_fnaf), "\n")
print(df_csv_no_fnaf)
cat('\n')

subset_hl <- subset(df_csv, Half.Life > 5 & !is.na(Half.Life))
cat("Half-Life > 5:", nrow(subset_hl), "строк\n")
print(subset_hl)
cat('\n')

df_external <- read.csv("favourite_videogame.csv")
cat("Повторная загрузка CSV, размер:", dim(df_external), "\n")
print(df_external)

plot_stat_bars <- function(df_stat, stat_col, title, y_label) {
  df_plot <- df_stat %>%
    select(Игра, value = all_of(stat_col)) %>%
    arrange(desc(value)) %>%
    mutate(Игра = factor(Игра, levels = Игра))
  
  ggplot(df_plot, aes(x = Игра, y = value)) +
    geom_col(fill = "steelblue") +
    labs(title = title, x = "Игра", y = y_label) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

p_sd <- plot_stat_bars(stats_df, "Ст_откл", 
                       "Рис.7. Стандартное отклонение оценок по играм (убывание)", 
                       "Стандартное отклонение")
print(p_sd)

p_skew <- plot_stat_bars(stats_df, "Асимметрия", 
                         "Рис.8. Асимметрия распределений оценок (убывание)", 
                         "Коэффициент асимметрии")
print(p_skew)

p_kurt <- plot_stat_bars(stats_df, "Эксцесс", 
                         "Рис.9. Эксцесс распределений оценок (убывание)", 
                         "Коэффициент эксцесса")
print(p_kurt)