library(ggplot2)
library(dplyr)
library(gridExtra)
library(car)
library(e1071)

data_raw <- NULL
tryCatch({
  data_raw <- read.csv("athlete_events.csv", 
                       header = TRUE, 
                       stringsAsFactors = FALSE,
                       fileEncoding = "UTF-8",
                       na.strings = c("", "NA", "N/A"),
                       quote = "\"",
                       comment.char = "")
}, error = function(e) {
  cat("Ошибка при загрузке с UTF-8:", e$message, "\n")
})

if (is.null(data_raw)) {
  tryCatch({
    data_raw <- read.csv("athlete_events.csv", 
                         header = TRUE, 
                         stringsAsFactors = FALSE,
                         na.strings = c("", "NA", "N/A"),
                         quote = "\"",
                         comment.char = "",
                         encoding = "latin1")
  }, error = function(e) {
    cat("Ошибка при загрузке с latin1:", e$message, "\n")
  })
}

if (is.null(data_raw)) {
  data_raw <- read.csv("athlete_events.csv", 
                       header = TRUE, 
                       stringsAsFactors = FALSE,
                       na.strings = c("", "NA", "N/A"),
                       quote = "",
                       comment.char = "")
}

cat("ПРОВЕРКА СТАТИСТИЧЕСКИХ ГИПОТЕЗ: ВЕС ФУТБОЛИСТОВ И ВЕС БАСКЕТБОЛИСТОВ\n")

cat("\n\nПОДГОТОВКА ДАННЫХ\n\n")
cat(sprintf("Исходное количество записей: %d\n", nrow(data_raw)))

cat("Столбцы в данных:", paste(names(data_raw), collapse = ", "), "\n")

if ("Weight" %in% names(data_raw)) {
  data_raw$Weight <- suppressWarnings(as.numeric(as.character(data_raw$Weight)))
} else {
  stop("Столбец 'Weight' не найден в данных!")
}

if ("Height" %in% names(data_raw)) {
  data_raw$Height <- suppressWarnings(as.numeric(as.character(data_raw$Height)))
}

if ("Age" %in% names(data_raw)) {
  data_raw$Age <- suppressWarnings(as.numeric(as.character(data_raw$Age)))
}

if ("Year" %in% names(data_raw)) {
  data_raw$Year <- suppressWarnings(as.numeric(as.character(data_raw$Year)))
}

data_clean <- data_raw %>%
  filter(!is.na(Weight) & is.finite(Weight) & Weight > 0 & Weight < 300)

cat(sprintf("После удаления NA: %d\n", nrow(data_clean)))

required_cols <- c("ID", "Games", "Sport", "Sex", "Weight")
missing_cols <- setdiff(required_cols, names(data_clean))
if (length(missing_cols) > 0) {
  stop("Отсутствуют необходимые столбцы: ", paste(missing_cols, collapse = ", "))
}

data_unique <- data_clean %>%
  group_by(ID, Games) %>%
  slice(1) %>%
  ungroup()

cat(sprintf("После удаления дубликатов: %d\n", nrow(data_unique)))

football <- subset(data_unique, Sport == "Football")
basketball <- subset(data_unique, Sport == "Basketball")

cat(sprintf("Записей по футболу: %d\n", nrow(football)))
cat(sprintf("Записей по баскетболу: %d\n", nrow(basketball)))

fb_M <- as.numeric(football$Weight[football$Sex == "M"])
fb_F <- as.numeric(football$Weight[football$Sex == "F"])
bb_M <- as.numeric(basketball$Weight[basketball$Sex == "M"])
bb_F <- as.numeric(basketball$Weight[basketball$Sex == "F"])

fb_M <- fb_M[is.finite(fb_M)]
fb_F <- fb_F[is.finite(fb_F)]
bb_M <- bb_M[is.finite(bb_M)]
bb_F <- bb_F[is.finite(bb_F)]

cat("\nРазмеры выборок (выступления спортсменов с известным весом):\n")
cat(sprintf("  Футбол     — мужчины: %d,  женщины: %d\n", length(fb_M), length(fb_F)))
cat(sprintf("  Баскетбол  — мужчины: %d,  женщины: %d\n", length(bb_M), length(bb_F)))

if (length(fb_M) < 3 | length(fb_F) < 3 | length(bb_M) < 3 | length(bb_F) < 3) {
  stop("Одна из выборок слишком мала для статистического анализа (нужно минимум 3 наблюдения)")
}

n_fb_total <- nrow(football)
n_bb_total <- nrow(basketball)

cat("\n\nДЕСКРИПТИВНЫЙ АНАЛИЗ\n")

describe <- function(x, name) {
  if (length(x) == 0 || !is.numeric(x)) {
    cat(sprintf("\n%s: НЕТ ДАННЫХ\n", name))
    return(invisible(NULL))
  }
  
  x <- x[is.finite(x)]
  
  if (length(x) == 0) {
    cat(sprintf("\n%s: НЕТ КОРРЕКТНЫХ ДАННЫХ\n", name))
    return(invisible(NULL))
  }
  
  n <- length(x)
  m <- mean(x, na.rm = TRUE)
  med <- median(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE)
  v <- var(x, na.rm = TRUE)
  mn <- min(x, na.rm = TRUE)
  mx <- max(x, na.rm = TRUE)
  q <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
  iqr_val <- q[2] - q[1]
  lower <- q[1] - 1.5 * iqr_val
  upper <- q[2] + 1.5 * iqr_val
  out <- x[x < lower | x > upper]
  
  sk <- tryCatch(if(n >= 3) skewness(x, na.rm = TRUE) else NA, error = function(e) NA)
  kt <- tryCatch(if(n >= 4) kurtosis(x, na.rm = TRUE) else NA, error = function(e) NA)
  
  cat(sprintf("\n%s (n = %d)\n", name, n))
  cat(sprintf("  Среднее:   %8.2f кг      Медиана:    %8.2f кг\n", m, med))
  cat(sprintf("  Стд.откл:  %8.2f кг      Дисперсия:  %8.2f кг²\n", s, v))
  cat(sprintf("  Мин:       %8.0f кг      Макс:       %8.0f кг\n", mn, mx))
  cat(sprintf("  Q1:        %8.1f кг      Q3:         %8.1f кг      IQR: %8.1f кг\n", 
              q[1], q[2], iqr_val))
  if(!is.na(sk)) cat(sprintf("  Асимметрия: %7.3f        Эксцесс:    %8.3f\n", sk, kt))
  cat(sprintf("  Выбросов (IQR-метод): %d  (границы: [%.1f, %.1f] кг)\n", 
              length(out), lower, upper))
  if (length(out) > 0 && length(out) <= 20) {
    cat(sprintf("  Значения выбросов: %s\n", paste(sort(round(out, 1)), collapse = ", ")))
  } else if (length(out) > 20) {
    cat(sprintf("  Значения выбросов (первые 20): %s ...\n", 
                paste(sort(round(out, 1))[1:20], collapse = ", ")))
  }
  
  invisible(list(mean = m, median = med, sd = s, var = v, min = mn, max = mx,
                 q1 = q[1], q3 = q[2], iqr = iqr_val, 
                 skewness = sk, kurtosis = kt,
                 n_outliers = length(out)))
}

desc_fb_M <- describe(fb_M, "Футбол, мужчины")
desc_fb_F <- describe(fb_F, "Футбол, женщины")
desc_bb_M <- describe(bb_M, "Баскетбол, мужчины")
desc_bb_F <- describe(bb_F, "Баскетбол, женщины")

col_fb_M <- "#1B9E77"
col_fb_F <- "#66C2A5"
col_bb_M <- "#D95F02"
col_bb_F <- "#FC8D62"

df_fb_M <- data.frame(Weight = as.numeric(fb_M))
df_fb_F <- data.frame(Weight = as.numeric(fb_F))
df_bb_M <- data.frame(Weight = as.numeric(bb_M))
df_bb_F <- data.frame(Weight = as.numeric(bb_F))

df_fb_M <- df_fb_M[is.finite(df_fb_M$Weight), , drop = FALSE]
df_fb_F <- df_fb_F[is.finite(df_fb_F$Weight), , drop = FALSE]
df_bb_M <- df_bb_M[is.finite(df_bb_M$Weight), , drop = FALSE]
df_bb_F <- df_bb_F[is.finite(df_bb_F$Weight), , drop = FALSE]

xlim_M <- range(c(df_fb_M$Weight, df_bb_M$Weight), na.rm = TRUE)
xlim_F <- range(c(df_fb_F$Weight, df_bb_F$Weight), na.rm = TRUE)

p1 <- ggplot(df_fb_M, aes(x = Weight)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = col_fb_M, alpha = 0.5, color = "white") +
  geom_density(color = col_fb_M, linewidth = 1.1) +
  labs(title = paste0("Футбол: мужчины (n=", nrow(df_fb_M), ")"), 
       x = "Вес (кг)", y = "Плотность") +
  xlim(xlim_M) +
  theme_minimal(base_size = 11)

p2 <- ggplot(df_fb_F, aes(x = Weight)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = col_fb_F, alpha = 0.5, color = "white") +
  geom_density(color = col_fb_F, linewidth = 1.1) +
  labs(title = paste0("Футбол: женщины (n=", nrow(df_fb_F), ")"), 
       x = "Вес (кг)", y = "Плотность") +
  xlim(xlim_F) +
  theme_minimal(base_size = 11)

p3 <- ggplot(df_bb_M, aes(x = Weight)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = col_bb_M, alpha = 0.5, color = "white") +
  geom_density(color = col_bb_M, linewidth = 1.1) +
  labs(title = paste0("Баскетбол: мужчины (n=", nrow(df_bb_M), ")"), 
       x = "Вес (кг)", y = "Плотность") +
  xlim(xlim_M) +
  theme_minimal(base_size = 11)

p4 <- ggplot(df_bb_F, aes(x = Weight)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30,
                 fill = col_bb_F, alpha = 0.5, color = "white") +
  geom_density(color = col_bb_F, linewidth = 1.1) +
  labs(title = paste0("Баскетбол: женщины (n=", nrow(df_bb_F), ")"), 
       x = "Вес (кг)", y = "Плотность") +
  xlim(xlim_F) +
  theme_minimal(base_size = 11)

plot_data_M <- rbind(
  data.frame(Weight = fb_M, Sport = paste0("Футбол\n(n=", length(fb_M), ")"), 
             stringsAsFactors = FALSE),
  data.frame(Weight = bb_M, Sport = paste0("Баскетбол\n(n=", length(bb_M), ")"), 
             stringsAsFactors = FALSE)
)

plot_data_F <- rbind(
  data.frame(Weight = fb_F, Sport = paste0("Футбол\n(n=", length(fb_F), ")"), 
             stringsAsFactors = FALSE),
  data.frame(Weight = bb_F, Sport = paste0("Баскетбол\n(n=", length(bb_F), ")"), 
             stringsAsFactors = FALSE)
)

p5 <- ggplot(plot_data_M, aes(x = Sport, y = Weight)) +
  geom_boxplot(alpha = 0.6, fill = "lightblue", outlier.color = "red", outlier.shape = 1) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, 
               fill = "yellow", color = "black") +
  labs(title = "Сравнение веса: мужчины", x = "", y = "Вес (кг)") +
  theme_minimal(base_size = 12)

p6 <- ggplot(plot_data_F, aes(x = Sport, y = Weight)) +
  geom_boxplot(alpha = 0.6, fill = "lightpink", outlier.color = "red", outlier.shape = 1) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, 
               fill = "yellow", color = "black") +
  labs(title = "Сравнение веса: женщины", x = "", y = "Вес (кг)") +
  theme_minimal(base_size = 12)

grid.arrange(p1, p2, p3, p4, p5, p6, ncol = 3,
             layout_matrix = rbind(c(1, 2, 5), c(3, 4, 6)))

cat("\n\nПРОВЕРКА НОРМАЛЬНОСТИ И РАВЕНСТВА ДИСПЕРСИЙ\n")

cat("\nТест Шапиро-Уилка на нормальность:\n\n")

shapiro_test <- function(x, label, alpha = 0.05) {
  if (!is.numeric(x)) {
    cat(sprintf("%s: ОШИБКА: данные не числовые\n", label))
    return(NA)
  }
  
  x <- x[is.finite(x)]
  n <- length(x)
  
  if (n < 3) {
    cat(sprintf("%s: НЕДОСТАТОЧНО ДАННЫХ (n = %d)\n", label, n))
    return(NA)
  }
  
  if (n > 5000) {
    cat(sprintf("%s: n = %d (> 5000), используется ЦПТ\n", label, n))
    return(TRUE)
  }
  
  test <- tryCatch(
    shapiro.test(x),
    error = function(e) {
      cat(sprintf("%s: ОШИБКА теста - %s\n", label, e$message))
      return(NULL)
    }
  )
  
  if (is.null(test)) return(NA)
  
  cat(sprintf("%s: n = %d, W = %.4f, p-value = %s\n", 
              label, n, test$statistic, 
              format(test$p.value, digits = 4, scientific = TRUE)))
  
  if (test$p.value > alpha) {
    cat("      Распределение можно считать нормальным (p > 0.05).\n\n")
    return(TRUE)
  } else {
    cat("      Распределение отличается от нормального (p < 0.05).\n")
    if (n > 30) {
      cat("      НО: при n > 30 полагаемся на ЦПТ для t-теста.\n\n")
      return(TRUE)
    }
    cat("\n")
    return(FALSE)
  }
}

norm_fb_M <- shapiro_test(fb_M, "Футбол, мужчины")
norm_fb_F <- shapiro_test(fb_F, "Футбол, женщины")
norm_bb_M <- shapiro_test(bb_M, "Баскетбол, мужчины")
norm_bb_F <- shapiro_test(bb_F, "Баскетбол, женщины")

par(mfrow = c(2, 2))

safe_qqplot <- function(x, title) {
  if (length(x) > 0 && is.numeric(x)) {
    tryCatch({
      qqPlot(x, main = title,
             xlab = "Теоретические квантили", ylab = "Вес (кг)",
             col.lines = "red", id = FALSE)
    }, error = function(e) {
      qqnorm(x, main = title)
      qqline(x, col = "red")
    })
  } else {
    plot(0, 0, type = "n", main = paste(title, "- НЕТ ДАННЫХ"))
    text(0, 0, "Нет данных")
  }
}

safe_qqplot(fb_M, paste0("Футбол, мужчины (n=", length(fb_M), ")"))
safe_qqplot(fb_F, paste0("Футбол, женщины (n=", length(fb_F), ")"))
safe_qqplot(bb_M, paste0("Баскетбол, мужчины (n=", length(bb_M), ")"))
safe_qqplot(bb_F, paste0("Баскетбол, женщины (n=", length(bb_F), ")"))

par(mfrow = c(1, 1))

cat("Проверка равенства дисперсий:\n")

safe_levene_test <- function(x1, x2, label1, label2, group_name) {
  cat(sprintf("\n%s (%s и %s):\n", group_name, label1, label2))
  
  df <- data.frame(
    Weight = c(x1, x2),
    Sport = factor(c(rep(label1, length(x1)), rep(label2, length(x2))))
  )
  
  result <- tryCatch({
    leveneTest(Weight ~ Sport, data = df, center = median)
  }, error = function(e) {
    cat("  Тест Левена не удался:", e$message, "\n")
    return(NULL)
  })
  
  if (!is.null(result)) {
    cat(sprintf("  Тест Левена: F = %.4f, df1 = %d, df2 = %d, p-value = %s\n", 
                result$`F value`[1], result$Df[1], result$Df[2],
                format(result$`Pr(>F)`[1], digits = 4, scientific = TRUE)))
    var_equal <- result$`Pr(>F)`[1] > 0.05
  } else {
    var_test <- var.test(x1, x2)
    cat(sprintf("  Тест Фишера: F = %.4f, p-value = %s\n", 
                var_test$statistic, format(var_test$p.value, digits = 4)))
    var_equal <- var_test$p.value > 0.05
  }
  
  cat(if(var_equal) "    Дисперсии статистически РАВНЫ\n" else 
    "    Дисперсии статистически РАЗЛИЧАЮТСЯ\n")
  
  return(var_equal)
}

var_equal_M <- safe_levene_test(fb_M, bb_M, "Футбол", "Баскетбол", "Мужчины")
var_equal_F <- safe_levene_test(fb_F, bb_F, "Футбол", "Баскетбол", "Женщины")

cat("\n\nОДНОВЫБОРОЧНЫЙ КРИТЕРИЙ: СРЕДНИЙ ВЕС ФУТБОЛИСТОВ\n")

all_weight_mean <- mean(data_unique$Weight, na.rm = TRUE)
all_weight_median <- median(data_unique$Weight, na.rm = TRUE)
all_weight_sd <- sd(data_unique$Weight, na.rm = TRUE)
all_weight_n <- sum(!is.na(data_unique$Weight))

cat(sprintf("\nЭталонное значение (mu_0) — средний вес ВСЕХ олимпийцев: %.2f кг\n", 
            all_weight_mean))
cat(sprintf("(n = %d, SD = %.2f кг, медиана = %.2f кг)\n", 
            all_weight_n, all_weight_sd, all_weight_median))

one_sample_test <- function(x, mu, label, is_normal, alpha = 0.05) {
  n <- length(x)
  x_mean <- mean(x)
  x_median <- median(x)
  
  cat(sprintf("\n %s \n", label))
  cat(sprintf("  H₀: μ = %.1f кг\n", mu))
  cat(sprintf("  H₁: μ ≠ %.1f кг\n", mu))
  cat(sprintf("  Выборочное среднее: %.2f кг, медиана: %.2f кг, n = %d\n", 
              x_mean, x_median, n))
  
  if (is_normal || n >= 30) {
    cat("  Метод: одновыборочный t-тест Стьюдента\n")
    test <- t.test(x, mu = mu, conf.int = TRUE)
  } else {
    cat("  Метод: тест Уилкоксона (ненормальное распределение, n < 30)\n")
    test <- wilcox.test(x, mu = mu, conf.int = TRUE)
  }
  
  cat(sprintf("  Статистика = %.4f", test$statistic))
  if (!is.null(test$parameter)) cat(sprintf(", df = %.1f", test$parameter))
  cat(sprintf("\n  p-value = %s\n", format(test$p.value, digits = 4, scientific = TRUE)))
  
  if (!is.null(test$conf.int)) {
    cat(sprintf("  95%% доверительный интервал: [%.2f, %.2f] кг\n", 
                test$conf.int[1], test$conf.int[2]))
  }
  
  if (test$p.value < alpha) {
    cat(sprintf("    H₀ ОТВЕРГАЕТСЯ: вес значимо отличается от %.1f кг\n", mu))
    if (x_mean > mu) {
      cat(sprintf("    Средний вес ВЫШЕ эталона на %.1f кг\n", x_mean - mu))
    } else {
      cat(sprintf("    Средний вес НИЖЕ эталона на %.1f кг\n", mu - x_mean))
    }
    return(FALSE)
  } else {
    cat(sprintf("    H₀ НЕ ОТВЕРГАЕТСЯ: вес не отличается от %.1f кг\n", mu))
    return(TRUE)
  }
}

if (length(fb_M) > 0) {
  one_samp_M_result <- one_sample_test(fb_M, all_weight_mean, "Футбол, мужчины", norm_fb_M)
} else {
  cat("\nФутбол, мужчины: НЕТ ДАННЫХ\n")
  one_samp_M_result <- NA
}

if (length(fb_F) > 0) {
  one_samp_F_result <- one_sample_test(fb_F, all_weight_mean, "Футбол, женщины", norm_fb_F)
} else {
  cat("\nФутбол, женщины: НЕТ ДАННЫХ\n")
  one_samp_F_result <- NA
}

cat("\n\nДВУХВЫБОРОЧНЫЙ КРИТЕРИЙ: ФУТБОЛ И БАСКЕТБОЛ\n")

two_sample_test <- function(x1, x2, label1, label2,
                            is_normal1, is_normal2, var_equal, alpha = 0.05) {
  n1 <- length(x1)
  n2 <- length(x2)
  mean1 <- mean(x1)
  mean2 <- mean(x2)
  
  cat(sprintf("\n%s и %s\n", label1, label2))
  cat("  H₀: μ₁ = μ₂  (средние веса равны)\n")
  cat("  H₁: μ₁ ≠ μ₂  (средние веса различаются)\n")
  cat(sprintf("  %s: среднее = %.2f кг, n = %d\n", label1, mean1, n1))
  cat(sprintf("  %s: среднее = %.2f кг, n = %d\n", label2, mean2, n2))
  cat(sprintf("  Разница средних: %.2f кг\n", mean1 - mean2))
  
  both_normal <- is_normal1 && is_normal2
  large_samples <- n1 >= 30 && n2 >= 30
  
  if ((both_normal || large_samples)) {
    if (var_equal) {
      cat("  Метод: t-тест Стьюдента (дисперсии равны)\n")
      test <- t.test(x1, x2, var.equal = TRUE)
    } else {
      cat("  Метод: t-тест Уэлча (дисперсии не равны)\n")
      test <- t.test(x1, x2, var.equal = FALSE)
    }
  } else {
    cat("  Метод: тест Манна-Уитни (ненормальные распределения)\n")
    test <- wilcox.test(x1, x2, conf.int = TRUE)
  }
  
  cat(sprintf("  Статистика = %.4f", test$statistic))
  if (!is.null(test$parameter)) cat(sprintf(", df = %.1f", test$parameter))
  cat(sprintf("\n  p-value = %s\n", format(test$p.value, digits = 4, scientific = TRUE)))
  
  if (!is.null(test$conf.int)) {
    cat(sprintf("  95%% ДИ для разницы: [%.2f, %.2f] кг\n", 
                test$conf.int[1], test$conf.int[2]))
  }
  
  pooled_sd <- sqrt(((n1-1)*var(x1) + (n2-1)*var(x2)) / (n1+n2-2))
  cohens_d <- abs(mean1 - mean2) / pooled_sd
  cat(sprintf("  Cohen's d = %.3f", cohens_d))
  if (cohens_d < 0.2) cat(" (пренебрежимо малый)")
  else if (cohens_d < 0.5) cat(" (малый)")
  else if (cohens_d < 0.8) cat(" (средний)")
  else cat(" (большой)")
  cat("\n")
  
  if (test$p.value < alpha) {
    cat("    H₀ ОТВЕРГАЕТСЯ: веса статистически значимо РАЗЛИЧАЮТСЯ.\n")
    if (mean1 > mean2) {
      cat(sprintf("    «%s» тяжелее на %.1f кг\n", label1, mean1 - mean2))
    } else {
      cat(sprintf("    «%s» тяжелее на %.1f кг\n", label2, mean2 - mean1))
    }
  } else {
    cat("    H₀ НЕ ОТВЕРГАЕТСЯ: нет значимых различий.\n")
  }
  
  return(list(p_value = test$p.value, cohens_d = cohens_d))
}

if (length(fb_M) > 0 && length(bb_M) > 0) {
  result_M <- two_sample_test(fb_M, bb_M, "Футбол (М)", "Баскетбол (М)",
                              norm_fb_M, norm_bb_M, var_equal_M)
  p_val_M <- result_M$p_value
  d_M <- result_M$cohens_d
} else {
  cat("\nМужчины: недостаточно данных для сравнения\n")
  p_val_M <- NA
  d_M <- NA
}

if (length(fb_F) > 0 && length(bb_F) > 0) {
  result_F <- two_sample_test(fb_F, bb_F, "Футбол (Ж)", "Баскетбол (Ж)",
                              norm_fb_F, norm_bb_F, var_equal_F)
  p_val_F <- result_F$p_value
  d_F <- result_F$cohens_d
} else {
  cat("\nЖенщины: недостаточно данных для сравнения\n")
  p_val_F <- NA
  d_F <- NA
}

cat("\n\nЗАКЛЮЧЕНИЕ\n")

cat("\nХарактеристика выборки:\n")
cat(sprintf("     Проанализированы данные о %d выступлениях футболистов\n", n_fb_total))
cat(sprintf("     и %d выступлениях баскетболистов\n", n_bb_total))

if ("Year" %in% names(data_unique) && is.numeric(data_unique$Year)) {
  years <- data_unique$Year[is.finite(data_unique$Year)]
  if (length(years) > 0) {
    cat(sprintf("     Период: %d-%d гг.\n", min(years), max(years)))
  }
}
cat(sprintf("     Всего Олимпийских игр в выборке: %d\n", 
            length(unique(data_unique$Games))))
cat("\n")

cat("Проверка нормальности (тест Шапиро-Уилка, α = 0.05):\n")
if (!is.na(norm_fb_M)) {
  cat(sprintf("     Футбол, мужчины (n=%d): %s\n", length(fb_M),
              if(norm_fb_M) "нормальное/ЦПТ" else "отличается от нормального"))
}
if (!is.na(norm_fb_F)) {
  cat(sprintf("     Футбол, женщины (n=%d): %s\n", length(fb_F),
              if(norm_fb_F) "нормальное/ЦПТ" else "отличается от нормального"))
}
if (!is.na(norm_bb_M)) {
  cat(sprintf("     Баскетбол, мужчины (n=%d): %s\n", length(bb_M),
              if(norm_bb_M) "нормальное/ЦПТ" else "отличается от нормального"))
}
if (!is.na(norm_bb_F)) {
  cat(sprintf("     Баскетбол, женщины (n=%d): %s\n", length(bb_F),
              if(norm_bb_F) "нормальное/ЦПТ" else "отличается от нормального"))
}
cat("\n")

cat("Проверка равенства дисперсий (тест Левена, α = 0.05):\n")
cat(sprintf("     Мужчины: дисперсии %s\n", if(var_equal_M) "РАВНЫ" else "РАЗЛИЧАЮТСЯ"))
cat(sprintf("     Женщины: дисперсии %s\n", if(var_equal_F) "РАВНЫ" else "РАЗЛИЧАЮТСЯ"))
cat("\n")

cat("Одновыборочный критерий:\n")
cat(sprintf("   Эталон: %.2f кг (средний вес всех олимпийцев, n=%d)\n", 
            all_weight_mean, all_weight_n))
if (!is.na(one_samp_M_result) && length(fb_M) > 0) {
  cat(sprintf("     Мужчины-футболисты (%.1f кг): H₀ %s\n",
              mean(fb_M),
              if(!one_samp_M_result) "ОТВЕРГНУТА" else "НЕ ОТВЕРГНУТА"))
}
if (!is.na(one_samp_F_result) && length(fb_F) > 0) {
  cat(sprintf("     Женщины-футболистки (%.1f кг): H₀ %s\n",
              mean(fb_F),
              if(!one_samp_F_result) "ОТВЕРГНУТА" else "НЕ ОТВЕРГНУТА"))
}
cat("\n")

cat("Двухвыборочный критерий:\n")
if (!is.na(p_val_M)) {
  cat(sprintf("     Мужчины: различия %s (p = %s, d = %.3f)\n",
              if(p_val_M < 0.05) "ЗНАЧИМЫ" else "НЕ ЗНАЧИМЫ",
              format(p_val_M, digits = 4, scientific = TRUE), d_M))
  cat(sprintf("     Футбол: %.1f кг, Баскетбол: %.1f кг\n", mean(fb_M), mean(bb_M)))
}
if (!is.na(p_val_F)) {
  cat(sprintf("     Женщины: различия %s (p = %s, d = %.3f)\n",
              if(p_val_F < 0.05) "ЗНАЧИМЫ" else "НЕ ЗНАЧИМЫ",
              format(p_val_F, digits = 4, scientific = TRUE), d_F))
  cat(sprintf("     Футбол: %.1f кг, Баскетбол: %.1f кг\n", mean(fb_F), mean(bb_F)))
}
cat("\n")

cat("Уровень значимости α = 0.05\n")
cat("Нормальность: тест Шапиро-Уилка + QQ-plot\n")
cat("Гомогенность дисперсий: тест Левена\n")
cat("Одновыборочный тест: t-тест или Уилкоксон\n")
cat("Двухвыборочный тест: t-тест/Уэлч или Манна-Уитни\n")
cat("Размер эффекта: Cohen's d\n")