library(learningtower)
library(ggplot2)

# student_data <- load_student("all")

countries <- c("AUT", "JPN", "CHN", "SVK", "HUN", "RUS", "ROU")
subset_stud <- subset(student_data, country %in% countries)
subset_stud <- subset_stud[!is.na(subset_stud$math) & 
                             !is.na(subset_stud$read) & 
                             !is.na(subset_stud$science), ]

cat("ФРАГМЕНТ ИСХОДНОЙ ТАБЛИЦЫ (первые 10 строк)\n")
print(head(subset_stud[, c("year", "country", "math", "read", "science", 
                           "gender", "computer_n")], 10))
cat('\n')



cat("Динамика средних оценок (7 стран)\n")
avg_scores <- aggregate(cbind(math, read, science) ~ year,
                        data = subset_stud, mean, na.rm = TRUE)
print(round(avg_scores, 2))
cat('\n')

par(mfrow = c(1, 1), mar = c(5, 4, 4, 6))
plot(avg_scores$year, avg_scores$math, type = "b", col = "blue", 
     xlab = "Год", ylab = "Средняя оценка", 
     main = "Динамика средних оценок PISA (7 стран)",
     ylim = c(400, 600), cex.main = 1.5, cex.lab = 1.2)
lines(avg_scores$year, avg_scores$read, type = "b", col = "green")
lines(avg_scores$year, avg_scores$science, type = "b", col = "red")
legend("topright", legend = c("Математика", "Чтение", "Естествознание"), 
       col = c("blue", "green", "red"), lty = 1, pch = 19, 
       cex = 0.6, bty = "n")



cat("Динамика средних оценок в Японии\n")
avg_scores <- aggregate(cbind(math, read, science) ~ year, 
                        data = subset(subset_stud, country == "JPN"),
                        mean, na.rm = TRUE)
print(round(avg_scores, 2))
cat('\n')

par(mfrow = c(1, 1), mar = c(5, 4, 4, 6))
plot(avg_scores$year, avg_scores$math, type = "b", col = "blue", 
     xlab = "Год", ylab = "Средняя оценка", 
     main = "Динамика средних оценок в Японии",
     ylim = c(400, 600), cex.main = 1.5, cex.lab = 1.2)
lines(avg_scores$year, avg_scores$read, type = "b", col = "green")
lines(avg_scores$year, avg_scores$science, type = "b", col = "red")
legend("topright", legend = c("Математика", "Чтение", "Естествознание"), 
       col = c("blue", "green", "red"), lty = 1, pch = 19, 
       cex = 0.6, bty = "n")



last_year <- max(subset_stud$year)
jpn_last <- subset(subset_stud, country == "JPN" & year == last_year)
bar_data <- c(mean(jpn_last$math, na.rm = TRUE),
              mean(jpn_last$read, na.rm = TRUE),
              mean(jpn_last$science, na.rm = TRUE))

cat("Средние оценки в Японии за год\n")
print(cbind(c("math", "read", "science"), round(bar_data, 2)))
cat('\n')

barplot(bar_data, names.arg = c("Математика", "Чтение", "Естествознание"),
        col = c("blue", "green", "red"), 
        main = paste("Средние оценки в Японии,", last_year, "год"),
        ylab = "Средняя оценка", cex.main = 1.5, ylim = c(400, 650))



male   <- subset(subset_stud, gender == "male")
female <- subset(subset_stud, gender == "female")

means_m <- c(mean(male$math, na.rm = TRUE), 
             mean(male$read, na.rm = TRUE), 
             mean(male$science, na.rm = TRUE))
means_f <- c(mean(female$math, na.rm = TRUE), 
             mean(female$read, na.rm = TRUE), 
             mean(female$science, na.rm = TRUE))

means_m = round(means_m, 2)
means_f = round(means_f, 2)
pie_data <- c(means_m, means_f)
labels_pie <- c("Математика", "Чтение", "Естествознание")

mf_df <- data.frame(row.names = labels_pie)
mf_df <- data.frame(mf_df, "М" = means_m, "Ж" = means_f)
print(mf_df)
cat('\n')

labels_means_m_num <- as.character(round(means_m, 2))
labels_means_m_num <- paste(labels_means_m_num, "баллов")
labels_means_m_perc <- as.character(round(means_m / sum(means_m), 4) * 100)
labels_means_m_perc <- paste(labels_means_m_perc, "%", sep = "")
labels_means_m <- paste(labels_means_m_num, labels_means_m_perc, sep = " | ")
par(mfrow = c(1, 1), xpd = TRUE)
pie(means_m, labels = labels_means_m, col = rainbow(6), 
    main = "Средние оценки среди мужчин", cex.main = 1.5, cex = 1.1)
legend("bottomright", legend = labels_pie, fill = rainbow(6), 
       cex = 1.0, bty = "n", inset = c(-0.25, 0))



labels_means_f_num <- as.character(round(means_f, 2))
labels_means_f_num <- paste(labels_means_f_num, "баллов")
labels_means_f_perc <- as.character(round(means_f / sum(means_f), 4) * 100)
labels_means_f_perc <- paste(labels_means_f_perc, "%", sep = "")
labels_means_f <- paste(labels_means_f_num, labels_means_f_perc, sep = " | ")
par(mfrow = c(1, 1), xpd = TRUE)
pie(means_f, labels = labels_means_f, col = rainbow(6), 
    main = "Средние оценки среди женщин", cex.main = 1.5, cex = 1.1)
legend("bottomright", legend = labels_pie, fill = rainbow(6), 
       cex = 1.0, bty = "n", inset = c(-0.25, 0))



par(mfrow = c(1, 2), mar = c(3, 4, 4, 2), xpd = FALSE)
hist(male$math, breaks = 30, col = "lightblue", 
     main = "Математика: Мужчины", xlab = "Оценка", cex.main = 1.3)
hist(female$math, breaks = 30, col = "lightpink", 
     main = "Математика: Женщины", xlab = "Оценка", cex.main = 1.3)



par(mfrow = c(1, 1), xpd = FALSE)
plot(0, 0, type = "n", xlim = range(subset_stud$year), 
     ylim = c(350, 650), 
     xlab = "Год", ylab = "Средняя оценка по математике",
     main = "Динамика математики по 7 странам", cex.main = 1.5)

cols <- rainbow(7)
countries_names <- c("AUT", "JPN", "CHN", "SVK", "HUN", "RUS", "ROU")

for(i in seq_along(countries_names)){
  country_data <- subset(subset_stud, country == countries_names[i])
  if(nrow(country_data) > 0){
    avg_country <- aggregate(math ~ year, data = country_data, mean, na.rm = TRUE)
    lines(avg_country$year, avg_country$math, col = cols[i], lwd = 2, type = "b")
  }
}

legend("topright", legend = countries_names, col = cols, lty = 1, lwd = 2, 
       cex = 0.6, bty = "n")



subset_stud$comp_num <- as.numeric(factor(subset_stud$computer_n, 
                                          levels = c("0","1","2","3+"),
                                          labels = c(0,1,2,3)))

years_unique <- sort(unique(subset_stud$year))
first_y <- years_unique[1]
mid_y   <- years_unique[ceiling(length(years_unique)/2)]
last_y  <- years_unique[length(years_unique)]

avg_comp <- aggregate(comp_num ~ year, data = subset_stud, mean, na.rm = TRUE)

pie_comp <- c(avg_comp$comp_num[avg_comp$year == first_y],
              avg_comp$comp_num[avg_comp$year == mid_y],
              avg_comp$comp_num[avg_comp$year == last_y])
pie_comp <- round(pie_comp, 2)

cat("Среднее кол-во компьтеров дома\n")
print(cbind(c("2000", "2009", "2022"), pie_comp))
cat('\n')

labels_comp_num <- as.character(pie_comp)
labels_comp_num <- paste(labels_comp_num, "комп. на чел.")
labels_comp_perc <- as.character(round(pie_comp / sum(pie_comp), 4) * 100)
labels_comp_perc <- paste(labels_comp_perc, "%", sep = "")
labels_comp <- paste(labels_comp_num, labels_comp_perc, sep = " | ")

par(mfrow = c(1, 1), xpd = FALSE)
pie(pie_comp, labels = labels_comp, 
    col = c("gold", "lightgreen", "skyblue"),
    main = "Среднее кол-во компьютеров дома", cex.main = 1.5)
legend("topright", legend = c(paste(first_y), paste(mid_y), paste(last_y)), 
       fill = c("gold", "lightgreen", "skyblue"), cex = 1.2)