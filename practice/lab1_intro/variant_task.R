# Task 10
# vect <- sample(1:100, 5)
#
# input_string <- readline()
# divisors <- scan(text = input_string)
#
# divisors <- divisors[divisors >= 2 & divisors <= 5]
#
# if (length(divisors) > 0) {
#   check_matrix <- sapply(divisors, function(d) {
#     vect %% d == 0
#   })
#  
#   counts_per_number <- rowSums(check_matrix)
#  
#   is_multiple <- counts_per_number > 0
#  
#   result <- sum(is_multiple)
# } else {
#   result <- 0
# }
#
# print(result)

# Task 25
# zones_df <- data.frame(
#   Hmax = c(500, 800, 1300, 1600, 2300, 2500, 3300, 5000),
#   Zone = c("Степь", "Дуб", "Бук", "Ель", "Кривол", "Луг", "Субнив","Нивал")
# )
# zones_df$Hmin <- c(400, zones_df$Hmax[-nrow(zones_df)] + 1)
#
# cat("Введите высоту (м): ")
# h <- as.numeric(readline())
# if (!is.na(h)) {
#   zone <- zones_df[h >= zones_df$Hmin & h <= zones_df$Hmax, ]
#   if (nrow(zone) > 0) print(zone) else cat("Высота вне диапазона\n")
# }
