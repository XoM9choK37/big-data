library(rvest)
library(dplyr)
library(tidyr)
library(ggplot2)

countries <- c("Austria", "Japan", "Russia", "Hungary", "China")
years <- 2014:2026

data_list <- list()

for (year in years) {
  url <- paste0("https://www.numbeo.com/quality-of-life/rankings_by_country.jsp?title=", year)
  cat("Loading", year, "... ")
  
  page <- read_html(url)
  tables <- page %>% html_nodes("table") %>% html_table(fill = TRUE)
  
  df_year <- NULL
  for (tbl in tables) {
    if (ncol(tbl) >= 10 && "Country" %in% colnames(tbl)) {
      df_year <- tbl
      break
    }
  }
  
  if (is.null(df_year)) {
    cat("FAILED\n")
    next
  }
  
  colnames(df_year) <- gsub(" ", "_", colnames(df_year))
  
  df_year <- df_year %>%
    mutate(across(everything(), ~ na_if(as.character(.), "-"))) %>%
    mutate(across(c(contains("Index"), contains("Ratio")), as.numeric),
           Rank = as.numeric(Rank)) %>%
    filter(Country %in% countries) %>%
    mutate(Year = year)
  
  data_list[[as.character(year)]] <- df_year
  cat("SUCCESS (", nrow(df_year), "rows)\n")
}

full_df <- bind_rows(data_list)

write.csv(full_df, "quality_of_life_5countries_2014_2026_clean.csv", row.names = FALSE)

long_df <- full_df %>%
  pivot_longer(
    cols = c(Quality_of_Life_Index, Purchasing_Power_Index, Safety_Index,
             Health_Care_Index, Cost_of_Living_Index, 
             Property_Price_to_Income_Ratio, Traffic_Commute_Time_Index,
             Pollution_Index, Climate_Index),
    names_to = "Indicator",
    values_to = "Value"
  )

cat("\nData ready. Rows:", nrow(full_df), "\n")

cat("\n=== SUMMARY ===\n")
full_df %>%
  group_by(Country) %>%
  summarise(
    QoL_2014 = first(Quality_of_Life_Index[Year == 2014]),
    QoL_2026 = last(Quality_of_Life_Index[Year == 2026]),
    Delta = QoL_2026 - QoL_2014,
    Avg_Pollution = mean(Pollution_Index, na.rm = TRUE),
    Avg_Safety = mean(Safety_Index, na.rm = TRUE)
  ) %>% print()

p1 <- ggplot(full_df, aes(x = Year, y = Quality_of_Life_Index, color = Country)) +
  geom_line(linewidth = 1.2, na.rm = TRUE) + 
  geom_point(size = 2.5, na.rm = TRUE) +
  labs(title = "Quality of Life Index (2014–2026)",
       subtitle = "Austria, Japan, Russia, Hungary, China",
       x = "Year", y = "Quality of Life Index") +
  theme_minimal(base_size = 14) + theme(legend.position = "bottom")

p2 <- ggplot(long_df, aes(x = Year, y = Value, color = Country)) +
  geom_line(linewidth = 1, na.rm = TRUE) + 
  geom_point(size = 1.5, na.rm = TRUE) +
  facet_wrap(~ Indicator, scales = "free_y", ncol = 3) +
  labs(title = "All 9 Quality of Life Indicators (2014–2026)",
       x = "Year", y = "Value") +
  theme_minimal(base_size = 12) + 
  theme(legend.position = "bottom", strip.text = element_text(size = 9))

print(p1)
print(p2)

ggsave("qol_index_one_plot.png", p1, width = 12, height = 6, dpi = 300)
ggsave("all_indicators_facet.png", p2, width = 16, height = 12, dpi = 300)

cat("\nPlots and cleaned CSV saved.\n")