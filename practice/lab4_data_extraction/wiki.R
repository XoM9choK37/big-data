library(rvest)
library(dplyr)
library(stringr)

cat_url <- "https://ru.wikipedia.org/wiki/Категория:Музеи_Калининграда"
cat_page <- read_html(cat_url)

museum_nodes <- cat_page %>% html_nodes("div.mw-category-group ul li a")

museum_names <- museum_nodes %>% html_text(trim = TRUE)
museum_links <- museum_nodes %>% html_attr("href") %>% paste0("https://ru.wikipedia.org", .)

cat("Museums found:", length(museum_names), "\n")

data_list <- list()

for (i in seq_along(museum_names)) {
  cat("Processing", i, "of", length(museum_names), "->", museum_names[i], "... ")
  
  tryCatch({
    muse_page <- read_html(museum_links[i])
    
    desc_nodes <- muse_page %>% html_nodes("p")
    description <- desc_nodes %>% html_text(trim = TRUE) %>% .[1]
    if (is.na(description) || nchar(description) < 20) {
      description <- "Description not found in first paragraph"
    }
    
    address <- "Address not specified"
    infobox <- muse_page %>% html_node("table.infobox")
    if (!is.null(infobox)) {
      rows <- infobox %>% html_nodes("tr")
      for (row in rows) {
        th_text <- row %>% html_node("th") %>% html_text(trim = TRUE)
        if (!is.null(th_text) && grepl("Адрес|Местонахождение|Location", th_text, ignore.case = TRUE)) {
          address <- row %>% html_node("td") %>% html_text(trim = TRUE) %>% str_squish()
          break
        }
      }
    }
    
    photo_link <- muse_page %>% 
      html_nodes("table.infobox img") %>% 
      html_attr("src") %>% 
      .[1]
    if (!is.na(photo_link) && !is.null(photo_link)) {
      photo_link <- paste0("https:", photo_link)
    } else {
      photo_link <- "Photo not found"
    }
    
    data_list[[i]] <- tibble(
      Museum_Name = museum_names[i],
      Address = address,
      Description = str_sub(description, 1, 600),
      Wiki_Link = museum_links[i],
      Photo_Link = photo_link
    )
    
    cat("SUCCESS\n")
    
  }, error = function(e) {
    cat("FAILED\n")
    data_list[[i]] <- tibble(
      Museum_Name = museum_names[i],
      Address = "Error loading page",
      Description = "",
      Wiki_Link = museum_links[i],
      Photo_Link = ""
    )
  })
}

museums_df <- bind_rows(data_list)

write.csv(museums_df, "museums_kaliningrad_full.csv", row.names = FALSE, fileEncoding = "UTF-8")

cat("\nCompleted. Data saved to museums_kaliningrad_full.csv\n")
print(museums_df)

museums_df %>% select(Museum_Name, Address, Wiki_Link) %>% head(5) %>% print()