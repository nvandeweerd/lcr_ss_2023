library(tidyverse)

icle_files <- list.files("data/icle_texts/", full.names = TRUE)

icle <- sapply(icle_files, function(x) scan(x, what = character(), sep = "\n") %>%
                 paste0(collapse = "\n") %>%
                 str_remove("<ICLE.*?>") %>%
                 trimws()) 

icle_df <- data.frame(
  file = sapply(icle_files, basename),
  text = icle
)

openxlsx::write.xlsx(icle_df, file = "data/icle.xlsx")
