library(tidyverse)

process_suicide_data <- function(file_path) {
  df <- readr::read_csv(file_path) %>%
    mutate(
      year = as.integer(as.character(year)),
    
      suicide_rate = as.numeric(suicides_no) / as.numeric(population) * 100000,

      age_group = factor(age,
                        levels = c("5-14 years", "15-24 years", "25-34 years",
                                 "35-54 years", "55-74 years", "75+ years"),
                        ordered = TRUE),
      
      gdp_per_capita = as.numeric(gsub("[^0-9.]", "", gdp_for_year)),
      
      generation = factor(generation)
    ) %>%
    filter(
      !is.na(suicide_rate),
      !is.na(year),
      population > 0
    )
  
  return(df)
}