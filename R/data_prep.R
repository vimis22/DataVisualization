library(tidyverse)
library(countrycode)
library(zoo)

process_suicide_data <- function(file_path) {
  df <- readr::read_csv(file_path) %>%
    filter(
      !is.na(suicides_no), 
      !is.na(population),
      !is.na(year),
      population > 0,
      year != 2016
    )

  country_mapping <- c(
      "Antigua and Barbuda" = "Antigua & Barbuda",
      "Bosnia and Herzegovina" = "Bosnia & Herzegovina", 
      "Czech Republic" = "Czechia",
      "Saint Kitts and Nevis" = "St. Kitts & Nevis",
      "Saint Lucia" = "St. Lucia",
      "Saint Vincent and Grenadines" = "St. Vincent & Grenadines",
      "Trinidad and Tobago" = "Trinidad & Tobago",
      "United Kingdom" = "UK",
      "United States" = "USA",
      "Macau" = "Macao SAR China"
  )

  df <- df %>%
    mutate(
      year = as.integer(year),
      suicide_rate = as.numeric(suicides_no) / as.numeric(population) * 100000,


      country = case_when(
        country %in% names(country_mapping) ~ country_mapping[country],
        TRUE ~ country
      ),

      continent = countrycode(sourcevar = country,
                      origin = "country.name",
                      destination = "continent"),
      
      continent = case_when(
        country %in% c(
          'Argentina', 'Brazil', 'Chile', 'Colombia', 
          'Ecuador', 'Guyana', 'Paraguay', 'Suriname', 
          'Uruguay', 'Antigua & Barbuda', 'Aruba', 'Bahamas', 
          'Barbados', 'Belize', 'Costa Rica', 'Cuba', 'Dominica', 
          'El Salvador', 'Grenada', 'Guatemala', 'Jamaica', 
          'Nicaragua', 'Panama', 'Puerto Rico', 'St. Kitts & Nevis', 
          'St. Lucia', 'St. Vincent & Grenadines', 'Trinidad & Tobago'
        ) ~ 'South America',
        continent == 'Americas' ~ 'North America',
        TRUE ~ continent
      ),
      
      continent_detailed = continent,
      continent_map = case_when(
        continent %in% c("North America", "South America") ~ "Americas",
        TRUE ~ continent
      ),
      
      age_group = factor(age,
                        levels = c("5-14 years", 
                                 "15-24 years", 
                                 "25-34 years",
                                 "35-54 years", 
                                 "55-74 years", 
                                 "75+ years"),
                        ordered = TRUE),
      
      sex = recode(sex,
                  "male" = "Male",
                  "female" = "Female"),
      
      gdp_per_capita = as.numeric(gsub("[^0-9.]", "", gdp_for_year)) / population
    )


  df <- df %>%
    group_by(country) %>%
    mutate(
      yoy_change = (suicide_rate - lag(suicide_rate)) / lag(suicide_rate) * 100,
      
      rolling_avg = zoo::rollmean(suicide_rate, k = 3, fill = NA, align = "right"),
      
      rate_zscore = scale(suicide_rate)
    ) %>%
    ungroup()

  df <- df %>%
    mutate(
      gdp_category = case_when(
        gdp_per_capita < quantile(gdp_per_capita, 0.25, na.rm = TRUE) ~ "Low",
        gdp_per_capita < quantile(gdp_per_capita, 0.50, na.rm = TRUE) ~ "Medium-Low",
        gdp_per_capita < quantile(gdp_per_capita, 0.75, na.rm = TRUE) ~ "Medium-High",
        TRUE ~ "High"
      ),
      gdp_category = factor(gdp_category, 
                          levels = c("Low", "Medium-Low", "Medium-High", "High"))
    )

  df <- df %>%
    mutate(
      population_category = case_when(
        population < quantile(population, 0.25, na.rm = TRUE) ~ "Small",
        population < quantile(population, 0.50, na.rm = TRUE) ~ "Medium-Small",
        population < quantile(population, 0.75, na.rm = TRUE) ~ "Medium-Large",
        TRUE ~ "Large"
      ),
      population_category = factor(population_category,
                                 levels = c("Small", "Medium-Small", 
                                          "Medium-Large", "Large"))
    )
  country_trends <- df %>%
    group_by(country) %>%
    summarise(
      has_enough_data = n() >= 5,
      trend_coefficient = if(has_enough_data) {
        tryCatch({
          coef(lm(suicide_rate ~ year))[2]
        }, error = function(e) NA_real_)
      } else NA_real_,
      trend_p_value = if(has_enough_data) {
        tryCatch({
          summary(lm(suicide_rate ~ year))$coefficients[2,4]
        }, error = function(e) NA_real_)
      } else NA_real_,
      .groups = 'drop'
    )

  df <- df %>%
    left_join(country_trends, by = "country") %>%
    mutate(
      trend_direction = case_when(
        trend_p_value < 0.05 & trend_coefficient > 0 ~ "Increasing",
        trend_p_value < 0.05 & trend_coefficient < 0 ~ "Decreasing",
        TRUE ~ "No Significant Trend"
      )
    )

  gender_metrics <- df %>%
    group_by(country, year) %>%
    summarize(
      gender_ratio = mean(suicide_rate[sex == "Male"]) / 
                    mean(suicide_rate[sex == "Female"]),
      gender_gap = mean(suicide_rate[sex == "Male"]) - 
                   mean(suicide_rate[sex == "Female"]),
      .groups = 'drop'
    )

  df <- df %>%
    left_join(gender_metrics, by = c("country", "year"))

  return(df)
}

validate_country_names <- function(data) {
  valid_countries <- maps::map.world$names
  
  mismatched <- setdiff(unique(data$country), valid_countries)
  
  if(length(mismatched) > 0) {
    warning(paste("The following countries may not display correctly on maps:",
                 paste(mismatched, collapse = ", ")))
  }
  
  return(mismatched)
}

get_data_ranges <- function(data) {
  list(
    year_range = range(data$year),
    gdp_range = range(data$gdp_per_capita),
    suicide_rate_range = range(data$suicide_rate),
    population_range = range(data$population),
    countries = sort(unique(data$country)),
    continents = sort(unique(data$continent_detailed)),
    age_groups = levels(data$age_group)
  )
}