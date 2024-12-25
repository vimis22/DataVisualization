library(scales)
library(viridis)

get_gender_colors <- function() {
  c("female" = "#2C7BB6", "male" = "#D7191C")
}

format_rate <- function(x) {
  paste0(round(x, 2), " per 100k")
}

format_population <- function(x) {
  scales::comma(x)
}

calculate_year_over_year_change <- function(data) {
  data %>%
    arrange(year) %>%
    group_by(country) %>%
    mutate(
      yoy_change = (suicide_rate - lag(suicide_rate)) / lag(suicide_rate) * 100
    ) %>%
    ungroup()
}

theme_suicide_dashboard <- function() {
  theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title = element_text(size = 10),
      legend.position = "top",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9)
    )
}