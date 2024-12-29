library(shiny)
library(shinydashboard)
library(tidyverse)
library(gganimate)
library(countrycode)
library(scales)
library(viridis)
library(plotly)
library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(ggalt)
library(maps)
library(mapproj)

# Data preprocessing

data <- read.csv("data.csv")
data <- filter(data, year != 2016)
data$generation <- NULL
data$HDI.for.year <- NULL

data <- data %>%
  group_by(country) %>%
  filter(n_distinct(year) > 3) %>%
  ungroup()

data$sex <- ifelse(data$sex == "male", "Male", "Female")
data$age <- factor(data$age, levels = c("5-14 years", "15-24 years", "25-34 years", "35-54 years", "55-74 years", "75+ years"), ordered = TRUE)
data$gdp_for_year = as.numeric(gsub("[^0-9.]", "", data$gdp_for_year))

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

data$country <- case_when(
  data$country %in% names(country_mapping) ~ country_mapping[data$country],
  TRUE ~ data$country
)

data$continent = countrycode(sourcevar = data$country, 
                             origin = "country.name", 
                             destination = "continent")

data$continent = case_when(
  data$country %in% c(
    'Argentina', 'Brazil', 'Chile', 'Colombia', 
    'Ecuador', 'Guyana', 'Paraguay', 'Suriname', 
    'Uruguay', 'Antigua & Barbuda', 'Aruba', 'Bahamas', 
    'Barbados', 'Belize', 'Costa Rica', 'Cuba', 'Dominica', 
    'El Salvador', 'Grenada', 'Guatemala', 'Jamaica', 
    'Nicaragua', 'Panama', 'Puerto Rico', 'St. Kitts & Nevis', 
    'St. Lucia', 'St. Vincent & Grenadines', 'Trinidad & Tobago'
  ) ~ 'South America',
  data$continent == 'Americas' ~ 'North America',
  TRUE ~ data$continent
)

data$continent_map = case_when(
  data$continent %in% c("North America", "South America") ~ "Americas",
  TRUE ~ data$continent
)

data$continent <- as.factor(data$continent)
data$continent_map <- as.factor(data$continent_map)

# Plots

global_mean_over_time <- (sum(as.numeric(data$suicides_no)) / sum(as.numeric(data$population))) * 100000
blue <- "#2C7BB6"
calculate_suicide_rate <- function(data, ...) {
  data %>%
    group_by(...) %>%
    summarise(
      total_suicides = sum(suicides_no),
      total_population = sum(population),
      suicide_rate = (total_suicides / total_population) * 100000,
      .groups = 'drop'
    )
}

create_time_series <- function(data) {
  data %>% 
    group_by(year) %>%
    summarise(suicides.100k.pop = mean(suicides.100k.pop, na.rm = TRUE)) %>%
    ggplot(aes(x = year, y = suicides.100k.pop)) +
    geom_line(color = blue, size = 1) +
    geom_point(color = blue, size = 2) +
    geom_hline(yintercept = global_mean_over_time, 
               linetype = "dashed", 
               color = "red",
               alpha = 0.7) +
    scale_x_continuous(breaks = seq(min(data$year), max(data$year), by = 5)) +
    scale_y_continuous(labels = function(x) paste0(round(x, 1), " per 100k")) +
    labs(
      title = "Global Suicide Rate Over Time in Years 1985 - 2015",
      subtitle = "The global average suicide rate; the mean over this period is approximately 13.",
      x = "Year",
      y = "Suicide Rate per 100,000 Population"
    )
}

p1 <- create_time_series(data)
#print(p1)

create_gender_overview <- function(data, plot_type = "line") {
  if(plot_type == "line") {
    data %>% 
      group_by(year, sex) %>%
      summarise(suicides.100k.pop = mean(suicides.100k.pop), .groups = 'drop') %>%
      ggplot(aes(x = year, y = suicides.100k.pop, color = sex)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
        geom_hline(yintercept = global_mean_over_time, 
            linetype = "dashed", 
            color = "red",
            alpha = 0.7) +
      scale_color_manual(values = c("Female" = "pink", "Male" = blue)) +
      scale_x_continuous(breaks = seq(min(data$year), max(data$year), by = 5)) +
      labs(
        title = "Suicide Rates by Gender Over Time",
        subtitle = "Average rates per 100,000 population",
        x = "Year",
        y = "Suicide Rate",
        color = "Gender"
      ) +
      theme_minimal()
  } else if(plot_type == "pie") {
    pie_data <- data %>%
      group_by(sex) %>%
      summarise(suicides.100k.pop = mean(suicides.100k.pop)) 
    
    plot_ly(pie_data, 
            labels = ~sex, 
            values = ~suicides.100k.pop,
            type = 'pie',
            marker = list(colors = c("pink", blue))) %>%
      layout(title = "Distribution of Suicides by Gender",
             showlegend = TRUE)
  } else {
    data %>%
      group_by(sex) %>%
      summarise(suicides.100k.pop = mean(suicides.100k.pop)) %>%
      ggplot(aes(x = sex, y = suicides.100k.pop, fill = sex)) +
      geom_bar(stat = "identity") +
      geom_hline(yintercept = global_mean_over_time, 
            linetype = "dashed", 
            color = "red",
            alpha = 0.7) +
      scale_fill_manual(values = c("Female" = "pink", "Male" = blue)) +
      labs(
        title = "Average Suicide Rates by Gender",
        x = "Gender",
        y = "Suicide Rate (per 100k population)",
        fill = "Gender"
      ) +
      theme_minimal()
  }
}

p2 <- create_gender_overview(data, "pie")
#print(p2)

create_age_overview <- function(data, plot_type = "bar") {
  age_data <- calculate_suicide_rate(data, age)
  
  if(plot_type == "pie") {
    plot_ly(age_data, 
            labels = ~age, 
            values = ~suicide_rate,
            type = 'pie',
            marker = list(colors = viridis(6))) %>%
      layout(
        title = "Distribution of Suicides by Age Group",
        showlegend = TRUE
      )
  } else {
    ggplot(age_data, aes(x = age, y = suicide_rate, fill = age)) +
      geom_bar(stat = "identity") +
      geom_hline(yintercept = global_mean_over_time, 
            linetype = "dashed", 
            color = "red",
            alpha = 0.7) +
      scale_fill_viridis_d() +
      labs(
        title = "Average Suicide Rates by Age Group in Years 1985-2015",
        subtitle = "Average suicide rates in time by age. Higher age groups are at more risk of suicide.",
        x = "Age Group",
        y = "Suicide Rate (per 100k population)",
        fill = "Age Group"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  }
}

p3 <- create_age_overview(data, "pie")
#print(p3)

create_age_time_series <- function(data) {
  age_time_data <- calculate_suicide_rate(data, year, age)
  
  ggplot(age_time_data, aes(x = year, y = suicide_rate, color = age)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    scale_color_viridis_d() +
    geom_hline(yintercept = global_mean_over_time, 
            linetype = "dashed", 
            color = "red",
            alpha = 0.7) +
    scale_x_continuous(breaks = seq(min(data$year), max(data$year), by = 5)) +
    labs(
      title = "Suicide Rates per 100k people by Age Group Over Time",
      subtitle = "Annual rates of suicide split into age bands. Across all age bands, the suicide rate is decreasing.",
      x = "Year",
      y = "Suicide Rate per 100k Population",
      color = "Age Group"
    ) +
    theme_minimal()
}

p4 <- create_age_time_series(data)
#print(p4)

create_continent_bar <- function(data, plot_type = "bar") {
  continent_data <- calculate_suicide_rate(data, continent)
  
  if(plot_type == "bar") {
    ggplot(continent_data, 
           aes(x = reorder(continent, -suicide_rate), 
               y = suicide_rate, 
               fill = continent)) +
      geom_bar(stat = "identity") +
      scale_fill_viridis_d() +
      geom_hline(yintercept = global_mean_over_time, 
            linetype = "dashed", 
            color = "red",
            alpha = 0.7) +
      labs(
        title = "Average Suicide Rates per 100k people by Continent through 1985-2015",
        subtitle = "Average suicide rate by continent. It should be noted that Africa has poor data quality.",
        x = "Continent",
        y = "Suicide Rate per 100,000 Population"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  } else {
    plot_ly(
      continent_data,
      labels = ~continent,
      values = ~suicide_rate,
      type = 'pie',
      marker = list(colors = viridis(n = nrow(continent_data)))
    ) %>%
      layout(title = "Distribution of Suicides by Continent")
  }
}

p5 <- create_continent_bar(data, "pie")
#print(p5)

create_continent_gender_bar <- function(data) {
  continent_gender_data <- calculate_suicide_rate(data, continent, sex)
  
  ggplot(continent_gender_data, aes(x = reorder(continent, -suicide_rate), 
                                   y = suicide_rate, 
                                   fill = sex)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_hline(yintercept = global_mean_over_time, 
            linetype = "dashed", 
            color = "red",
            alpha = 0.7) +
    scale_fill_manual(values = c("Female" = "pink", "Male" = blue)) +
    labs(
      title = "Average Suicide Rates by Continent and Gender through 1985-2015",
      subtitle = "Throughout all continents, males are 3x more likely than females to die by suicide.",
      x = "Continent",
      y = "Suicide Rate per 100,000 Population",
      fill = "Gender"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

p6 <- create_continent_gender_bar(data)
#print(p6)

create_continent_age_bar <- function(data) {
  continent_age_data <- calculate_suicide_rate(data, continent, age)
  
  ggplot(continent_age_data, aes(x = reorder(continent, -suicide_rate), 
                                y = suicide_rate, 
                                fill = age)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_hline(yintercept = global_mean_over_time, 
            linetype = "dashed", 
            color = "red",
            alpha = 0.7) +
    scale_fill_viridis_d() +
    labs(
      title = "Average Suicide Rates by Continent and Age Group through 1985-2015",
      subtitle = "When controlled for continent, different patterns emerge in the suicide rate in age groups.",
      x = "Continent",
      y = "Suicide Rate per 100,000 Population",
      fill = "Age Group"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

p6 <- create_continent_age_bar(data)
#print(p6)

create_continent_time_series <- function(data) {
  continent_time_data <- calculate_suicide_rate(data, year, continent)
  
  ggplot(continent_time_data, aes(x = year, y = suicide_rate, color = continent)) +
    geom_line(size = 1) +
    geom_point(size = 2) +
    geom_hline(yintercept = global_mean_over_time, 
            linetype = "dashed", 
            color = "red",
            alpha = 0.7) +
    scale_color_viridis_d() +
    labs(
      title = "Suicide Rates by Continent Over Time through 1985-2015",
      subtitle = "When showing suicide rate over time, it's visible that Africa's data quality drops in 1996.",
      x = "Year",
      y = "Suicide Rate per 100,000 Population",
      color = "Continent"
    ) +
    theme_minimal()
}

p7 <- create_continent_time_series(data)
#print(p7)

create_continent_choropleth <- function(data) {
  continent_data <- calculate_suicide_rate(data, continent_map)
  
  world <- map_data("world")
  world$continent <- countrycode(world$region,
                                origin = "country.name",
                                destination = "continent",
                                warn = FALSE)
  
  world <- left_join(world, continent_data, 
                    by = c("continent" = "continent_map"))
  
  ggplot(world, aes(x = long, y = lat, group = group, fill = suicide_rate)) +
    geom_polygon(color = "white", size = 0.1) +
    coord_map(projection = "mercator") +
    scale_fill_viridis_c(
      name = "Suicide Rate\n(per 100k)",
      na.value = "gray80"
    ) +
    labs(title = "Global Suicide Rates by Continent") +
    theme_void()
}

p8 <- create_continent_choropleth(data)
#print(p8)

create_country_bar <- function(data, top_n = 20) {
  country_continent <- data %>%
    group_by(country) %>%
    slice(1) %>%
    select(country, continent)
  
  country_data <- calculate_suicide_rate(data, country) %>%
    left_join(country_continent, by = "country") %>%
    top_n(top_n, suicide_rate)
  
  ggplot(country_data, 
         aes(x = reorder(country, suicide_rate), 
             y = suicide_rate, 
             fill = continent)) +
    geom_bar(stat = "identity") +
    geom_hline(yintercept = global_mean_over_time, 
            linetype = "dashed", 
            color = "red",
            alpha = 0.7) +
    scale_fill_viridis_d() +
    coord_flip() +
    labs(
      title = paste("Top", top_n, "Countries by Average Suicide Rate through 1985-2015"),
      subtitle = "According to this dataset, the highest suicide rate of countries is in Europe.",
      x = "Country",
      y = "Suicide Rate per 100,000 Population",
      fill = "Continent"
    ) +
    theme_minimal()
}

p9 <- create_country_bar(data)
#print(p9)

create_country_gender_proportion <- function(data) {
  country_gender_data <- calculate_suicide_rate(data, country, sex)
  
  country_gender_data %>%
    group_by(country) %>%
    mutate(total = sum(suicide_rate),
           proportion = suicide_rate / total) %>%
    ggplot(aes(x = reorder(country, -total), y = proportion, fill = sex)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = c("Female" = "pink", "Male" = blue)) +
    coord_flip() +
    labs(
      title = "Gender Distribution of Suicides by Country",
      x = "Country",
      y = "Proportion",
      fill = "Gender"
    ) +
    theme_minimal()
}

p10 <- create_country_gender_proportion(data)
#print(p10)

create_country_age_proportion <- function(data) {
  country_age_data <- calculate_suicide_rate(data, country, age)
  
  country_age_data %>%
    group_by(country) %>%
    mutate(total = sum(suicide_rate),
           proportion = suicide_rate / total) %>%
    ggplot(aes(x = reorder(country, -total), y = proportion, fill = age)) +
    geom_bar(stat = "identity") +
    scale_fill_viridis_d() +
    coord_flip() +
    labs(
      title = "Age Distribution of Suicides by Country",
      x = "Country",
      y = "Proportion",
      fill = "Age Group"
    ) +
    theme_minimal()
}

p11 <- create_country_age_proportion(data)
#print(p11)

create_gender_disparity_plot <- function(data) {
  gender_data <- calculate_suicide_rate(data, country, year, sex) %>%
    group_by(country, sex) %>%
    summarise(
      suicide_rate = mean(suicide_rate),
      .groups = 'drop'
    ) %>%
    pivot_wider(names_from = sex, values_from = suicide_rate) %>%
    mutate(disparity = Male - Female) %>%
    arrange(disparity)

  gender_data$country <- factor(gender_data$country, 
                              levels = gender_data$country)

  ggplot(gender_data, 
         aes(y = country)) +
    geom_dumbbell(aes(x = Female, xend = Male),
                  size = 1,
                  colour = "grey80",
                  colour_x = "pink",  
                  colour_xend = blue,  
                  dot_guide = FALSE,
                  size_x = 3,
                  size_xend = 3) +
    labs(
      title = "Gender Disparity in Suicide Rates",
      x = "Suicide Rate per 100,000 Population",
      y = "Country"
    ) +
    theme_minimal() +
    theme(
      panel.grid.major.y = element_line(color = "grey90"),
      panel.grid.minor = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 16)
    )
}

p12 <- create_gender_disparity_plot(data)
#print(p12)

create_animated_violin_plot <- function(data, selected_year = NULL) {
  if(is.null(selected_year)) {
    base_plot <- data %>%
      mutate(year = as.integer(year)) %>%
      ggplot(aes(x = age, y = suicides.100k.pop, fill = age)) +
      geom_violin(trim = FALSE) +
      geom_boxplot(width = 0.2, fill = "white", alpha = 0.5) +
      scale_y_continuous(limits = c(0, 190)) +
      scale_fill_viridis_d() +
      labs(
        title = "Distribution of Suicide Rates by Age Group",
        subtitle = "Year: {closest_state}",
        x = "Age Group",
        y = "Suicide Rate (per 100k population)"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none"
      )

    anim <- base_plot +
      transition_states(year, 
                       transition_length = 2,
                       state_length = 1) +
      ease_aes('linear')
    
    return(anim)
    
  } else {
    data %>%
      filter(year == selected_year) %>%
      ggplot(aes(x = age, y = suicides.100k.pop, fill = age)) +
      geom_violin(trim = FALSE) +
      geom_boxplot(width = 0.2, fill = "white", alpha = 0.5) +
      scale_y_continuous(limits = c(0, 190)) +
      scale_fill_viridis_d() +
      labs(
        title = "Distribution of Suicide Rates by Age Group",
        subtitle = paste("Year:", selected_year),
        x = "Age Group",
        y = "Suicide Rate (per 100k population)"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none"
      )
  }
}

p16 <- create_animated_violin_plot(data)
#print(p16)

create_gdp_suicide_scatter <- function(data) {

  country_continent <- data %>%
    group_by(country) %>%
    slice(1) %>%
    select(country, continent)
  
  gdp_data <- data %>%
    group_by(country, year) %>%
    summarise(
      total_suicides = sum(suicides_no),
      total_population = sum(population),
      suicide_rate = (total_suicides / total_population) * 100000,
      gdp_per_capita = mean(gdp_per_capita),
      .groups = 'drop'
    ) %>%
    group_by(country) %>%
    summarise(
      suicide_rate = mean(suicide_rate),
      gdp_per_capita = mean(gdp_per_capita),
      total_population = mean(total_population),
      .groups = 'drop'
    ) %>%
    left_join(country_continent, by = "country")
  
  ggplot(gdp_data, 
         aes(x = gdp_per_capita, 
             y = suicide_rate, 
             size = total_population,
             color = continent)) +
    geom_point(alpha = 0.6, aes(text = country)) +
    geom_smooth(
      method = "lm",
      se = TRUE,
      color = "black",
      size = 0.5,
      alpha = 0.2,
      aes(weight = total_population)
    ) +
    scale_size_continuous(
      range = c(2, 20),
      labels = scales::comma
    ) +
    scale_x_continuous(labels = scales::dollar_format()) +
    scale_color_viridis_d() +
    labs(
      title = "GDP per Capita vs Average Suicide Rate, 1985-2015",
      subtitle = "Line of best fit shows a weak relationship with GDP per Capita to suicide",
      x = "GDP per Capita",
      y = "Suicide Rate per 100,000 Population",
      color = "Continent",
      size = "Total Population"
    ) +
    theme_minimal()
}

p17 <- create_gdp_suicide_scatter(data)
#print(p17)

create_animated_gdp_plot <- function(data, selected_year = NULL) {
  country_continent <- data %>%
    group_by(country) %>%
    slice(1) %>%
    select(country, continent)
  
  gdp_data <- data %>%
    group_by(country, year) %>%
    summarise(
      total_suicides = sum(suicides_no),
      total_population = sum(population),
      suicide_rate = (total_suicides / total_population) * 100000,
      gdp_per_capita = mean(gdp_per_capita),
      .groups = 'drop'
    ) %>%
    left_join(country_continent, by = "country") %>%
    mutate(year = as.integer(year)) %>%
    {if (!is.null(selected_year)) filter(., year == selected_year) else .}
  
  base_plot <- ggplot(gdp_data, 
              aes(x = gdp_per_capita, 
                  y = suicide_rate,
                  size = total_population,
                  color = continent,
                  tooltip = country)) +
    geom_point(alpha = 0.6) +
    scale_size_continuous(range = c(2, 20)) +
    scale_x_continuous(limits = c(0, 110000), labels = scales::dollar_format()) +    
    scale_y_continuous(limits = c(0, 160)) +
    scale_color_viridis_d() +
    labs(
      title = "GDP per Capita vs Suicide Rate",
      subtitle = if(is.null(selected_year)) "Year: {frame_time}" else paste("Year:", selected_year),
      x = "GDP per Capita (log scale)", 
      y = "Suicide Rate (per 100k population)",
      color = "Continent",
      size = "Total Population"
    ) +
    theme_minimal()
  
  if (is.null(selected_year)) {
    base_plot + 
      transition_time(year) +
      ease_aes('linear')
  } else {
    base_plot
  }
}

p18 <- create_animated_gdp_plot(data)
#print(p18)

get_data_ranges <- function(data) {
  list(
    year_range = range(data$year),
    gdp_range = range(data$gdp_per_capita),
    suicide_rate_range = range(data$suicides.100k.pop),
    countries = sort(unique(data$country)),
    continents = sort(unique(data$continent)),
    age_groups = levels(data$age)
  )
}

create_country_choropleth <- function(data) {
  country_data <- calculate_suicide_rate(data, country) %>%
    rename(region = country)
  
  world <- map_data("world")
  world <- left_join(world, country_data, by = "region", warn = FALSE)
  
  ggplot(world, aes(x = long, y = lat, group = group, fill = suicide_rate)) +
    geom_polygon(color = "white", size = 0.1) +
    coord_map(projection = "mercator") +
    scale_fill_viridis_c(
      name = "Suicide Rate\n(per 100k)",
      na.value = "gray80"
    ) +
    labs(title = "Global Suicide Rates by Country") +
    theme_void()
}

#c11 <- create_country_choropleth(data)

ui <- dashboardPage(
  dashboardHeader(title = "Global Suicide Rates Analysis"),
  
  dashboardSidebar(
    tags$head(
      tags$style(HTML("
        .main-header {
          position: fixed;
          width: 100%;
          top: 0;
          z-index: 1030;
        }
        .main-sidebar {
          position: fixed;
          top: 50px;
          height: calc(100vh - 50px);
          padding-top: 0;
        }
        .sidebar {
          height: 100%;
          overflow-y: auto;
          width: inherit;
          padding-top: 0;
        }
        .content-wrapper {
          margin-top: 50px;
          margin-left: 230px;
          padding-top: 0px;
        }
        .sidebar-menu {
          margin-top: 0;
        }
      "))
    ),
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Geographic Analysis", tabName = "geographic", icon = icon("globe")),
      menuItem("Demographics", tabName = "demographics", icon = icon("users")),
      menuItem("Economic Factors", tabName = "economic", icon = icon("chart-line")),

        downloadButton("downloadPDF", "Download Report", 
                style = "margin: 10px 15px; width: 90%"),
      
      hr(),
      sliderInput("yearRange", "Year Range:",
                  min = 1985, max = 2015,
                  value = c(1985, 2015),
                  step = 1,
                  sep = ""),
      selectInput("countries", "Select Countries:",
                  choices = NULL,
                  multiple = TRUE),
      selectInput("continents", "Select Continents:",
                  choices = NULL,
                  multiple = TRUE),
      selectInput("ageGroups", "Select Age Groups:",
                  choices = NULL,
                  multiple = TRUE),
      selectInput("gender", "Select Gender:",
                  choices = c("Male", "Female"),
                  multiple = TRUE)
    )
  ),

  dashboardBody(
    tags$head(
      tags$style(HTML("
        .small-box {height: 100px}
        .tab-content {padding: 10px}
      "))
    ),
    
    tabItems(
      tabItem(tabName = "overview",
        fluidRow(
          box(width = 12,
              title = "Global Suicide Rates Over Time",
              plotlyOutput("timeSeriesPlot")
          )
        ),
        fluidRow(
          box(width = 6,
              title = "Suicide Rates by Gender",
              radioButtons("genderPlotType", "Plot Type:",
                         choices = c("line", "pie", "bar"),
                         selected = "bar", inline = TRUE),
              plotlyOutput("genderPlot")
          ),
          box(width = 6,
              title = "Suicide Rates by Age",
              radioButtons("agePlotType", "Plot Type:",
                         choices = c("bar", "pie"),
                         selected = "bar", inline = TRUE),
              plotlyOutput("agePlot")
          )
        ),
        fluidRow(
          box(width = 12,
              title = "Age Groups Over Time",
              plotlyOutput("ageTimeSeries")
          )
        ),
        fluidRow(
          box(width = 12,
              title = "Suicide Rates by Gender",
              radioButtons("genderTimeSeriesOverviewPlotType", "Plot Type:",
                         choices = c("line", "pie", "bar"),
                         selected = "line", inline = TRUE),
              plotlyOutput("genderTimeSeriesOverview")
          ),
        )
      ),

      tabItem(tabName = "geographic",
        tabBox(width = 12,
          tabPanel("Continental Analysis",
            fluidRow(
              box(width = 6,
                  title = "Suicide Rates by Continent",
                  radioButtons("continentPlotType", "Plot Type:",
                             choices = c("bar", "pie"),
                             selected = "bar", inline = TRUE),
                  plotlyOutput("continentBarPlot")
              ),
              box(width = 6,
                  title = "Continent Map",
                  plotlyOutput("continentMap")
              )
            ),
            fluidRow(
              box(width = 6,
                  title = "By Gender",
                  plotlyOutput("continentGenderPlot")
              ),
              box(width = 6,
                  title = "By Age",
                  plotlyOutput("continentAgePlot")
              ),
            ),
            fluidRow(
              box(width = 12,
                  title = "Time Trends",
                  plotlyOutput("continentTimeSeries")
              )
            )
          ),
          tabPanel("Country Analysis",
            fluidRow(
              box(width = 12,
                  title = "Country Map",
                  plotlyOutput("countryMap")
              )
            ),
            fluidRow(
              box(width = 6,
                  title = "Top Countries",
                  sliderInput("topN", "Number of countries:", 
                            min = 5, max = 50, value = 50),
                  div(
                    style = "height:600px; overflow-y: scroll;",
                    plotlyOutput("countryBarPlot", height = "1500px")
                ),
              ),
              box(width = 6,
                    title = "Gender Distribution",
                    div(
                  style = "height:600px; overflow-y: scroll;",
                  plotlyOutput("countryGenderPlot", height = "1500px")
                )
              )
            ),
            fluidRow(
              box(width = 12,
                  title = "Age Distribution",
                    div(
                  style = "height:600px; overflow-y: scroll;",
                  plotlyOutput("countryAgePlot", height = "1500px")
                )
              )
            )
          )
        )
      ),

      tabItem(tabName = "demographics",
        tabBox(width = 12,
          tabPanel("Gender Analysis",
            fluidRow(
            box(width = 12,
                title = "Suicide Rates by Gender",
                radioButtons("genderOverviewType", "Plot Type:",
                            choices = c("line", "pie", "bar"),
                            selected = "line", inline = TRUE),
                plotlyOutput("genderOverview")
            ),
            )
          ),
          tabPanel(
            "Age Analysis",
            fluidRow(
              box(
                width = 12,
                title = "Age Distribution Over Time",
                fluidRow(
                  column(
                    width = 12,
                    plotlyOutput("ageViolinPlotAnimated", height = "500px")
                  )
                ),
                fluidRow(
                  column(
                    width = 12,
                    sliderInput(
                      "violinYearSlider",
                      "Select Year:",
                      min = 1985,
                      max = 2015,
                      value = 1985,
                      step = 1,
                      sep = ""
                    ),
                    div(
                      style = "margin-top: 20px; text-align: center;",
                      actionButton("playPauseViolin", "Play/Pause", icon = icon("play")),
                      actionButton("resetViolin", "Reset", icon = icon("sync"))
                    )
                  )
                )
              )
            )
          )
        )
      ),

      tabItem(tabName = "economic",
      tabBox(width = 12,
      tabPanel(title = "GDP",
            fluidRow(
              box(width = 12,
                    title = "GDP vs Suicide Rate Relationship",
                  plotlyOutput("gdpScatterPlot")
              )
            ),
      ),
          tabPanel(
            title = "GDP animated",
            fluidRow(
              box(
                width = 12,
                title = "GDP vs Suicide Rate Relationship",
                fluidRow(
                  column(
                    width = 12,
                    sliderInput(
                      "gdpYearSlider",
                      "Select Year:",
                      min = 1985,
                      max = 2015,
                      value = 1985,
                      step = 1,
                      sep = ""
                    ),
                    div(
                      style = "margin-top: 20px; text-align: center;",
                      actionButton("playPauseGDP", "Play/Pause", icon = icon("play")),
                      actionButton("resetGDP", "Reset", icon = icon("sync"))
                    )
                  )
                ),
                fluidRow(
                  column(
                    width = 12,
                    plotlyOutput("gdpAnimatedPlot", height = "500px")
                  )
                )
              )
            )
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    req(input$yearRange)
    data %>%
      filter(
        year >= input$yearRange[1],
        year <= input$yearRange[2]
      ) %>%
      {if (!is.null(input$countries) && length(input$countries) > 0)
        filter(., country %in% input$countries)
        else .} %>%
      {if (!is.null(input$continents) && length(input$continents) > 0)
        filter(., continent %in% input$continents)
        else .} %>%
      {if (!is.null(input$ageGroups) && length(input$ageGroups) > 0)
        filter(., age %in% input$ageGroups)
        else .} %>%
      {if (!is.null(input$gender) && length(input$gender) > 0)
        filter(., sex %in% input$gender)
        else .} %>%
      {if (!is.null(input$gdpRange))
        filter(., gdp_per_capita >= input$gdpRange[1], 
               gdp_per_capita <= input$gdpRange[2])
        else .}
  })

  rv <- reactiveValues(
    animating = FALSE,
    timer = NULL
  )
  
  observe({
    if (rv$animating) {
      rv$timer <- invalidateLater(100)
      isolate({
        newYear <- input$violinYearSlider + 1
        if (newYear > 2015) {
          newYear <- 1985
        }
        updateSliderInput(session, "violinYearSlider", value = newYear)
      })
    }
  })
  
  observeEvent(input$playPauseViolin, {
    rv$animating <- !rv$animating
    if (rv$animating) {
      updateActionButton(session, "playPauseViolin",
                        icon = icon("pause"))
    } else {
      updateActionButton(session, "playPauseViolin",
                        icon = icon("play"))
    }
  })
  
  observeEvent(input$resetViolin, {
    rv$animating <- FALSE
    updateActionButton(session, "playPauseViolin",
                      icon = icon("play"))
    updateSliderInput(session, "violinYearSlider",
                     value = 1985)
  })

  output$ageViolinPlotAnimated <- renderPlotly({
    year_data <- filtered_data() %>%
      filter(year == input$violinYearSlider)
    
    p <- create_animated_violin_plot(year_data, input$violinYearSlider)
    
    ggplotly(p) %>%
      layout(
        showlegend = FALSE,
        margin = list(l = 50, r = 50, b = 100, t = 50)
      )
  })

  rvGdp <- reactiveValues(
    animating = FALSE,
    timer = NULL,
    gdpAnimating = FALSE,
    gdpTimer = NULL
  )

  observe({
    if (rvGdp$gdpAnimating) {
      rvGdp$gdpTimer <- invalidateLater(100)
      isolate({
        newYear <- input$gdpYearSlider + 1
        if (newYear > 2015) {
          newYear <- 1985
        }
        updateSliderInput(session, "gdpYearSlider", value = newYear)
      })
    }
  })

  observeEvent(input$playPauseGDP, {
    rvGdp$gdpAnimating <- !rvGdp$gdpAnimating
    if (rvGdp$gdpAnimating) {
      updateActionButton(session, "playPauseGDP",
                        icon = icon("pause"))
    } else {
      updateActionButton(session, "playPauseGDP",
                        icon = icon("play"))
    }
  })

  observeEvent(input$resetGDP, {
    rvGdp$gdpAnimating <- FALSE
    updateActionButton(session, "playPauseGDP",
                      icon = icon("play"))
    updateSliderInput(session, "gdpYearSlider",
                    value = 1985)
  })

  output$gdpAnimatedPlot <- renderPlotly({
    year_data <- filtered_data() %>%
      filter(year == input$gdpYearSlider)
    
    p <- create_animated_gdp_plot(year_data, input$gdpYearSlider)
    
    ggplotly(p) %>%
      layout(
        showlegend = TRUE,
        legend = list(orientation = "h", y = -0.2),
        margin = list(l = 50, r = 50, b = 100, t = 50)
      )
  })

  output$timeSeriesPlot <- renderPlotly({
    p <- create_time_series(filtered_data())
    ggplotly(p) %>% 
      layout(legend = list(orientation = "h", y = -0.2))
  })

  output$genderPlot <- renderPlotly({
    p <- create_gender_overview(filtered_data(), input$genderPlotType)
    ggplotly(p) %>% 
      layout(legend = list(orientation = "h", y = -0.2))
  })

  output$agePlot <- renderPlotly({
    p <- create_age_overview(filtered_data(), input$agePlotType)
    ggplotly(p)
  })

  output$ageTimeSeries <- renderPlotly({
    p <- create_age_time_series(filtered_data())
    ggplotly(p) %>% 
      layout(legend = list(orientation = "h", y = -0.2))
  })

  output$genderTimeSeriesOverview <- renderPlotly({
    p <- create_gender_overview(filtered_data(), input$genderTimeSeriesOverviewPlotType)
    ggplotly(p) %>% 
      layout(legend = list(orientation = "h", y = -0.2))
  })

  output$continentBarPlot <- renderPlotly({
    p <- create_continent_bar(filtered_data(), input$continentPlotType)
    ggplotly(p)
  })

  output$continentGenderPlot <- renderPlotly({
    p <- create_continent_gender_bar(filtered_data())
    ggplotly(p)
  })

  output$continentAgePlot <- renderPlotly({
    p <- create_continent_age_bar(filtered_data())
    ggplotly(p)
  })

  output$continentTimeSeries <- renderPlotly({
    p <- create_continent_time_series(filtered_data())
    ggplotly(p)
  })

  output$continentMap <- renderPlotly({
    p <- create_continent_choropleth(filtered_data())
    ggplotly(p)
  })

  output$countryBarPlot <- renderPlotly({
    p <- create_country_bar(filtered_data(), input$topN)
    ggplotly(p)
  })

  output$countryGenderPlot <- renderPlotly({
    p <- create_country_gender_proportion(filtered_data())
    ggplotly(p)
  })

  output$countryAgePlot <- renderPlotly({
    p <- create_country_age_proportion(filtered_data())
    ggplotly(p)
  })

  output$countryMap <- renderPlotly({
    p <- create_country_choropleth(filtered_data())
    ggplotly(p)
  })

  output$genderOverview <- renderPlotly({
    p <- create_gender_overview(filtered_data(), input$genderOverviewType)
    ggplotly(p)
  })

  output$ageViolinPlot <- renderPlotly({
    p <- create_age_violin_plot(filtered_data())
    ggplotly(p)
  })

  output$gdpScatterPlot <- renderPlotly({
    p <- create_gdp_suicide_scatter(filtered_data())
    ggplotly(p)
  })

  output$downloadPDF <- downloadHandler(
    filename = function() {
      "G1 - Project Delivery - DV.pdf"
    },
    content = function(file) {
      file.copy("G1 - Project Delivery - DV.pdf", file)
    },
    contentType = "application/pdf"
  )

  observe({
    ranges <- get_data_ranges(data)
    
    updateSelectInput(session, "countries",
                     choices = ranges$countries)
    updateSelectInput(session, "continents",
                     choices = ranges$continents)
    updateSelectInput(session, "ageGroups",
                     choices = ranges$age_groups)
    updateSliderInput(session, "gdpRange",
                     min = floor(ranges$gdp_range[1]),
                     max = ceiling(ranges$gdp_range[2]))
  })
}

shinyApp(ui = ui, server = server)
