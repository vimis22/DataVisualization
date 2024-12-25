ui <- dashboardPage(
  dashboardHeader(title = "Global Suicide Rates Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Temporal Analysis", tabName = "temporal", icon = icon("chart-line")),
      menuItem("Geographic Analysis", tabName = "geographic", icon = icon("globe")),
      menuItem("Demographics", tabName = "demographics", icon = icon("users")),
      menuItem("Economic Factors", tabName = "economic", icon = icon("money-bill")),
      
      hr(),
      sliderInput("yearRange", "Select Year Range:",
                  min = 1985, max = 2016,
                  value = c(1985, 2016),
                  step = 1,
                  sep = ""),
      selectInput("countries", "Select Countries:",
                  choices = NULL,
                  multiple = TRUE),
      selectInput("ageGroups", "Select Age Groups:",
                  choices = NULL,
                  multiple = TRUE),
      hr(),
      helpText("Generation Ranges:",
               HTML("<br>G.I. Generation: 1901-1927",
                    "<br>Silent: 1928-1945",
                    "<br>Boomers: 1946-1964",
                    "<br>Generation X: 1965-1980",
                    "<br>Millennials: 1981-1996",
                    "<br>Generation Z: 1997-2012"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "overview",
            fluidRow(
                box(plotlyOutput("timeSeriesPlot"), width = 12,
                    title = "Suicide Rates Over Time")
              ),
              fluidRow(
                box(plotlyOutput("topCountriesPlot"), width = 6,
                    title = "Top Countries by Suicide Rate"),
                box(plotlyOutput("ageDistributionPlot"), width = 6,
                    title = "Distribution by Age Group")
              )
      ),

    tabItem(tabName = "temporal",
            fluidRow(
                    box(plotlyOutput("timeSeriesPlot"), width = 12)
                ),
                fluidRow(
                    box(plotlyOutput("ageEvolutionPlot"), width = 6),
                    box(plotlyOutput("rateChangePlot"), width = 6)
                )
      ),
        tabItem(tabName = "geographic",
            fluidRow(
            box(plotlyOutput("regionHeatmap"), width = 12)
            )
      ),
      
      tabItem(tabName = "demographics",
              fluidRow(
                box(plotlyOutput("generationTrends"), width = 12,
                    title = "Suicide Rates Across Generations")
              )
      ),
      
      tabItem(tabName = "economic",
              fluidRow(
                box(plotlyOutput("gdpScatter"), width = 12,
                    title = "GDP vs Suicide Rate")
              ),
              fluidRow(
                box(plotOutput("gdpAnimation"), width = 12,
                    title = "Economic Development Over Time")
              ),
                fluidRow(
                box(plotlyOutput("gdpPopulationPlot"), width = 12)
              )
      )
    )
  )
)