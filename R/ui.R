ui <- dashboardPage(
  dashboardHeader(title = "Global Suicide Rates Analysis"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("dashboard")),
      menuItem("Geographic Analysis", tabName = "geographic", icon = icon("globe")),
      menuItem("Demographics", tabName = "demographics", icon = icon("users")),
      menuItem("Economic Factors", tabName = "economic", icon = icon("chart-line")),
      
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
                  multiple = TRUE),
      sliderInput("gdpRange", "GDP per Capita Range:",
                  min = 0, max = 100000,
                  value = c(0, 100000))
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
                         selected = "line", inline = TRUE),
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
              box(width = 4,
                  title = "By Gender",
                  plotlyOutput("continentGenderPlot")
              ),
              box(width = 4,
                  title = "By Age",
                  plotlyOutput("continentAgePlot")
              ),
              box(width = 4,
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
                            min = 5, max = 50, value = 20),
                  plotlyOutput("countryBarPlot")
              ),
              box(width = 6,
                  title = "Gender Distribution",
                  plotlyOutput("countryGenderPlot")
              )
            ),
            fluidRow(
              box(width = 6,
                  title = "Age Distribution",
                  plotlyOutput("countryAgePlot")
              ),
              box(width = 6,
                  title = "Country Trends",
                  plotlyOutput("countryTrends")
              )
            )
          )
        )
      ),

      tabItem(tabName = "demographics",
        tabBox(width = 12,
          tabPanel("Gender Analysis",
            fluidRow(
              box(width = 6,
                  title = "Gender Time Series",
                  plotlyOutput("genderTimeSeries")
              ),
              box(width = 6,
                  title = "Gender Disparity Evolution",
                  plotlyOutput("genderDisparityEvolution")
              )
            ),
            fluidRow(
              box(width = 12,
                  title = "Gender Disparity by Country",
                  plotlyOutput("genderDisparityPlot")
              )
            )
          ),
          tabPanel("Age Analysis",
            fluidRow(
              box(width = 6,
                  title = "Age Distribution",
                  plotlyOutput("ageViolinPlot")
              ),
              box(width = 6,
                  title = "Age Trends",
                  imageOutput("animatedAgeTrends")
              )
            ),
            fluidRow(
              box(width = 12,
                  title = "Age Evolution Heatmap",
                  plotlyOutput("ageHeatmap")
              )
            )
          )
        )
      ),

      tabItem(tabName = "economic",
        fluidRow(
          box(width = 12,
              title = "GDP vs Suicide Rate Relationship",
              plotlyOutput("gdpScatterPlot")
          )
        ),
        fluidRow(
          box(width = 6,
              title = "GDP Evolution",
              imageOutput("animatedGdpPlot")
          ),
          box(width = 6,
              title = "GDP Correlation by Continent",
              plotlyOutput("gdpCorrelationPlot")
          )
        ),
        fluidRow(
          box(width = 12,
              title = "GDP Quantile Analysis",
              plotlyOutput("gdpQuantilePlot")
          )
        )
      )
    )
  )
)