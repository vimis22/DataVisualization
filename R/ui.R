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
                  div(
                    style = "height:600px; overflow-y: scroll;",
                    plotlyOutput("countryBarPlot")
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