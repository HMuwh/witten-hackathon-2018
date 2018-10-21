library(shinydashboard)
library(shiny)

indi_choices <- c("control_of_corruption", "government_effectiveness", "political_stability", "rule_of_law", "regulatory_quality", "voice_and_accountability")
country_choices <- unique(joined_2$country)

dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Map", tabName = "maptab", icon = icon("globe")),
      menuItem("Chart", tabName = "chart", icon = icon("dashboard"),
#      menuItem("Spider Chart", tabName = "spider_chart", icon = icon("dashboard")),
               badgeLabel = "new", badgeColor = "green")
    ),
    sliderInput("year", "Year", min = 1996, max = 2016, step = 2, value = 2016, animate = TRUE),
    selectInput("indicator", "Indicator", choices = indi_choices),
    selectInput("country", "Country", choices = country_choices)
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "maptab",
              h2("Map content"),
              plotOutput("map")
      ),
      
      tabItem(tabName = "chart",
              h2("Chart content"),
              plotOutput("chart_1")
      )
      
#      tabItem(tabName = "spider_chart",
#              h2("Spider Chart content"),
#              plotOutput("chart_1")
#      )
    
  )
)
)

