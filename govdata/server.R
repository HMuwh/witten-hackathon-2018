#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$map <- renderPlot({
    
    sPDF <- joined_2 %>%
      filter(year == input$year) %>%
      joinCountryData2Map(
        joinCode = "ISO3",
        nameJoinColumn = "country_code") %>%
      mapCountryData( 
        nameColumnToPlot = input$indicator,
        # Je nach gewählter Variable!
        mapTitle = input$indicator,
        catMethod = "diverging",
        numCats = 5,
        colourPalette = c("darkred", "red", "yellow", "chartreuse", "chartreuse4"),
        oceanCol = "aliceblue",
        missingCountryCol = "white")
    
    
  })
  output$chart_1 <- renderPlot({
    
    cat("here")
    
    joined_2 %>%
      filter(country == input$country) %>%
      select(date, value = matches(input$indicator)) %>%
      ggplot() +
        aes(date, value) + 
        geom_line() +
        ggtitle(paste(input$indicator, "in", input$country)) +
        xlab("") +
        ylab(input$indicator) +
        ylim(-3.5, 2.5) +
        theme_minimal()
    
  })
  
  
})
