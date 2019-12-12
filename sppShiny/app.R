#
# This is a Shiny web application. 
#
# It includes visualizations of the stops made in California across time of the day and 
#

library(shiny)
library(leaflet)
library(ggplot2)

# Define UI ----
ui <- fluidPage(
  titlePanel("Traffic Stops in Californian Cities"),
  mainPanel( 
    #this will create a space for us to display our map
    leafletOutput(outputId = "mymap"), 
    #this allows me to put the checkmarks ontop of the map to allow people to view earthquake depth or overlay a heatmap
    absolutePanel(top = 60, left = 20, 
                  checkboxInput("markers", "Depth", FALSE),
                  checkboxInput("heat", "Heatmap", FALSE)
    )
  ))


# Define server logic ----
server <- function(input, output, session) {
  
  
  output$mymap <- renderLeaflet({
    leaflet(data) %>%
      addPolygons(
        label=~label, labelOptions = labelOptions(),
        popup=~popup, popupOptions = popupOptions(),
        # Shape Options
        options = pathOptions(),
        weight = 1, opacity=0.8, color = "#000000",
        fillColor="#ff0000", fillOpacity=0.7,
        # Highlighting on mouse-over
        highlightOptions = highlightOptions(
          color='#00ff00', weight = 2,
          opacity = 1, fillOpacity = 1,
          bringToFront = TRUE, sendToBack = TRUE),
        group = 'Group-A')
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)