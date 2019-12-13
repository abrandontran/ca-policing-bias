#
# This is a Shiny web application. 
#
# It includes visualizations of the stops made in California across time of the day and 
#

library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(tidyverse)

  #Generating data sets here


#Define UI ----
ui <- fluidPage(
  titlePanel("In the Dark: Exploring Racial Disparities in Traffic Stops Before and After Sunset"),

  sidebarLayout(
    sidebarPanel(selectInput("choose_city", "Choose City", choices = c("sf", "ok", "sj", "bf", "la", "sd"))),
    mainPanel(plotOutput("piechart")))


) #end of Ui

# ui <- fluidPage(
#   selectInput("choose_city", "Choose City", choices = c("sf", "ok", "sj", "bf", "la", "sd")),
#   plotOutput("piechart")
# )

# Define server logic ----
server <- function(input, output) {
  
  output$piechart <- renderPlot({
    
    stops.day <- ca.df %>% filter(city == input$choose_city, daytime == "TRUE") %>%
      group_by(subject_race) %>%
      tally() %>%
      mutate(proportion = n/ sum(n)) %>%
      mutate_at(vars(proportion), funs(round(., 3))) 
    
    ggplot(stops.day, aes(x = "", y = proportion, fill = as.character(subject_race)))+
      geom_bar(width = 1, stat = "identity", color = "white") +
      coord_polar("y", start = 0)+
      theme_minimal() + 
      labs(title = "Daytime stops in San Francisco")+
      labs(fill = "Race")+
      scale_fill_manual(values = c("deepskyblue3","deeppink3", "olivedrab3", "darkorange2", "firebrick3"),
                        labels = c("asian/pacific islander", "black", "hispanic", "white", "other"))
   

    
  })
  
  }


# Run the app ----
shinyApp(ui = ui, server = server)
