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


# Define UI ----
ui <- fluidPage(
  titlePanel("In the Dark: Exploring Racial Disparities in Traffic Stops Before and After Sunset"),
  
  sidebarLayout(
    sidebarPanel(selectInput("choose_city", label = h5("Choose City"),  #the choose city widget
                              choices = list("sf" = 1, "la" = 2,
                                "sd" = 3, "bf" = 4, "sj" = 5, "ok" = 6), selected = 1)
      
               ),
    mainPanel( plotOutput("piechart")
              )
  )
  
  
) #end of Ui

# Define server logic ----
server <- function(input, output) {
  
  output$piechart <- renderPlot({
    
    stops.day.sf <- ca.df %>% filter(city == "sf", daytime == "TRUE") %>%
      group_by(subject_race) %>%
      tally() %>%
      mutate(proportion = n/ sum(n)) %>%
      mutate_at(vars(proportion), funs(round(., 3))) 
    
    stops.day.la <- ca.df %>% filter(city == "la", daytime == "TRUE") %>%
      group_by(subject_race) %>%
      tally() %>%
      mutate(proportion = n/ sum(n)) %>%
      mutate_at(vars(proportion), funs(round(., 3))) 
    
    data <- switch(input$choose_city, 
                   "sf" = stops.day.sf,
                   "la" = stops.day.la,
                   "sd" = stops.day.sd,
                   "sj" = stops.day.sj,
                   "bf" = stops.day.bf,
                   "ok" = stops.day.ok)
    
    ggplot(data)+
      geom_bar(aes(x = "", y = proportion, fill = subject_race), width = 1, stat = "identity", color = "white") +
      coord_polar("y", start = 0)+
      theme_minimal() + 
      labs(title = "Daytime Stops by Race")+
      labs(fill = "Race")+
      scale_fill_manual(values = c("deepskyblue3","deeppink3", "olivedrab3", "darkorange2", "firebrick3"),
                        labels = c("asian/pacific islander", "black", "hispanic", "white", "other"))
  })
  
  }


# Run the app ----
shinyApp(ui = ui, server = server)
