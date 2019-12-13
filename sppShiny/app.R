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
library(cowplot)

  #Generating data sets here


#Define UI ----
ui <- fluidPage(
  titlePanel("In the Dark: Exploring Racial Disparities in Traffic Stops Before and After Sunset"),

  sidebarLayout(
    sidebarPanel(selectInput("choose_city", "Choose City", choices = c("San Francisco", "Oakland", "San Jose", "Bakersfield", 
                                                                       "Los Angeles", "San Diego"))),
    mainPanel(
      plotOutput("daypie")))


) #end of Ui

# ui <- fluidPage(
#   selectInput("choose_city", "Choose City", choices = c("sf", "ok", "sj", "bf", "la", "sd")),
#   plotOutput("piechart")
# )

# Define server logic ----
server <- function(input, output) {
  
  output$daypie <- renderPlot({
    
    select_city <- switch (input$choose_city,
      "San Francisco" = "sf",
      "Oakland" = "ok",
      "Los Angeles" = "la",
      "San Jose" = "sj",
      "San Diego" = "sd",
      "Bakersfield" = "bf")
    
    stops.day <- ca.df %>% filter(city == select_city, daytime == "TRUE") %>%
      group_by(subject_race) %>%
      tally() %>%
      mutate(proportion = n/ sum(n)) %>%
      mutate_at(vars(proportion), funs(round(., 3))) 
    
    day.plot <- ggplot(stops.day, aes(x = "", y = proportion, fill = as.character(subject_race)))+
      geom_bar(width = 1, stat = "identity", color = "white") +
      coord_polar("y", start = 0)+
      theme_minimal() + 
      labs(title = "Daytime stops: Racial break-up")+
      labs(fill = "Race")+
      scale_fill_manual(values = c("deepskyblue3","deeppink3", "olivedrab3", "darkorange2", "firebrick3"),
                        labels = c("asian/pacific islander", "black", "hispanic", "white", "other"))
   
    stops.night <- ca.df %>% filter(city == select_city, daytime == "FALSE") %>%
      group_by(subject_race) %>%
      tally() %>%
      mutate(proportion = n/ sum(n)) %>%
      mutate_at(vars(proportion), funs(round(., 3))) 
    
    night.plot <- ggplot(stops.night, aes(x = "", y = proportion, fill = subject_race))+
      geom_bar(width = 1, stat = "identity", color = "white") +
      coord_polar("y", start = 0)+
      theme_minimal() + 
      labs(title = "Nightime stops in San Francisco")+
      labs(fill = "Race")+
      scale_fill_manual(values = c("deepskyblue3","deeppink3", "olivedrab3", "darkorange2", "firebrick3"),
                        labels = c("asian/pacific islander", "black", "hispanic", "white", "other"))
    
    plot_grid(day.plot, night.plot)
   
  })
  
  
  }


# Run the app ----
shinyApp(ui = ui, server = server)
