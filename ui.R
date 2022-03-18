### BILKOLLEKTIVET COST BREAKDOWN (UI) ########################

#-- Libraries -------------------------

library(tidyverse)
library(shiny)
library(shinyWidgets)
library(gt)

#-- Define UI -------------------------

shinyUI(fluidPage(
    theme = "style.css",
    
    # Application title
    titlePanel(title = "Bilkollektivet trip cost", 
               windowTitle = "Cost Bilkollektivet"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            tags$style(".well {background-color: transparent;border: none; box-shadow: none}"),
            chooseSliderSkin("Flat", color = "#8A29BE"),
            sliderInput(inputId = "dist",
                        label = "Estimated distance:",
                        min = 10,
                        max = 2000,
                        value = 250, 
                        step = 10,
                        ticks = FALSE),
            sliderInput(inputId = "n_days",
                        label = "Number of days:",
                        min = 0,
                        max = 14,
                        value = 2,
                        ticks = FALSE),
            sliderInput(inputId = "n_hours",
                        label = "Number of hours:",
                        min = 0,
                        max = 7,
                        value = 0,
                        ticks = FALSE),
            sliderInput(inputId = "n_pass",
                        label = "Number of passengers (incl. driver):",
                        min = 1,
                        max = 9,
                        value = 1,
                        ticks = FALSE),
            radioGroupButtons(inputId = "cartype",
                              label = "Select car type:", 
                              choices = c("Liten elbil", #= elbil_liten
                                          "Småbil", #= "smabil",
                                          "Mellomklasse elbil", #= "elbil_mellom",
                                          "Stasjonsvogn", #= "stasjonsvogn",
                                          "Sportsbil", #= "sportsbil",
                                          "Tesla Model 3", #= "elbil",
                                          "Tesla Model Y", #= "elbil_premium",
                                          #"Jaguar Ipace", #= "elbil_premium",
                                          #"7-seter", #= "7-seter",
                                          "SUV 4x4", #= "suv",
                                          "El-varebil", #= "varebil_el",
                                          "9-seter", #= "9-seter",
                                          "Varebil" #= "varebil",
                              ),
                              checkIcon = list(yes = icon("check")), 
                              justified = FALSE,
                              width = "300px",
                              #selected = "Mellomklasse elbil", 
                              #choicesOpt = list(
                              #    content = sprintf("<span class='label label-%s'>%s</span>", 
                              #                      c("success", "success",
                              #                        "success", "success", 
                              #                        "primary","primary","primary",
                              #                        "primary", "primary", "primary",
                              #                        "info","info"), 
                              #                      c("Mellomklasse elbil", "Premium elbil",
                              #                        "Elektrisk lastesykkel","Elektrisk varebil",
                              #                        "Småbil", "Mellomklasse", "Stasjonsvogn",
                              #                        "7-seter", "SUV 4x4", "9-seter", 
                              #                        "Varebil", "Stor varebil")))
            ),
            checkboxInput(inputId = "insurance",
                          label = "Additional insurance",
                          value = FALSE)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            span(h1(textOutput("title")), style = "text-indent: 10%"),
            gt_output("baseprices"),
            br(),
            gt_output("breakdown"),
            br(),
            gt_output("per_passenger"),
            br()
        )
    )
))
