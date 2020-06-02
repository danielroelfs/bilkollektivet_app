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
    headerPanel("Bilkollektivet trip cost"),
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            chooseSliderSkin("Flat", color = "#8A29BE"),
            sliderInput(inputId = "dist",
                        label = "Estimated distance:",
                        min = 10,
                        max = 1200,
                        value = 250, 
                        step = 10,
                        ticks = FALSE),
            sliderInput(inputId = "n_days",
                        label = "Number of days:",
                        min = 1,
                        max = 14,
                        value = 2,
                        ticks = FALSE),
            sliderInput(inputId = "n_hours",
                        label = "Number of hours:",
                        min = 0,
                        max = 7,
                        value = 0,
                        ticks = FALSE),
            radioGroupButtons(inputId = "cartype",
                label = "Select car type:", 
                choices = c("Mellomklasse elbil" ,#= "elbil_mellom",
                            "Premium elbil" ,#= "elbil_premium",
                            "Elektrisk lastesykkel" ,#= "elsykkel",
                            "Elektrisk varebil" ,#= "varebil_el",
                            "Småbil" ,#= "smabil",
                            "Mellomklasse" ,#= "mellomklasse",
                            "Stasjonsvogn" ,#= "stasjonsvogn",
                            "7-seter" ,#= "7-seter",
                            "SUV 4x4" ,#= "suv",
                            "9-seter" ,#= "9-seter",
                            "Varebil" ,#= "varebil",
                            "Stor varebil"#= "varebil_stor"))
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
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            gt_output("baseprices"),
            br(),
            hr(),
            br(),
            gt_output("breakdown")
        )
    )
))
