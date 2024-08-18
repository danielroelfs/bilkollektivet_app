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
  titlePanel(
    title = "Bilkollektivet trip cost calculator",
    windowTitle = "Bilkollektivet Calculator"
  ),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      tags$style(".well {background-color: transparent;border: none; box-shadow: none}"),
      chooseSliderSkin("Flat", color = "#8A29BE"),
      sliderInput(
        inputId = "dist",
        label = "Estimated distance:",
        min = 0,
        max = 2000,
        value = 250,
        step = 10,
        ticks = FALSE
      ),
      sliderInput(
        inputId = "n_days",
        label = "Number of days:",
        min = 0,
        max = 14,
        value = 2,
        ticks = FALSE
      ),
      sliderInput(
        inputId = "n_hours",
        label = "Number of hours:",
        min = 0,
        max = 7,
        value = 0,
        ticks = FALSE
      ),
      sliderInput(
        inputId = "n_pass",
        label = "Number of passengers (incl. driver):",
        min = 1,
        max = 9,
        value = 1,
        ticks = FALSE
      ),
      radioGroupButtons(
        inputId = "car",
        label = "Select car type:",
        choices = c(
          "Toyota Yaris (Budsjettklasse)",
          "Opel Corsa-e",
          "Toyota Yaris",
          "Mazda MX5",
          "Toyota Yaris Cross",
          "MG ZS",
          "Toyota Corolla",
          "Tesla Model 3",
          "Tesla Model Y",
          "Corolla Cross",
          "Toyota Proace",
          "Toyota Rav4",
          "Toyota Proace EL",
          "Toyota Proace Verso"
        ),
        checkIcon = list(yes = icon("check")),
        justified = FALSE,
        width = "300px",
      ),
      checkboxInput(
        inputId = "insurance",
        label = "Additional insurance",
        value = TRUE
      )
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
