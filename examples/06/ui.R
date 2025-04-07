source("../../utils.R")

library(shiny)
library(cookies)

ui <- add_cookie_handlers(
  fluidPage(
    includeCSS(path = "../styles.css"),
    wellPanel(
      class = "custom-well-panel",
      fluidRow(
        column(
          width = 2,
          offset = 1,
          numericInput(
            inputId = "duration",
            label = "Duration (min)",
            min = 0,
            step = 1,
            value = 60
          )
        ),
        column(
          width = 2,
          numericInput(
            inputId = "weight",
            label = "Weight (kg)",
            min = 0,
            step = 1,
            value = 90
          )
        ),
        column(
          width = 2,
          numericInput(
            inputId = "age",
            label = "Age",
            min = 18,
            step = 1,
            value = 33
          )
        ),
        column(
          width = 2,
          selectInput(
            inputId = "gender",
            label = "Gender",
            choices = c("Male", "Female"),
            selected = "Male"
          )
        ),
        column(
          width = 2,
          selectInput(
            inputId = "activity",
            label = "Activity",
            choices = c(
              "Cycling",
              "Football",
              "Running",
              "Swimming",
              "Tennis",
              "Volleyball",
              "Walking"
            ),
            selected = "Tennis"
          )
        )
      )
    ),
    wellPanel(
      class = "custom-well-panel",
      uiOutput(outputId = "message")
    )
  )
)
