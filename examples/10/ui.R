source("../../utils.R")

library(shiny)
library(shinyjs)
library(cookies)

ui <- add_cookie_handlers(
  fluidPage(
    includeCSS(path = "../styles.css"),
    useShinyjs(),
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
            value = NULL
          ),
          hidden(
            span(
              id = "duration_msg",
              class = "cannot-be-empty",
              cannot_be_empty
            )
          )
        ),
        column(
          width = 2,
          numericInput(
            inputId = "weight",
            label = "Weight (kg)",
            min = 0,
            step = 1,
            value = NULL
          ),
          hidden(
            span(
              id = "weight_msg",
              class = "cannot-be-empty",
              cannot_be_empty
            )
          )
        ),
        column(
          width = 2,
          numericInput(
            inputId = "age",
            label = "Age",
            min = 18,
            step = 1,
            value = NULL
          ),
          hidden(
            span(
              id = "age_msg",
              class = "cannot-be-empty",
              cannot_be_empty
            )
          ),
          hidden(
            span(
              id = "age_msg_invalid",
              class = "cannot-be-empty",
              invalid_age
            )
          )
        ),
        column(
          width = 2,
          selectInput(
            inputId = "gender",
            label = "Gender",
            choices = c("", "Male", "Female"),
            selected = ""
          ),
          hidden(
            span(
              id = "gender_msg",
              class = "cannot-be-empty",
              cannot_be_empty
            )
          )
        ),
        column(
          width = 2,
          selectInput(
            inputId = "activity",
            label = "Activity",
            choices = c(
              "",
              "Cycling",
              "Football",
              "Running",
              "Swimming",
              "Tennis",
              "Volleyball",
              "Walking"
            ),
            selected = ""
          ),
          hidden(
            span(
              id = "activity_msg",
              class = "cannot-be-empty",
              cannot_be_empty
            )
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
