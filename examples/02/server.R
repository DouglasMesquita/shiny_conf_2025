# What to test?
#
# Second scenario: retrieving adjusted MET in a reactive
# Try changing the age, gender, and activity inputs — it will trigger all computations repeatedly!
#
# Why is this a problem?
# It's suboptimal for several reasons:
# - Changing age should not affect gender or MET factors.
# - Changing gender should not affect age or MET factors.
# - Changing activity should not affect age or gender factors.
server <- function(input, output, session) {
  # calculate adjusted MET in advance
  adjusted_MET <- reactive({
    # collect inputs
    age <- input$age
    gender <- input$gender
    activity <- input$activity

    # collect factors
    age_factor <- get_age_factor(age, with_delay = with_delay)
    gender_factor <- get_gender_factor(gender, with_delay = with_delay)
    MET_factor <- get_MET_factor(activity, with_delay = with_delay)

    # calculate factor
    age_factor * gender_factor * MET_factor
  })

  # output message
  output$message <- renderUI({
    # start time
    start <- Sys.time()
    message(glue(">>> Creating the output message <<<"))

    # collect inputs
    duration <- input$duration
    weight <- input$weight
    MET <- adjusted_MET()

    # compute metric
    calories <- calculate_calories_burn(
      duration = duration,
      weight = weight,
      adjusted_MET = MET,
      with_delay = with_delay
    )

    # Calculate the time spent
    total_time <- Sys.time() - start
    message(glue(">>> Creating the output message took {round(total_time, 2)} seconds <<<"))

    # output message
    span(
      glue("Based on the current setup, you have burned {calories} calories."),
      class = "output-text"
    )
  })
}
