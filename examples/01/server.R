# What to test?
#
# First scenario: only one chunk (output)
# Try changing the inputs - it will trigger all computations again and again!
#
# Why is this a problem?
# It's suboptimal for several reasons:
# - Changing duration should not affect age, gender, or MET factors.
# - Changing weight should not affect age, gender, or MET factors.
# - Changing age should not affect gender or MET factors.
# - Changing gender should not affect age or MET factors.
# - Changing the activity should not affect age or gender factors.
server <- function(input, output, session) {
  # output message
  output$message <- renderUI({
    # start time
    start <- Sys.time()
    message(glue(">>> Creating the output message <<<"))

    # collect inputs
    duration <- input$duration
    weight <- input$weight
    age <- input$age
    gender <- input$gender
    activity <- input$activity

    # collect factors
    age_factor <- get_age_factor(age, with_delay = with_delay)
    gender_factor <- get_gender_factor(gender, with_delay = with_delay)
    MET_factor <- get_MET_factor(activity, with_delay = with_delay)

    # compute metric
    calories <- calculate_calories_burn(
      duration = duration,
      weight = weight,
      age_factor = age_factor,
      gender_factor = gender_factor,
      MET_factor = MET_factor,
      with_delay = with_delay
    )

    # Calculate the time spent
    total_time <- Sys.time() - start

    # output message
    message(glue(">>> Creating the output message took {round(total_time, 2)} seconds <<<"))
    span(
      glue("Based on the current setup, you have burned {calories} calories."),
      class = "output-text"
    )
  })
}
