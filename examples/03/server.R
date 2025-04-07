# What to test?
#
# Third scenario: separate reactives - one for each heavy task
# Play with the inputs. We can still improve the behavior for age.
#
# Why is this a problem?
# Even though age is a numeric input, the age factor is categorical - it only
# changes within certain intervals. It would be better if the reactive triggered
# only when the age interval changes, not on every numeric change.
server <- function(input, output, session) {
  # calculate individual factors in advance
  age_factor_reac <- reactive({
    age <- input$age
    get_age_factor(age, with_delay = with_delay)
  })
  gender_factor_reac <- reactive({
    gender <- input$gender
    get_gender_factor(gender, with_delay = with_delay)
  })
  MET_factor_reac <- reactive({
    activity <- input$activity
    get_MET_factor(activity, with_delay = with_delay)
  })

  # calculate adjusted MET in advance
  adjusted_MET <- reactive({
    age_factor <- age_factor_reac()
    gender_factor <- gender_factor_reac()
    MET_factor <- MET_factor_reac()

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
