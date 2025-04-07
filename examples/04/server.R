# What to test?
#
# Fourth scenario: using a reactive to get the age category
# Our attempt didn’t work, as the reactive is still triggering even when its
# returned value hasn’t changed.
#
# Why is this a problem?
# The reactive still triggers downstream computations, even though it's returning
# the exact same value — which leads to unnecessary updates.
server <- function(input, output, session) {
  # collecting age category. e.g: [18-30)
  age_category <- reactive({
    age <- input$age
    return(get_age_range(age))
  })

  # calculate individual factors in advance
  # observe age_category instead of age
  age_factor_reac <- eventReactive(age_category(), {
    # even though I am using age, the reactive will only trigger when
    # age_category() changes
    age <- input$age
    age_factor <- get_age_factor(age, with_delay = with_delay)

    return(age_factor)
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
