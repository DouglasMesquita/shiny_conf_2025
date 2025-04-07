# What to test?
#
# Fifth scenario: using observeEvent + reactiveVal
# Now, the message updates only when the age category actually changes.
#
# Why is this still not ideal?
# Gender, age category, and activity have only a few possible values.
# So… what if we cache the results?
# But first - let’s talk with the client
server <- function(input, output, session) {
  # collecting age category. e.g: [18-30)
  age_category <- reactiveVal(NULL)
  observeEvent(input$age, {
    age <- input$age
    if (isTruthy(age)) {
      age_cat <- get_age_range(age)

      # update reactiveVal
      age_category(age_cat)
    } else {
      age_category(NULL)
    }
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
