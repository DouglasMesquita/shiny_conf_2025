# What to test?
#
# Seventh scenario: using `isolate()` in the message
# - Uncomment the `isolate()` block and observe that it prevents unnecessary reactivity
#
# Why is it bad?
# We added a button to control reactivity, but this behavior was not requested by the client.
# If the user deletes information from the input widgets and presses "Calculate",
# errors appear in the console and no messages are shown on the screen.
#
# If `remove_cookies` is set to TRUE, the app will intentionally crash and
# all cookies will be cleared. Run once with TRUE to test the behavior,
# then switch it back to FALSE.
remove_cookies <- FALSE
server <- function(input, output, session) {
  # initialize inputs
  observeEvent(
    {
      input$duration
      input$weight
      input$age
      input$gender
      input$activity
    }, {
      # display message
      message("--- Reading from cookies")

      # collect values from cookies
      if (remove_cookies) { remove_cookies(input_names, session); stop() }
      selected_values <- get_cookies(input_names)

      # update widgets
      update_widgets(selected_values, session)
    },
    once = TRUE
  )

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
    # handling NULL
    if (is.null(age_category())) {
      showNotification("Calculation applicable only for individuals aged 18 and above.", duration = 10)
      return(NULL)
    }

    # even though I am using age, the reactive will only trigger when
    # age_category() changes
    age <- input$age
    age_factor <- get_age_factor(age, with_delay = with_delay)

    return(age_factor)
  }, ignoreNULL = FALSE)
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

  # calculate calories
  calories_reac <- reactive({
    # collect inputs
    duration <- input$duration
    weight <- input$weight
    MET <- adjusted_MET()

    # compute metric
    calculate_calories_burn(
      duration = duration,
      weight = weight,
      adjusted_MET = MET,
      with_delay = with_delay
    )
  })

  # output message
  output$message <- renderUI({
    # adding dependency on the actionButton
    # returning NULL in case the button was never clicked
    if (input$calculate_calories == 0) return(NULL)

    # start time
    start <- Sys.time()
    message(glue(">>> Creating the output message <<<"))

    # cookies: save selection
    isolate(
      save_cookies(input_names, input)
    )

    # collect metric and inputs
    duration <- isolate(input$duration)
    weight <- isolate(input$weight)
    age <- isolate(input$age)
    gender <- isolate(input$gender)
    activity <- isolate(input$activity)

    # compute metric
    calories <- isolate(calories_reac())

    # >>> uncomment this chunk
    # duration <- input$duration
    # weight <- input$weight
    # age <- input$age
    # gender <- input$gender
    # activity <- input$activity

    # calories <- calories_reac()
    # <<< uncomment this chunk

    # Calculate the time spent
    total_time <- Sys.time() - start
    message(glue(">>> Creating the output message took {round(total_time, 2)} seconds <<<"))

    # output message
    span(
      glue(
        "For a {gender} aged {age}, doing {activity} for {duration} minutes
            at a weight of {weight} kg, the estimated calorie burn is {calories} kcal."
      ),
      class = "output-text"
    )
  })
}
