# What to test?
#
# Sixth scenario: using cookies and observeEvent options
# - Set `ignoreNULL = TRUE` to prevent the observer from triggering when
#   age (category) is NULL - no notification in this case.
# - Set `once = FALSE` to see how this argument controls how many times
#   the observer is triggered.
#
# Why is this problematic?
# When something is already stored in the cookies, the app runs twice
# upon initial load. The reactive chain gets triggered before we
# properly handle the cookies!
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
    # once = FALSE
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
  }, ignoreNULL = FALSE) # ignoreNULL = TRUE
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

    # cookies: save selection
    isolate(
      save_cookies(input_names, input)
    )

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
