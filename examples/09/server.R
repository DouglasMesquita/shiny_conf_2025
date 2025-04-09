# What to test?
#
# Ninth scenario: Removing the action button
# - Use `cookies_are_loaded()` instead of `cookies_are_loaded_d()`
# - Initialize `age_category` as `reactiveVal(NULL)` and remove the `ignoreInit` argument
#
# Why is it important?
# - This approach removes the need for an action button, aligning with the client’s request
# - It ensures that reactivity is only triggered after cookies are fully loaded,
# avoiding unnecessary intermediate updates
#
# If `remove_cookies` is set to TRUE, the app will intentionally crash and
# all cookies will be cleared. Run once with TRUE to test the behavior,
# then switch it back to FALSE.
remove_cookies <- FALSE
server <- function(input, output, session) {
  # initialize inputs
  cookies_are_loaded <- reactiveVal(FALSE)
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
      if (remove_cookies) { remove_cookies(input_names, session); stopApp() }
      selected_values <- get_cookies(input_names)

      # update widgets
      update_widgets(selected_values, session)

      # update cookies_are_loaded state
      cookies_are_loaded(TRUE)
    },
    once = TRUE
  )

  # debounce cookies_are_loaded() so we can wait the outputs to be updated
  # before computing the number of calories
  cookies_are_loaded_d <- debounce(reactive(cookies_are_loaded()), millis = 500)

  # updating notes
  observeEvent({
    input$duration
    input$weight
    input$age
    input$gender
    input$activity
  }, {
    # display message
    message("--- Updating notes")

    show_hide_input_msgs(input_names, input)
  },
  # we do not care about the first update. We know it has only valid choices
  ignoreInit = TRUE
  )

  # collecting age category. e.g: [18-30)
  # age_category <- reactiveVal(NULL)
  age_category <- reactiveVal("[30-40)")
  observeEvent(input$age, {
    age <- input$age
    if (isTruthy(age)) {
      age_cat <- get_age_range(age)

      # update reactiveVal
      age_category(age_cat)
    } else {
      age_category(NULL)
    }
  }, ignoreInit = TRUE) # We already know the first value for age_category

  # calculate individual factors in advance
  # observe age_category instead of age
  age_factor_reac <- eventReactive(age_category(), {
    # requirements
    req(age_category())

    # even though I am using age, the reactive will only trigger when
    # age_category() changes
    age <- input$age
    age_factor <- get_age_factor(age, with_delay = with_delay)

    return(age_factor)
  })
  gender_factor_reac <- reactive({
    gender <- input$gender

    # requirements
    req(gender)

    get_gender_factor(gender, with_delay = with_delay)
  })
  MET_factor_reac <- reactive({
    activity <- input$activity

    # requirements
    req(activity)

    get_MET_factor(activity, with_delay = with_delay)
  })

  # calculate adjusted MET in advance
  adjusted_MET <- reactive({
    age_factor <- age_factor_reac()
    gender_factor <- gender_factor_reac()
    MET_factor <- MET_factor_reac()

    # requirements
    req(age_factor, gender_factor, MET_factor)

    # calculate factor
    age_factor * gender_factor * MET_factor
  })

  # calculate calories
  calories_reac <- reactive({
    # collect inputs
    duration <- input$duration
    weight <- input$weight
    MET <- adjusted_MET()

    # requirements
    req(duration, weight, MET)

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
    # requirements
    req(cookies_are_loaded_d())

    # collect metric and inputs
    duration <- input$duration
    weight <- input$weight
    age <- input$age
    gender <- input$gender
    activity <- input$activity

    # return error message in case any of the inputs are not set
    # it will also avoid calories calculation
    valid_inputs <- isolate(
      validate_inputs(input_names, input)
    )

    if (!valid_inputs) {
      message(glue(">>> Invalid inputs <<<"))

      return(
        span(
          "Please provide a valid set of parameters",
          class = "output-text"
        )
      )
    }

    # start time
    start <- Sys.time()
    message(glue(">>> Creating the output message <<<"))

    # cookies: save selection
    isolate(
      save_cookies(input_names, input)
    )

    # compute metric
    calories <- calories_reac()

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
