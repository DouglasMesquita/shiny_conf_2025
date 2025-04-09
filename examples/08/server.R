# What to test?
#
# Eighth scenario: using `req()` to avoid unnecessary calculations
# - Set valid_inputs <- validate_inputs(input_names, input) to show that (remove)
#   functions can be affected by the reactivity
# - Remove some `req()` statements and observe what happens
# - Change the age category and reload the app - this part of the code
#   triggers multiple times. Why?
#
# Why is it bad?
# - We still have an action button, but the client explicitly requested to remove it
# - We shouldn’t trigger `age_category` twice on app startup, even when
#   the cookie value differs from the default
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
      if (remove_cookies) { remove_cookies(input_names, session); stopApp() }
      selected_values <- get_cookies(input_names)

      # update widgets
      update_widgets(selected_values, session)
    },
    once = TRUE
  )

  # updating notes
  observeEvent(
    {
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
  observeEvent(input$calculate_calories, {
    # adding dependency on the actionButton
    # returning NULL in case the button was never clicked
    if (input$calculate_calories == 0) return(NULL)

    output$message <- renderUI({
      req(input$calculate_calories)

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

      # return error message in case any of the inputs are not set
      # TAKE CARE!
      # if you are evaluating inputs inside a function, it turns the function into
      # a reactive context!!!
      # valid_inputs <- validate_inputs(input_names, input) # UNCOMMENT HERE!
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

      # compute metric
      calories <- isolate(calories_reac())

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
  })
}
