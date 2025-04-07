library(glue)

options(shiny.display.mode = "showcase")

update_widgets <- function(selected_values, session) {
  if (!is.null(selected_values$duration)) {
    updateNumericInput(
      session = session,
      inputId = "duration",
      value = as.numeric(selected_values$duration)
    )
  }

  if (!is.null(selected_values$weight)) {
    updateNumericInput(
      session = session,
      inputId = "weight",
      value = as.numeric(selected_values$weight)
    )
  }

  if (!is.null(selected_values$age)) {
    updateNumericInput(
      session = session,
      inputId = "age",
      value = as.integer(selected_values$age)
    )
  }

  if (!is.null(selected_values$gender)) {
    updateSelectInput(
      session = session,
      inputId = "gender",
      selected = selected_values$gender
    )
  }

  if (!is.null(selected_values$activity)) {
    updateSelectInput(
      session = session,
      inputId = "activity",
      selected = selected_values$activity
    )
  }
}

get_age_range <- function(age) {
  message("--- Computing age category")

  if (age < 18) range <- NULL
  if (age >= 18 & age < 30) range <- "[18 - 30)"
  if (age >= 30 & age < 40) range <- "[30 - 40)"
  if (age >= 40 & age < 50) range <- "[40 - 50)"
  if (age >= 50 & age < 60) range <- "[50 - 60)"
  if (age >= 60 & age < 70) range <- "[60 - 70)"
  if (age >= 70) range <- "70+"

  message(glue("------ ||| age range: {age} - {range}", .null = "NULL"))
  return(range)
}

get_age_factor <- function(age, with_delay = FALSE) {
  if (with_delay) delay_it("get_age_factor")

  if (age < 18) return(NULL)
  if (age >= 18 & age < 30) factor <- 1.00
  if (age >= 30 & age < 40) factor <- 0.98
  if (age >= 40 & age < 50) factor <- 0.96
  if (age >= 50 & age < 60) factor <- 0.94
  if (age >= 60 & age < 70) factor <- 0.92
  if (age >= 70) factor <- 0.92

  message(glue("------ ||| age factor: {age} - {factor}", .null = "NULL"))
  return(factor)
}

get_gender_factor <- function(gender, with_delay = FALSE) {
  if (with_delay) delay_it("get_gender_factor")

  if (gender == "Male") factor <- 1.1
  if (gender == "Female") factor <- 0.9

  message(glue("------ ||| gender factor: {gender} - {factor}", .null = "NULL"))
  return(factor)
}

get_MET_factor <- function(MET, with_delay = FALSE) {
  if (with_delay) delay_it("get_MET_factor")

  if (MET == "Volleyball") factor <- 3.0
  if (MET == "Walking") factor <- 3.8
  if (MET == "Cycling") factor <- 6.8
  if (MET == "Football") factor <- 7.0
  if (MET == "Tennis") factor <- 7.3
  if (MET == "Running") factor <- 8.0
  if (MET == "Swimming") factor <- 9.8

  message(glue("------ ||| MET factor: {MET} - {factor}", .null = "NULL"))
  return(factor)
}

calculate_calories_burn <- function(
    duration,
    weight,
    age_factor = NULL,
    gender_factor = NULL,
    MET_factor = NULL,
    adjusted_MET = NULL,
    with_delay = FALSE
) {
  if (with_delay) delay_it("calculate_calories_burn")

  if (is.null(adjusted_MET)) adjusted_MET <- age_factor * gender_factor * MET_factor
  cal <- (duration * weight * adjusted_MET * 3.5)/200

  message(
    glue(
      "------ ||| calories burn:
      ------ ||| --- duration: {duration}
      ------ ||| --- weight: {weight}
      ------ ||| --- age_factor: {age_factor}
      ------ ||| --- gender_factor: {gender_factor}
      ------ ||| --- MET_factor: {MET_factor}
      ------ ||| --- adjusted_MET: {adjusted_MET}
      ",
      .null = "NULL"
    )
  )

  return(round(cal, 2))
}

delay_it <- function (what, time = 1) {
  message(glue("------ delaying {what} for {time} seconds"))
  Sys.sleep(time = time)
}

get_cookies <- function(input_names) {
  sapply(
    X = input_names,
    FUN = function(x) {
      val <- get_cookie(cookie_name = x)
      if (!is.null(val) && val == "null") {
        val <- NULL
      }

      return(val)
    },
    simplify = FALSE
  )
}

save_cookies <- function(input_names, input) {
  lapply(
    X = input_names,
    FUN = function(x) set_cookie(cookie_name = x, cookie_value = input[[x]])
  )
}

remove_cookies <- function(input_names, session) {
  lapply(
    X = input_names,
    FUN = function(x) remove_cookie(cookie_name = x, session = session)
  )
}

validate_input <- function(input, inputId) {
  if (isTruthy(input[[inputId]])) {
    if (inputId == "age" && input[[inputId]] < 18) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  } else {
    return(FALSE)
  }
}

validate_inputs <- function(input_names, input) {
  validation <- sapply(
    X = input_names,
    FUN = function(x) validate_input(input = input, inputId = x)
  )

  if (any(!validation)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

show_hide_input_msg <- function(input, inputId) {
  msg_id <- glue("{inputId}_msg")
  msg_id_invalid <- glue("{inputId}_msg_invalid")

  if (isTruthy(input[[inputId]])) {
    if (inputId == "age" && input[[inputId]] < 18) {
      show(msg_id_invalid)
    } else {
      hide(msg_id_invalid)
    }

    hide(msg_id)
  } else {
    show(msg_id)
  }
}

show_hide_input_msgs <- function(input_names, input) {
  lapply(input_names, show_hide_input_msg, input = input)
}

# constants
with_delay <- TRUE
cannot_be_empty <- "This field cannot be empty"
invalid_age <- "Calculation applicable only for individuals aged 18 and above"
input_names <- c("duration", "weight", "age", "gender", "activity")
