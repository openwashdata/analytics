library(googledrive)
library(googlesheets4)
library(tidyverse)
library(fs)

#' Obtain Post-Course Survey Data from Google Sheets
#'
#' This code is used to authenticate a gmail account (or utilise a service account) and retreive data from the post survey sheet for the DS4OWD course.
#'
#' @param sheet_url URL to the google sheet. Defaults to post course survey for 2023
#' @param service_acc_path Path to the JSON file of the service account that has access to the Google Sheet. No Default, not required if email is provided
#' @param email Email address of the user that has access to the Google Sheet. Defaults to dummy email, Either this or the service account path is required.
#' @param n_course_modules Number of modules in the course. Default is 10
#'
#' @return A CSV file with the cleaned survey data
#' @export
#'
#' @examples
#' \dontrun {
#' get_survey_data(sheet_url = "docs.google.com/spreadsheets/d/YOUR_SHEET_ID/edit", email = "jane.doe@gmail.com", n_course_modules = 10)
#' }
get_survey_data <- function(sheet_url = "",
                                      service_acc_path = "path/to/service/account.json",
                                      email = "jane.doe@gmail.com",
                                      n_course_modules = 10) {

  if (sheet_url == ""){
    sheet_url = "https://docs.google.com/spreadsheets/d/1Bb0f9fUePINt6Nr_uxiRJoQ36oYwcSc4hyTL1WoZZ7o/edit"
  }

  # Authentication logic
  if (service_acc_path == "path/to/service/account.json") {
    if (email == "jane.doe@gmail.com") {
      stop("Error: No authentication provided. Please add either an email or service account to authenticate.")
    } else {
      drive_auth(email = email)
    }
  } else {
    drive_auth(path = service_acc_path)
  }

  # Confirm authentication
  drive_user <- drive_user()$emailAddress
  print(paste("Authenticating with", drive_user, sep=" "))

  # Function to get data from Google Sheet and save as CSV
  get_data_gsheet <- function(sheet_url) {
    # Get the sheet
    data <- read_sheet(sheet_url)

    # Get the name of the sheet
    sheet_name <- gs4_get(sheet_url)$name

    # Display first 5 rows of data
    print(head(data))

    # Create a "data" directory and sheet-specific subdirectory if not exist
    if (!dir.exists("data")) {
      dir_create("data")
    }

    dir_create(file.path("data", sheet_name))

    # Clean the data
    data <- clean_survey_data(data)

    # Write data to CSV
    write_csv(data, file.path("data", sheet_name, "postsurvey.csv"))

    print(paste("Data saved to", file.path("data", sheet_name, "postsurvey.csv")))
  }

  # Call the get_data_gsheet function
  get_data_gsheet(sheet_url)
}

clean_survey_data <- function(data = data.frame(), n_course_modules = 10) {

  data <- data %>%
    rename("timestamp" = "Timestamp",
           "course_completion" = contains("participate in"),
           "rating_overall" = contains("rate the overall content"),
           "expectations" = contains("expectations"),
           "learning_objectives" = contains("learning objectives"),
           "rating_structure" = contains("rate the course structure"),
           "rating_ins_knowledge" = contains("knowledgeable"),
           "rating_ins_clarity" = contains("explain the course material"),
           "rating_ins_comp" = contains("instructor's competency"),
           "rating_ins_app" = contains("approach the instructor"),
           "feedback" = contains("useful feedback"),
           "rating_self_r_comp" = contains("rate your competency in R"),
           "rating_self_vc_comp" = contains("rate your competency in using Git"),
           "conf_skill_app" = contains("applying the skills"),
           "most_liked" = contains("like most"),
           "least_liked" = contains("like least"),
           "suggestion" = contains("suggestions"),
           "other_comments" = contains("anything else"),
           "reason_non_participation" = contains("not participating")
    )

  # Clean up course_completion column
  data <- data |>
    mutate(course_completion = case_when(
      course_completion == "completed the course including the capstone project" ~ toString(n_course_modules),
      TRUE ~ str_extract(course_completion, "\\d+")
    ))

  # Reomve timestamp column (not relevant)
  data <- data %>%
    select(-timestamp)

  # Convert course_completion to numeric factor
  data <- data |>
    mutate(course_completion = as.numeric(course_completion))
  postsurvey <- data |>
    mutate(course_completion = as.factor(course_completion))

}

