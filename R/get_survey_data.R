#' Obtain Post-Course Survey Data from Google Sheets
#'
#' Authenticate using a Gmail account or a service account and retrieve data from the post-survey sheet for the DS4OWD course.
#'
#' @param sheet_url URL to the Google Sheet. Defaults to the post-course survey for 2023.
#' @param service_acc_path Path to the JSON file of the service account with access to the Google Sheet. Not required if `email` is provided.
#' @param email Email address of the user with access to the Google Sheet. Defaults to a dummy email. Either this or `service_acc_path` is required.
#' @param n_course_modules Number of modules in the course. Default is 10.
#'
#' @return A CSV file with the cleaned survey data.
#' @export
#'
#' @examples
#' \dontrun{
#' get_survey_data(sheet_url = "docs.google.com/spreadsheets/d/YOUR_SHEET_ID/edit",
#'                 email = "jane.doe@gmail.com", n_course_modules = 10)
#' }
get_survey_data <- function(sheet_url = "",
                            service_acc_path = "path/to/service/account.json",
                            email = "jane.doe@gmail.com",
                            n_course_modules = 10) {

  # Default URL if none is provided
  if (sheet_url == "") {
    sheet_url <- "https://docs.google.com/spreadsheets/d/1Bb0f9fUePINt6Nr_uxiRJoQ36oYwcSc4hyTL1WoZZ7o/edit"
  }

  # Authentication logic
  if (service_acc_path == "path/to/service/account.json") {
    if (email == "jane.doe@gmail.com") {
      stop("Error: No authentication provided. Please specify either an email or a service account path for authentication.")
    } else {
      googledrive::drive_auth(email = email)
      googlesheets4::gs4_auth(email = email)
    }
  } else {
    googledrive::drive_auth(path = service_acc_path)
    googlesheets4::gs4_auth(path = service_acc_path)
  }

  # Confirm authentication
  drive_user_email <- googledrive::drive_user()$emailAddress
  message("Authenticated as: ", drive_user_email)

  # Function to get data from Google Sheet and save as CSV
  get_data_gsheet <- function(sheet_url) {
    # Get the sheet data
    data <- googlesheets4::read_sheet(sheet_url)

    # Get the sheet name
    sheet_name <- googlesheets4::gs4_get(sheet_url)$name

    # Display the first 5 rows of data for verification
    print(head(data))

    # Create necessary directories
    if (!fs::dir_exists("data")) {
      fs::dir_create("data")
    }
    fs::dir_create(file.path("data", sheet_name))

    # Clean the data
    data <- clean_survey_data(data, n_course_modules)

    # Write cleaned data to a CSV file
    output_file <- file.path("data", sheet_name, "postsurvey.csv")
    readr::write_csv(data, output_file)

    message("Data saved to: ", output_file)
  }

  # Call the function to retrieve and process the survey data
  get_data_gsheet(sheet_url)
}

#' Clean Survey Data
#'
#' Cleans the raw survey data retrieved from the Google Sheet.
#'
#' @param data A data frame containing the raw survey data.
#' @param n_course_modules Number of modules in the course. Default is 10.
#'
#' @return A cleaned data frame.
clean_survey_data <- function(data, n_course_modules = 10) {

  # Rename columns based on matching patterns
  data <- data %>%
    rename_with(~ case_when(
      str_detect(., "Timestamp") ~ "timestamp",
      str_detect(., "participate in") ~ "course_completion",
      str_detect(., "rate the overall content") ~ "rating_overall",
      str_detect(., "expectations") ~ "expectations",
      str_detect(., "learning objectives") ~ "learning_objectives",
      str_detect(., "rate the course structure") ~ "rating_structure",
      str_detect(., "knowledgeable") ~ "rating_ins_knowledge",
      str_detect(., "explain the course material") ~ "rating_ins_clarity",
      str_detect(., "instructor's competency") ~ "rating_ins_comp",
      str_detect(., "approach the instructor") ~ "rating_ins_app",
      str_detect(., "useful feedback") ~ "feedback",
      str_detect(., "rate your competency in R") ~ "rating_self_r_comp",
      str_detect(., "rate your competency in using Git") ~ "rating_self_vc_comp",
      str_detect(., "applying the skills") ~ "conf_skill_app",
      str_detect(., "like most") ~ "most_liked",
      str_detect(., "like least") ~ "least_liked",
      str_detect(., "suggestions") ~ "suggestion",
      str_detect(., "anything else") ~ "other_comments",
      str_detect(., "not participating") ~ "reason_non_participation",
      TRUE ~ .
    ))

  # Clean up the course_completion column
  data <- data %>%
    mutate(course_completion = case_when(
      course_completion == "completed the course including the capstone project" ~ as.character(n_course_modules),
      TRUE ~ str_extract(course_completion, "\\d+")
    ))

  # Drop the timestamp column as it's not relevant
  data <- data %>% select(-timestamp)

  # Convert course_completion to a numeric factor
  data <- data %>%
    mutate(course_completion = as.factor(as.numeric(course_completion)))

  return(data)
}
