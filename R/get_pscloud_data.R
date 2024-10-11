library(tidyverse)
library(rscloud)
library(lubridate)

# Function to pull daily usage data from posit cloud for the past 365 days.
#' Pull Data
#'
#' @param space_id ID of the space to retrieve data from. Defaults to the DS$OWD space ID.
#'
#' @return Saves a CSV file with daily usage data. The file is stored as /data-raw/space_member_daily_usage.csv
#'
#' @export
#' @examples
#' space_id <- 999999
#' get_space_data(space_id)
get_space_data <- function(space_id=426916) {
  # Assuming you've already set up your space connection
  pcloud001 <- rscloud_space(space_id = space_id)

  # Set the start date to 1 year ago
  start_date <- as.Date(Sys.Date() - 365)
  end_date <- Sys.Date()  # Today's date

  # Create a sequence of dates
  date_sequence <- seq(start_date, end_date, by = "day")

  # Function to get data for a single day
  get_daily_data <- function(date) {
    from_date <- format(date, "%Y-%m-%dT00:00:00Z")
    until_date <- format(date + days(1), "%Y-%m-%dT00:00:00Z")

    tryCatch({
      space_member_usage(
        space = pcloud001,
        filters = list(
          groupby = "user_id",
          from = from_date,
          until = until_date
        )
      )
    }, error = function(e) {
      warning(paste("Error for date", date, ":", e$message))
      return(NULL)
    })
  }

  # Apply the function to each date and combine results
  all_data <- map_dfr(date_sequence, get_daily_data)

  # Remove any NULL entries (if there were errors)
  all_data <- all_data %>% filter(!is.null(.))

  if (!dir.exists("data")) {
    fs::dir_create("data")
  }

  folder_name <- "posit_cloud_data"

  fs:: dir_create("data", folder_name)

  # Save the data to a CSV file
  write_csv(all_data, file.path("data", folder_name, "space_member_daily_usage.csv"))
}

