library(httr)
library(tidyverse)

#' Title: Download and Load Data
#'
#' @param download_token - See README on how to obtain this token
#' @param github_token - GitHub Personal Access Token (PAT) with 'repo' scope
#'
#' @return Exports RDA and CSV files for locations, presurvey, and roster data
#' @export
#'
#' @examples
#' download_and_load_data(download_token = DOWNLOAD_TOKEN, github_token = GITHUB_PAT)
#'
get_registrations_data <- function(repo = "ds4owd", download_token=Sys.getenv("GITHUB_DOWNLOAD_TOKEN"), github_token=Sys.getenv("GITHUB_PAT")) {

  # Define URLs for the datasets
  loc_url <- paste("https://raw.githubusercontent.com/openwashdata/",repo, "/main/data/locations.rda?token=", download_token, sep="")
  presurvey_url <- paste("https://raw.githubusercontent.com/openwashdata/",repo,"/main/data/presurvey.rda?token=", download_token, sep="")
  roster_url <- paste("https://raw.githubusercontent.com/openwashdata/",repo,"/main/data/roster.rda?token=", download_token, sep="")

  # Create a 'data' folder if it doesn't exist
  if (!exists("dat")) {
    fs::dir_create("data")
  }

  fs::dir_create("data", "registrations")
  # List of URLs and corresponding file names
  urls <- c(loc_url, presurvey_url, roster_url)
  data_files <- c("locations.rda", "presurvey.rda", "roster.rda")
  names <- c("locations", "presurvey", "roster")

  # Loop over the URLs to download and load the files
  for (i in seq_along(urls)) {
    response <- GET(urls[i], add_headers(Authorization = paste("token", github_token)))

    if (status_code(response) == 200) {
      # Save the file as csv
      writeBin(content(response), file.path("data", "registrations", data_files[i]))
    } else {
      cat("Failed to download", data_files[i], ". Status code:", status_code(response), "\n")
    }
  }

# Load all 3 files
  for (i in seq_along(data_files)) {
    load(paste("data/registrations/",data_files[i], sep=""))
  }

# Write to csv
  write_csv(locations, file.path("data", "registrations", "locations.csv"))
  write_csv(presurvey, file.path("data", "registrations", "presurvey.csv"))
  write_csv(roster, file.path("data", "registrations", "roster.csv"))

  print("Successfully downloaded and loaded data")

}
