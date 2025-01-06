#' Title: Download and Load Data
#'
#' @param repo The name of the repository (default: "ds4owd")
#' @param download_token See README on how to obtain this token
#' @param github_token GitHub Personal Access Token (PAT) with 'repo' scope
#'
#' @return Exports RDA and CSV files for locations, presurvey, and roster data
#' @export
#'
#' @examples
#' download_and_load_data(download_token = DOWNLOAD_TOKEN, github_token = GITHUB_PAT)
get_registrations_data <- function(repo = "ds4owd",
                                   download_token = Sys.getenv("GITHUB_DOWNLOAD_TOKEN"),
                                   github_token = Sys.getenv("GITHUB_PAT")) {

  # Define URLs for the datasets
  loc_url <- paste0("https://raw.githubusercontent.com/openwashdata/", repo,
                    "/main/data/locations.rda?token=", download_token)
  presurvey_url <- paste0("https://raw.githubusercontent.com/openwashdata/", repo,
                          "/main/data/presurvey.rda?token=", download_token)
  roster_url <- paste0("https://raw.githubusercontent.com/openwashdata/", repo,
                       "/main/data/roster.rda?token=", download_token)

  # Ensure the 'data/registrations' directory exists
  if (!fs::dir_exists("data")) {
    fs::dir_create("data")
  }
  fs::dir_create(file.path("data", "registrations"))

  # List of URLs and corresponding file names
  urls <- c(loc_url, presurvey_url, roster_url)
  data_files <- c("locations.rda", "presurvey.rda", "roster.rda")
  names <- c("locations", "presurvey", "roster")

  # Loop over the URLs to download and load the files
  for (i in seq_along(urls)) {
    response <- httr::GET(urls[i], httr::add_headers(Authorization = paste("token", github_token)))

    if (httr::status_code(response) == 200) {
      # Save the file as .rda
      writeBin(httr::content(response, "raw"), file.path("data", "registrations", data_files[i]))
    } else {
      message("Failed to download ", data_files[i], ". Status code: ", httr::status_code(response))
    }
  }

  # Load all 3 files and write as CSV
  for (i in seq_along(data_files)) {
    load(file.path("data", "registrations", data_files[i]))
  }

  # Assuming `locations`, `presurvey`, and `roster` are loaded objects
  readr::write_csv(locations, file.path("data", "registrations", "locations.csv"))
  readr::write_csv(presurvey, file.path("data", "registrations", "presurvey.csv"))
  readr::write_csv(roster, file.path("data", "registrations", "roster.csv"))

  message("Successfully downloaded and loaded data.")
}
