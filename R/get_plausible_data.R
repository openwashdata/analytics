library(tidyverse)
library(httr)
library(jsonlite)

#' Retrieve and Save Web Analytics Data from Plausible
#'
#' This function fetches analytics data from the Plausible API for a given website and saves it into CSV files. The data includes metrics like pageviews, visitors, bounce rate, and more. The function allows customization of the website URL and the option to retrieve pagewise data.
#'
#'
#' @param site_url A character string specifying the website URL to retrieve data for. If left empty, the default URL "ds4owd-001.github.io/website" is used.
#' @param pagewise A logical value indicating whether to retrieve pagewise breakdown data. Default is `FALSE`.
#' @param token A character string for the Plausible API token. Defaults to retrieving the token from the environment variable "PLAUSIBLE_TOKEN". See the Readme for more information on how to save tokens to the R environment
#'
#'@details
#' The function constructs API requests to Plausible to retrieve data for countries, sources, browsers, and a time series of visits. It saves the data in CSV files within a folder named after the site.
#'
#' The data is broken down by country, source, and browser. Optionally, if `pagewise = TRUE`, it also retrieves pagewise data.
#'
#' The time series data is for the period from 2023-09-27 (when the website was initially created) to the current date.
#'
#' The resulting data is saved in the `data/` folder under a subdirectory named after the first part of the website URL.
#'
#' @return This function does not return a value. It saves the retrieved data as CSV files.
#'
#' @examples
#' \dontrun {
#' get_plausible_data(site_url = "ds4owd-001.github.io/website", pagewise = TRUE, token = "MY_PLAUSIBLE_TOKEN)
#' }
#'
#' @export

get_plausible_data <- function(site_url="", pagewise=FALSE, token=Sys.getenv("PLAUSIBLE_TOKEN")) {

  SITE_URL <- "ds4owd-001.github.io/website"

  # If site url and github site url is not specified, use defaults
  if (site_url == "") {
    site_url <- SITE_URL
    print("Using default website URL")
  }

  # URL construct for stats breakdown
  breakdown_api <- "https://plausible.io/api/v1/stats/breakdown?site_id="
  period <- "&period=12mo"
  metrics_url <- "&metrics=pageviews,visitors,bounce_rate,visit_duration,visits&limit=100"

  # URL Construct for Timeseries
  timeseries_api <- "https://plausible.io/api/v1/stats/timeseries?site_id="
  period <- paste0("&period=custom&date=2023-09-27,",Sys.Date())
  metrics_url <- "&metrics=pageviews,visitors,bounce_rate,visit_duration,visits"

  # Construct the URLs
  url_country <- paste0(breakdown_api, site_url, period, "&property=visit:country", metrics_url)
  url_source <- paste0(breakdown_api, site_url, period, "&property=visit:source", metrics_url)
  url_browser <- paste0(breakdown_api, site_url, period, "&property=visit:browser", metrics_url)

  url_timeseries <- paste0(timeseries_api, site_url, period, metrics_url)

country_data <- get_data(url_country, token)
source_data <- get_data(url_source, token)
browser_data <- get_data(url_browser, token)
timeseries_data <- get_data(url_timeseries, token)

timeseries_data$date <- as.Date(timeseries_data$date)

# If pagewise data is requested
if (pagewise) {
  url_pageviews <- paste0(breakdown_api, site_url, period, "&property=event:page", metrics_url)
  pageviews_data <- get_data(url_pageviews)
}

# Export to CSV

# Website name
folder_name <- unlist(strsplit(site_url, "\\."))[1]

if (!dir.exists("data")) {
  fs::dir_create("data")
}

fs::dir_create("data", folder_name)

write_csv(country_data, file.path("data", folder_name, paste(folder_name,"country_data.csv",sep="_")))
write_csv(source_data, file.path("data", folder_name, paste(folder_name,"source_data.csv",sep="_")))
write_csv(browser_data, file.path("data", folder_name, paste(folder_name,"browser_data.csv",sep="_")))
write_csv(timeseries_data, file.path("data", folder_name, paste(folder_name,"timeseries_data.csv",sep="_")))

print(paste("Successfully saved data to", folder_name, sep=" "))

}

#' Send Request to Plausible for Data
#'
#' @param url A character string specifying the URL to send the request to.
#'
#' @return A list of results from the request.
#' @export
#'
#' @examples
#' \dontrun{
#' country_data <- get_data(country_url)
#' }
get_data <- function(url, TOKEN) {
  response <- GET(url,
                  add_headers(Authorization = paste("Bearer", TOKEN)))

  # Check if the request was successful
  if (status_code(response) == 200) {
    # Parse the JSON content
    data <- content(response, "text", encoding = "UTF-8")
    results <- fromJSON(data)$results

    # Return the results
    return(results)
  } else {
    print("Could not retreive data, please check your authentication token or URL")
    return(NULL)
  }
}
