sheet_link <- "https://docs.google.com/spreadsheets/u/1/d/1vtw16vpvJbioDirGTQcy0Ubz01Cz7lcwFVvbxsNPSVM/edit?gid=0#gid=0"


get_metadata_gsheet <- function(sheet_url = sheet_link,service_acc_path = "path/to/service/account.json",email = "") {

  # Service account is preferred over email\

  if (service_acc_path == "path/to/service/account.json") {
    if (email == "") {
      stop("Error: No authentication provided. Please specify either an email or a service account path for authentication.")
    } else {
      googledrive::drive_auth(email = email)
      googlesheets4::gs4_auth(email = email)
    }
  } else {
    googledrive::drive_auth(path = service_acc_path)
    googlesheets4::gs4_auth(path = service_acc_path)
  }

metadata_table <- googlesheets4::read_sheet(sheet_url)

# Save to a folder in data
if (!fs::dir_exists("data/metadata")) {
  fs::dir_create("data/metadata")
}

# Write the metadata to a CSV file
readr::write_csv(metadata_table, file.path("data/metadata", "publishing_metadata.csv"))

}


