library(RPostgres)
library(tidyverse)


# Connect to the database
HOST = "id-hdb-psgr-ct39.ethz.ch"
PORT = 5432
DBNAME = "openwashdata"


connect_db = function(user="", password="") {
  con <- dbConnect(Postgres(), host=HOST, port=PORT, dbname=DBNAME, user=user, password=password)
  return(con)
}

#' Write To Database Table
#'
#' @param file filepath of data being written
#' @param data data to be written to the database
#' @param con variable, connection to the Postgres database
#'
#' @details This is a helper function to check if the table exists, if not it creates the table and writes to it.
#' If the table exists, it overwrites existing data.
#' @return Writes the data to the database
#' @export
#'
#' @examples
write_table = function(file, data, con) {



  tab_name = tools::file_path_sans_ext(basename(file))
  if (!dbExistsTable(con, tab_name)) {
    dbCreateTable(con, tab_name, data)
    dbWriteTable(con, tab_name, data, overwrite=TRUE)
    print(paste("Created new table: ", tab_name))
  }
  else {
  dbWriteTable(con, tab_name, data, overwrite = TRUE)
  }
}

#' Write to Database
#'
#' @param user Username for the database
#' @param password Password for above username
#'
#' @details Change the host, port and database name to connect to chosen database.
#'
#' @return Writes all csv and rda files in the data folder to the database
#' @export
#'
#' @examples
#' \dontrun{
#' write_db("janedoe", "areallystrongpassword")
#' }
#'
write_db = function(user="", password="") {

  con <- connect_db(user,password)
  # Loop through all folders in the data folder
  for (folder in list.dirs("data", full.names = FALSE)) {
    # Loop through all files in the folder
    for (file in list.files(paste0("data/", folder), full.names = TRUE)) {
      # Check file extension
      if (grepl(".csv", file)) {
        # Read the file
        data = read.csv(file)
        print(head(data))
        print(paste("Read csv:", file, sep=" "))
        write_table(file, data, con)
        # Wait 3 seconds
        Sys.sleep(3)
      }
      else if (grepl(".rda",file)){
        # Load the file
        load(file)
        print(paste("Loaded rda:", file, sep=" "))
        write_table(file, data, con)
        Sys.sleep(3)
      }
      else {
        print(paste("Unknown file type", file, sep=" "))
        next
      }
    }
  }
}
