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

# All existing tables in the database are copied and archived
#' Title
#'
#' @param user Username for the database
#' @param password Password to login for given username
#'
#' @details This is a helper function to archive data before tables are deleted. Old archives are owverwritten.
#'
#' @return All tables in the database are copied and archived
#' @export
#'
#' @examples
archive_db = function(user="", password="") {


  con <- connect_db(user,password)
  # Get all tables in the database
  tables <- dbListTables(con)
  # Loop through all tables
  for (table in tables) {
    # If archive already exists, drop the archive
    if (paste0(table, "_archive") %in% tables) {
      dbRemoveTable(con, paste0(table, "_archive"))
      print(paste("Dropped archive:", paste0(table, "_archive")))
      dbWriteTable(con, paste0(table, "_archive"), dbReadTable(con, table))
      print(paste("Archived table:", table))
    }
    else {
      dbWriteTable(con, paste0(table, "_archive"), dbReadTable(con, table))
      print(paste("Archived table:", table))
    }
  }
}

# All tables in the database are deleted, except archives and specified tables
#' Delete Tables
#'
#' @param user Username for the database
#' @param password Password to login for given username
#'
#' @details This is a helper function to clean the database. It deletes all tables except admin generated and archives.
#'
#' @return Deletes all tables except admin generated and archives.
#' @export
#'
#' @examples
clean_db = function(user="", password="") {


  con <- connect_db(user,password)
  # Get all tables in the database
  tables <- dbListTables(con)
  # Loop through all tables
  for (table in tables) {
    # Skip tables ending with "_archive" and the special tables
    if (!grepl("_archive$", table) && !table %in% c("mtcars", "admin", "compostblantyre")) {
      # Drop the table
      dbRemoveTable(con, table)
      print(paste("Dropped table:", table))
    }
  }
}

#' Cleans and archives the remote Postgres Database
#'
#' @param user Username for the database
#' @param password Password to login for given username
#'
#' @details This is a helper function to clean and archive the database. It first archives all tables and then deletes them.
#'
#' @return Archives all existing tables (except admin) and deletes them to allow new data to be entered
#' @export
#'
#' @examples
clean_and_archive_db = function(user="", password="") {

  # Ask for confirmation
  cat("This will archive all existing tables and delete them. Are you sure you want to continue?\n1. Yes\n2. No: ")
  response <- readline()
  if (response != 1) {
    print("Aborted")
    return()
  }
  archive_db(user, password)
  clean_db(user, password)
}
