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
