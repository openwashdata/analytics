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

write_table = function(file, data, con) {
  tab_name = unlist(strsplit(file,"\\."))[1]
  print(tab_name)
  if (!dbExistsTable(con, tab_name)) {
    dbCreateTable(con, tab_name, data)
    print("Created new table:", tab_name, sep=" ")
  }
  else {
  dbWriteTable(con, tab_name, data, overwrite = TRUE)
  }
}

write_db = function() {
  # Loop through all folders in the data folder
  for (folder in list.dirs("data", full.names = FALSE)) {
    # Loop through all files in the folder
    for (file in list.files(paste0("data/", folder), full.names = TRUE)) {
      # Check file extension
      if (grepl(".csv", file)) {
        # Read the file
        data = read_csv(file)
        print(paste("Read csv:", file, sep=" "))
        write_table(file, data, con)
      }
      else if (grepl(".rda",file)){
        # Load the file
        load(file)
        print(paste("Loaded rda:", file, sep=" "))
        write_table(file, data, con)
      }
      else {
        print(paste("Unknown file type", file, sep=" "))
        next
      }
    }
  }
}
