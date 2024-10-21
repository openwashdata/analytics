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
