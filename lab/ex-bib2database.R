# Load packages
library(divDB)
library(biblioDB)

# Connect database (already existing)
cred <- credentials()

conn <- connect_db("test_bibrefs", user = cred["user"],
    password = cred["password"])

# create empty database --------------------------------------------------------
dbSendQuery(conn, "drop schema bibrefs_empty cascade")
dbSendQuery(conn, "create schema bibrefs_empty")

## only query
query <- bib2database(conn, "bibrefs_empty", eval = FALSE)
query

## run query
bib2database(conn, "bibrefs_empty", eval = TRUE)

# create database with rows on it ----------------------------------------------
dbSendQuery(conn, "drop schema bibrefs_content cascade")
dbSendQuery(conn, "create schema bibrefs_content")

# Load the data
bibrefs <- read_bib(file.path(path.package("biblio"), "LuebertPliscoff.bib"))

#bib1 <- as(bibrefs[1:10], "lib_db")

## only query
query <- bib2database(conn, "bibrefs_content", bibrefs[1:10, ], eval = FALSE)
query

## Fill the database
bib2database(conn, "bibrefs_content", bibrefs)

# Including all information in the object --------------------------------------
bibrefs <- as(openaccess, "lib_db")
bibrefs@dir$connection <- conn
bibrefs@dir$folder <- "inst/articles" # Change to package folder
bibrefs@dir$schema <- "bibrefs_full"

dbSendQuery(conn, "drop schema bibrefs_full cascade")
dbSendQuery(conn, "create schema bibrefs_full")

# Only query
query <- bib2database(bibrefs, eval = FALSE)
query

bib2database(bibrefs)

# Close connection
disconnect_db(conn)
