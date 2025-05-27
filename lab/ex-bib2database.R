# Load packages
library(divDB)
library(biblioDB)

# Connect database (already existing)
cred <- credentials()

conn <- connect_db("test_bibrefs", user = cred["user"],
    password = cred["password"])

# create empty database
dbSendQuery(conn, "create schema bibrefs_empty")

## only query
query <- bib2database(conn, "bibrefs_empty", eval = FALSE)
query

## run query
bib2database(conn, "bibrefs_empty", eval = TRUE)

# create database with rows on it (further methods)

# go to update database


disconnect_db(conn)
