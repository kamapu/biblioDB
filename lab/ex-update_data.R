# Load packages
library(divDB)
library(biblioDB)

# Connect database (already existing)
cred <- credentials()

conn <- connect_db("test_bibrefs", user = cred["user"],
    password = cred["password"])

# Create full database
dbSendQuery(conn, "drop schema bibrefs_update cascade")
dbSendQuery(conn, "create schema bibrefs_update")

# Load the data
bibrefs <- read_bib(file.path(path.package("biblio"), "LuebertPliscoff.bib"))
bib2database(conn, "bibrefs_update", bibrefs[1:5, ])

# Reviewed version
bib_reviewed <- bibrefs[2:7, ]
bib_reviewed$author[bib_reviewed$bibtexkey == "pliscoff2006"] <- "anclassonymous"

# Do comparison
update_data(conn, bib_reviewed, "bibrefs_update")






# Compare using lib_db
bib_reviewed2 <- as(bib_reviewed, "lib_db")

bib_reviewed2@dir$connection <- conn
bib_reviewed2@dir$schema <- "bibrefs_update"

compare_df(bib_reviewed2)

# Disconnect database
disconnect_db(conn)











# TODO:   Test full lib_db
# 
# Author: Miguel Alvarez
################################################################################

library(biblioDB)

# Load reference lists
bibrefs <- read_bib(file.path(path.package("biblio"), "LuebertPliscoff.bib"))
bibl_new <- as(bibrefs[1:10, ], "lib_db")

# Create database
cred <- credentials()
db_name <- "postgres"

conn <- connect_db(db_name, user = cred["user"], password = cred["password"])

# Drop database if exists
RPostgres::dbSendQuery(conn, "drop database if exists test_bibrefs")
RPostgres::dbSendQuery(conn, "create database test_bibrefs")

# complete metadata
bibl_new@dir$schema <- "bib_references"
#bibl_new@dir$folder <- "../../db-dumps/literatur_db/soft-copies"
bibl_new@dir$connection <- connect_db("test_bibrefs", user = cred["user"],
    password = cred["password"])

# check-files
#Files <- check_files(bibl_new)

# New database -----------------------------------------------------------------

# Drop Schema if exists
dbSendQuery(bibl_new@dir$connection, "create schema bib_references")

# Create database
query <- bib2database(bibl_new, eval = FALSE)











bibl_new@dir$connection <- connect_db("example_db", user = "miguel")
bib2database(bibl_new)

bibl_new@dir$schema <- "bib_references"
bib2database(bibl_new)

# Reimport DB
bibl_db <- db2lib_db(bibl_new@dir$connection, bibl_new@dir$schema,
    file_folder = "../../db-dumps/literatur_db/soft-copies")

# Everything from zero ---------------------------------------------------------
conn <- bibl_db@dir$connection
rm(list = ls()[!ls() %in% c("conn", "bibl")])

dbSendQuery(conn, "drop schema if exists bib_references cascade")

bibl_new <- as(bibl[1:50, ], "lib_db")
bibl_new@dir$connection <- conn
bibl_new@dir$schema <- "bib_references"

bib2database(bibl_new)

# Delete entries
bibl_new <- bibl[1:50, ]
bibl_new <- bibl_new[-c(5,15), ]

bibl_new <- as(bibl_new, "lib_db")
bibl_new@dir$connection <- conn
bibl_new@dir$schema <- "bib_references"

compare_df(bibl_new)
update_data(bibl_new)
update_data(bibl_new, delete = TRUE)

compare_df(bibl_new)

# Add entries
bibl_new <- bibl[1:50, ]

bibl_new <- as(bibl_new, "lib_db")
bibl_new@dir$connection <- conn
bibl_new@dir$schema <- "bib_references"

update_data(bibl_new)
update_data(bibl_new, add = TRUE)

compare_df(bibl_new)

# Update entries
bibl_new@main_table$title[c(3,6)] <- "no-title"
bibl_new@main_table$author[c(20,30)] <- "anonym"

update_data(bibl_new)
update_data(bibl_new, update = TRUE)

compare_df(bibl_new)
