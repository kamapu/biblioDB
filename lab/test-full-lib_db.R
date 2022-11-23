# TODO:   Test full lib_db
# 
# Author: Miguel Alvarez
################################################################################

library(biblioDB)
## library(RPostgreSQL)

bibl <- read_bib("../../db-dumps/literatur_db/bib/MiguelReferences.bib")
bibl_new <- as(bibl, "lib_db")

# complete metadata
bibl_new@dir$schema <- "bib_references"
bibl_new@dir$folder <- "../../db-dumps/literatur_db/soft-copies"
bibl_new@dir$connection <- connect_db("example_db", user = "miguel")

# check-files
Files <- check_files(bibl_new)

# New database -----------------------------------------------------------------

# Drop Schema if exists
dbSendQuery(bibl_new@dir$connection,
    "drop schema if exists bib_references cascade")
dbSendQuery(bibl_new@dir$connection, "create schema bib_references")

# Create database
bibl_new <- as(bibl[1:100, ], "lib_db")
bib2database(bibl_new)

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
