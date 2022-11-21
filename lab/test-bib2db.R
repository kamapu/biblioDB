# TODO:   Testing new functions 'bib2database()' and 'update_data()'
# 
# Author: Miguel Alvarez
################################################################################

library(biblioDB)
## library(RPostgreSQL)

bibl <- read_bib("../../db-dumps/literatur_db/bib/MiguelReferences.bib")
bibl_new <- bibl[1:50, ]

# New
conn <- connect_db("example_db", user = "miguel")

# Drop Schema if exists
dbSendQuery(conn, "drop schema if exists bib_references cascade")
dbSendQuery(conn, "create schema bib_references")

# Create empty database and update
bib2database(conn, "bib_references")

compare_df(conn, bibl_new, schema = "bib_references")

update_data(conn, bibl_new, schema = "bib_references", add = TRUE)

compare_df(conn, bibl_new, schema = "bib_references")

# Delete entries
bibl_new <- db2lib_db(conn, "bib_references")
bibl_new <- as(bibl_new, "lib_df")
bibl_new

bibl_new <- bibl_new[-c(5,15), ]

update_data(conn, bibl_new, schema = "bib_references")
update_data(conn, bibl_new, schema = "bib_references", delete = TRUE)

compare_df(conn, bibl_new, schema = "bib_references")

# Update entries
bibl_new$title[c(3,6)] <- "no-title"
bibl_new$author[c(20,30)] <- "anonym"

update_data(conn, bibl_new, schema = "bib_references")

# TODO: Check generated error messages
update_data(conn, bibl_new, schema = "bib_references", delete = TRUE)
update_data(conn, bibl_new, schema = "bib_references", update = TRUE)

compare_df(conn, bibl_new, schema = "bib_references")

