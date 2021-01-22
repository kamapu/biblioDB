# TODO:   Protocol for potential comparisson between two PG databases
# 
# Author: Miguel Alvarez
################################################################################

### Test
library(biblioDB)
library(dbaccess)
library(RPostgreSQL)

conn <- connect_db2(dbname = "veg_databases", user = "miguel")
Bib <- read_bib("../Literatur_db/bib/MiguelReferences.bib")

colnames(y)[!colnames(y) %in% colnames(x)]
colnames(x)[!colnames(x) %in% colnames(y)]

Test <- compare_df(conn, Bib, name = "bib_references")
