# TODO:   Importing Open Access from own database
# 
# Author: Miguel Alvarez
################################################################################

library(devtools)
install_github("kamapu/biblio")
install_github("kamapu/biblioDB", "new_pack")

library(biblio)
library(biblioDB)

# Testing function write_pg
Bib <- read_bib(x = "../Literatur_db/bib/MiguelReferences.bib")

Bib <- Bib[c(1:100),]

# Connect postgres
library(dbaccess)
library(RPostgreSQL)
conn <- connect_db(dbname = "test_db", user = "miguel")

# TODO: resolve not working function
write_pg(Bib, conn, "references2021", match_cols = TRUE, overwrite = TRUE)




# Debug
library(readODS)
library(RPostgreSQL)

name = "references2021"
df1 =Bib
df2 = Bib_files
main_table="main_table"
file_list="file_list"
overwrite=TRUE





















