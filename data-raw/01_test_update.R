# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

library(devtools)

install_github("kamapu/biblio")
install_github("kamapu/biblioDB", "update_pg")

library(biblio)
library(biblioDB)
library(taxlist)
library(dbaccess);library(RPostgreSQL)

# Produce example data sets
Bib <- read_bib("../Literatur_db/bib/MiguelReferences.bib")

Bib2 <- Bib[500:510, ]
Bib <- Bib[1:30, ]

Bib3 <- Bib[-c(5,10,15), ]
Bib3$journaltitle <- replace_x(Bib3$journaltitle,
		old = c("J Biogeogr", "Mol Ecol", "Ecol Lett"),
		new = c("Journal of Biogeography", "Molecular Ecology",
				"Ecology Letters"))
Bib2 <- do.call(rbind, list(Bib3, Bib2))

compare_df(Bib, Bib2)

# Create database
conn <- connect_db2(dbname = "test_db", user = "miguel")

write_pg(Bib, conn, "refs_1", overwrite = TRUE)

#library(readODS)
#dbSendQuery(conn, "DROP SCHEMA refs_1 CASCADE;")

# debug update
object <- conn
revision <- Bib2
key = "bibtexkey"
name <- "refs_1"
db_args = list()
delete = FALSE
add = FALSE
update = FALSE
main_table = "main_table"
file_list = "file_list"


Test <- read_pg(conn = conn, name = name)

add_files = TRUE
simplify = FALSE
match_cols = FALSE
