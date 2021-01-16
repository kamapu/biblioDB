# TODO:   Importing Open Access from own database
# 
# Author: Miguel Alvarez
################################################################################

library(biblio)

openaccess <- read_bib(x = "../Literatur_db/bib/MiguelReferences.bib")
openaccess <- subset(openaccess, bibtexkey %in%
				c("Alvarez2018", "Mora2018", "Oulas2016"))

write_bib(x = openaccess, file = "inst/openaccess.bib")
save(openaccess, file = "data/openaccess.rda")

# TODO: Copy the files
