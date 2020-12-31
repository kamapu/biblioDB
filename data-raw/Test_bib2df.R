# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

library(devtools)
install_github("ropensci/bib2df")

library(bib2df)
library(biblio)


start <- Sys.time()
df1 <- bib2df("/media/miguel/Miguel/Literatur/Literatur_db/MiguelReferences.bib")
Sys.time() - start

start <- Sys.time()
df2 <- read_bib("/media/miguel/Miguel/Literatur/Literatur_db/MiguelReferences.bib")
Sys.time() - start

