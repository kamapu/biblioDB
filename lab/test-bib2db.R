# TODO:   Add comment
# 
# Author: Miguel Alvarez
################################################################################

library(biblioDB)
library(RPostgreSQL)
library(taxlist)

bibl <- read_bib("../../db-dumps/literatur_db/bib/MiguelReferences.bib")
bibl <- bibl[1:50, ]

bibl2 <- as(bibl, "lib_db")

# Read sys data
conn <- connect_db("example_db", user = "miguel")

bib_tags <- biblioDB:::bib_tags
schema <- c("bib_references")
comment = ""

# TODO: Delete schemas at the begining
# TODO: Test with and without data
# TODO: Test update methods
