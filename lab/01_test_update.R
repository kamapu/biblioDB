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
library(dbaccess)
library(RPostgreSQL)

# Produce example data sets
Bib <- read_bib("../Literatur_db/bib/MiguelReferences.bib")

Bib2 <- Bib[500:510, ]
Bib <- Bib[1:30, ]

Bib3 <- Bib[-c(5, 10, 15), ]
Bib3$journaltitle <- replace_x(Bib3$journaltitle,
  old = c("J Biogeogr", "Mol Ecol", "Ecol Lett"),
  new = c(
    "Journal of Biogeography", "Molecular Ecology",
    "Ecology Letters"
  )
)
Bib2 <- do.call(rbind, list(Bib3, Bib2))

compare_df(Bib, Bib2)

# Create database
conn <- connect_db2(dbname = "test_db", user = "miguel")

write_pg(Bib, conn, "refs_1", overwrite = TRUE)

# Test read function
Test <- read_pg(conn = conn, name = "refs_1")
## Test <- read_pg(conn = conn, name = "refs_1", simplify = TRUE)

# Test compare function
compare_df(x = conn, y = Bib2, name = "refs_1")

# Test no action
update(object = conn, revision = Bib2, name = "refs_1")

# Test delete
update(object = conn, revision = Bib2, name = "refs_1", delete = TRUE)

# Test add
update(object = conn, revision = Bib2, name = "refs_1", add = TRUE)

# Test update
update(object = conn, revision = Bib2, name = "refs_1", update = TRUE)
