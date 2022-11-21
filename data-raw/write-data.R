# TODO:   Produce package's data
#
# Author: Miguel Alvarez
################################################################################

library(usethis)
library(readODS)

bib_tags <- list()
bib_tags$file_list <- read_ods(
  "../biblio/data-raw/common-data.ods",
  "file_list"
)
bib_tags$tags_bib <- read_ods(
  "../biblio/data-raw/prototype-bib.ods",
  "main_table"
)

use_data(bib_tags, internal = TRUE, overwrite = TRUE)
