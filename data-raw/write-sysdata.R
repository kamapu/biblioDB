library(usethis)

bib_tags <- list(
  file_list = biblio:::files_jabref,
  tags_bib = biblio:::standard_bib
)

use_data(bib_tags, internal = TRUE, overwrite = TRUE)
