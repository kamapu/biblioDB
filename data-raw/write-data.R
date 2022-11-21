# TODO:   Produce package's data
#
# Author: Miguel Alvarez
################################################################################

library(usethis)
library(biblio)

bib_tags <- biblio:::data_bib[c("file_list", "tags_bib")]

usethis::use_data(bib_tags, internal = TRUE)
