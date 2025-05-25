library(biblio)
library(biblioDB)

openaccess <- read_bib("inst/openaccess.bib")
openaccess$journal <- openaccess$journaltitle

save(openaccess, file = "data/openaccess.rda")
