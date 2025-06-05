library(biblio)
library(biblioDB)

openaccess <- read_bib("data-raw/openaccess.bib")
openaccess$journal <- openaccess$journaltitle

save(openaccess, file = "data/openaccess.rda")
