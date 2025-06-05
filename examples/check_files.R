## Coerce data to 'lib_db' and set articles path
bibrefs <- as(openaccess, "lib_db")
bibrefs@dir$folder <- file.path(path.package("biblioDB"), "articles")

## List stored articles
list.files(bibrefs@dir$folder)

## Check with electronic references
check_files(bibrefs)
