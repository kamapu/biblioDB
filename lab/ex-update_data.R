# Load packages
library(divDB)
library(biblioDB)

# Connect database (already existing)
cred <- credentials()

conn <- connect_db("test_bibrefs", user = cred["user"],
    password = cred["password"])

# Create full database
dbSendQuery(conn, "drop schema bibrefs_update cascade")
dbSendQuery(conn, "create schema bibrefs_update")

# Load the data
bibrefs <- read_bib(file.path(path.package("biblio"), "LuebertPliscoff.bib"))
bib2database(conn, "bibrefs_update", bibrefs[1:5, ])

# Reviewed version
bib_reviewed <- bibrefs[2:7, ]
bib_reviewed$author[bib_reviewed$bibtexkey == "pliscoff2006"] <- "anonymous"

# Do comparison
update_data(conn, bib_reviewed, schema = "bibrefs_update")

# Compare using lib_db
bib_reviewed2 <- as(bib_reviewed, "lib_db")

bib_reviewed2@dir$connection <- conn
bib_reviewed2@dir$schema <- "bibrefs_update"

update_data(bib_reviewed2)

# Update (only query)
query <- update_data(bib_reviewed2, eval = FALSE, add = TRUE, delete = TRUE,
    update = TRUE)
query

# Run the query
update_data(bib_reviewed2, add = TRUE, delete = TRUE, update = TRUE)

# Disconnect database
disconnect_db(conn)
