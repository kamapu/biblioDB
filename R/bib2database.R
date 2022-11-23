#' @name bib2database
#' @rdname bib2database
#'
#' @title Create a database to store bibliographic references
#'
#' @description
#' A relational data model to create a storage of bibliographic references
#' (electronic library) from [lib_df-class] objects.
#'
#' @param conn A database connection.
#' @param schema A character value with the name of the schema where tables
#'     will get stored. If the schema does not exist in the database, it will
#'     be created by this function.
#' @param bib A [lib_df-class] or [lib_db-class] object. If missing, an empty
#'     database will be created.
#' @param comment A character value with the comment (description) assigned to
#'     the created schema.
#' @param ... Further arguments passed among methods.
#'
#' @exportMethod bib2database
setGeneric(
  "bib2database",
  function(conn, schema, bib, ...) {
    standardGeneric("bib2database")
  }
)

#' @rdname bib2database
#' @aliases bib2database,PostgreSQLConnection,character,missing-method
setMethod(
  "bib2database", signature(
    conn = "PostgreSQLConnection",
    schema = "character", bib = "missing"
  ),
  function(conn, schema, comment = "", ...) {
    if (dbExistsTable(conn, c(schema, "main_table"))) {
      stop(paste0(
        "Table 'main_table' already existing in schema '", schema,
        "'"
      ))
    }
    if (dbExistsTable(conn, c(schema, "file_list"))) {
      stop(paste0(
        "Table 'file_list' already existing in schema '", schema,
        "'"
      ))
    }
    # Modify internal tables
    tags_bib <- bib_tags$tags_bib[bib_tags$tags_bib$field != "file", ]
    tags_bib$description <- gsub("'", "''", tags_bib$description,
      fixed = TRUE
    )
    file_list <- bib_tags$file_list
    file_list$description <- gsub("'", "''", file_list$description,
      fixed = TRUE
    )
    # Create schema
    query <- paste0("create schema if not exists \"", schema, "\"")
    # Create main table
    query_mt <- replace_idx(
      x = rep("text", nrow(tags_bib)),
      idx1 = tags_bib$field,
      idx2 = "bibtexkey",
      new = "text primary key"
    )
    query_mt <- paste(tags_bib$field, query_mt)
    query_mt <- paste0(query_mt, collapse = ",\n")
    query_mt <- paste0(c(
      paste0("create table \"", schema, "\".main_table"),
      "(", query_mt, ")"
    ), collapse = "\n")
    # Create file list
    query_fl <- replace_idx(
      x = rep("text", nrow(file_list)),
      idx1 = file_list$field,
      idx2 = c("file", "bibtexkey"),
      new = c(
        "text primary key",
        paste0(
          "text references \"", schema,
          "\".main_table (bibtexkey)"
        )
      )
    )
    query_fl <- paste(file_list$field, query_fl)
    query_fl <- paste0(query_fl, collapse = ",\n")
    query_fl <- paste0(c(
      paste0("create table \"", schema, "\".file_list"),
      "(", query_fl, ")"
    ), collapse = "\n")
    # Comment on schema
    query_comm_sc <- c(paste0(
      "comment on schema \"", schema, "\" is '",
      comment, "'"
    ))
    # Comment on tables
    query_comm_tabs <- c(paste0(
      "comment on table \"", schema, "\".",
      c("main_table", "file_list"), " is '",
      c("Main entry table", "List of stored files"), ".'"
    ))
    # Comment on columns
    query_comm_mt <- c(paste0(
      "comment on column \"", schema,
      "\".main_table.\"", tags_bib$field, "\" is '",
      tags_bib$description, "'"
    ))
    query_comm_fl <- c(paste0(
      "comment on column \"", schema,
      "\".file_list.\"", file_list$field, "\" is '",
      file_list$description, "'"
    ))
    # All query in one
    query <- c(
      query, query_mt, query_fl, query_comm_sc, query_comm_mt,
      query_comm_fl
    )
    class(query) <- c("sql", "character")
    # Send the query
    dbSendQuery(conn, query)
  }
)

#' @rdname bib2database
#' @aliases bib2database,PostgreSQLConnection,character,lib_db-method
setMethod(
  "bib2database", signature(
    conn = "PostgreSQLConnection",
    schema = "character", bib = "lib_db"
  ),
  function(conn, schema, bib, ...) {
    bib2database(conn = conn, schema = schema, ...)
    dbWriteTable(conn, c(schema, "main_table"), bib@main_table,
      row.names = FALSE, append = TRUE
    )
    dbWriteTable(conn, c(schema, "file_list"), bib@file_list,
      row.names = FALSE, append = TRUE
    )
  }
)

#' @rdname bib2database
#' @aliases bib2database,PostgreSQLConnection,character,lib_df-method
setMethod(
  "bib2database", signature(
    conn = "PostgreSQLConnection",
    schema = "character", bib = "lib_df"
  ),
  function(conn, schema, bib, ...) {
    bib <- as(bib, "lib_db")
    bib2database(conn = conn, schema = schema, bib = bib, ...)
  }
)

#' @rdname bib2database
#' @aliases bib2database,lib_db,missing,missing-method
setMethod(
  "bib2database", signature(
    conn = "lib_db",
    schema = "missing", bib = "missing"
  ),
  function(conn, ...) {
    if (is.null(conn@dir$connection)) {
      stop("Database connection is not set in input object.")
    }
    if (length(conn@dir$schema) == 0) {
      stop("Name of schema is missing in input object.")
    }
    bib2database(
      conn = conn@dir$connection, schema = conn@dir$schema,
      bib = conn, ...
    )
  }
)
