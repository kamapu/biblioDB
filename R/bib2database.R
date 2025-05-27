#' @name bib2database
#' @rdname bib2database
#'
#' @title Create a database to store bibliographic references
#'
#' @description
#' A relational data model to create a storage of bibliographic references
#' (electronic library) from [biblio::lib_df-class] objects.
#'
#' @param conn A database connection.
#' @param schema A character value with the name of the schema where tables
#'     will get stored. If the schema does not exist in the database, it will
#'     be created by this function.
#' @param bib A [biblio::lib_df-class] or [lib_db-class] object. If missing,
#'     an empty database will be created.
#' @param comment A character value with the comment (description) assigned to
#'     the created schema.
#' @param eval A logical value, whether the resulting SQL commands should be
#'     executed or not. This may be usefull if the target is retrieving SQL
#'     scripts for further execution.
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
  function(conn, schema, comment = "", eval = TRUE, ...) {
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
    query <- character(0)
    # Create tables
    query <- c(query, paste(
      "create table", paste0(schema, ".main_table"),
      "()"
    ))
    query <- c(query, paste(
      "create table", paste0(schema, ".file_list"),
      "()"
    ))
    # Add columns to main table
    def_main_table <- bib_tags$tags_bib[, c("field", "description")]
    names(def_main_table) <- c("name", "comment")
    def_main_table$type <- "text"
    def_main_table$type[def_main_table$name == "bibtexkey"] <-
        "text primary key"
    query <- c(query, add_columns(conn,
      df = def_main_table,
      name = c(schema, "main_table"), eval = FALSE
    ))
    # Add columns to file list
    def_file_list <- bib_tags$file_list[, c("field", "description")]
    names(def_file_list) <- c("name", "comment")
    def_file_list$type <- "text"
    def_file_list <- do.call(rbind, list(def_file_list,
          data.frame(name = "file_id", comment = "Identifier for file entry.",
              type = "serial primary key")))
    query <- c(query, add_columns(conn,
      df = def_file_list,
      name = c(schema, "file_list"), eval = FALSE
    ))
    # Foreign key
    query <- c(query, paste0(
      "alter table ", schema, ".file_list\n",
      "add constraint fk_bibtexkey foreign key (bibtexkey) ",
      "references ", schema, ".main_table (bibtexkey)"
    ))
    class(query) <- c("sql", "character")
    if (eval) {
      dbSendQuery(conn, query)
      message("DONE!")
    }
    invisible(query)
  }
)





#' @rdname bib2database
#' @aliases bib2database,PostgreSQLConnection,character,lib_db-method
setMethod(
  "bib2database", signature(
    conn = "PostgreSQLConnection",
    schema = "character", bib = "lib_db"
  ),
  function(conn, schema, bib, eval = TRUE, ...) {
    query <- bib2database(conn = conn, schema = schema, eval = FALSE)
    for (i in names(bib@main_table)) {
      bib@main_table[[i]] <- gsub("'", "''", bib@main_table[[i]],
        fixed = TRUE
      )
    }
    query <- c(query, insert_rows(conn, bib@main_table, c(schema, "main_table"),
      eval = FALSE
    ))
    query <- c(query, insert_rows(conn, bib@file_list, c(schema, "file_list"),
      eval = FALSE
    ))
    class(query) <- c("sql", "character")
    if (eval) {
      dbSendQuery(conn, query)
      message("DONE!")
    }
    invisible(query)
  }
)





#' @rdname bib2database
#' @aliases bib2database,PostgreSQLConnection,character,lib_df-method
setMethod(
  "bib2database", signature(
    conn = "PostgreSQLConnection",
    schema = "character", bib = "lib_df"
  ),
  function(conn, schema, bib, eval = TRUE, ...) {
    bib <- as(bib, "lib_db")
    query <- bib2database(conn = conn, schema = schema, bib = bib, eval = FALSE)
    if (eval) {
      dbSendQuery(conn, query)
      message("DONE!")
    }
    invisible(query)
  }
)

#' @rdname bib2database
#' @aliases bib2database,lib_db,missing,missing-method
setMethod(
  "bib2database", signature(
    conn = "lib_db",
    schema = "missing", bib = "missing"
  ),
  function(conn, eval = TRUE, ...) {
    if (is.null(conn@dir$connection)) {
      stop("Database connection is not set in input object.")
    }
    if (length(conn@dir$schema) == 0) {
      stop("Name of schema is missing in input object.")
    }
    query <- bib2database(
      conn = conn@dir$connection, schema = conn@dir$schema,
      bib = conn, eval = FALSE
    )
    if (eval) {
      dbSendQuery(conn, query)
      message("DONE!")
    }
    invisible(query)
  }
)
