#' @name db2lib_db
#'
#' @rdname db2lib_db
#'
#' @title Read bibliographic databases from PostgreSQL
#'
#' @description
#' Databases tabulated in PostgreSQL will be imported into a [lib_db-class]
#' object.
#'
#' @param conn A connection retrieved either by [dbConnect()]
#'     or [connect_db()].
#' @param schema A character value with the name of the schema containing the
#'     main table and the file list. If the length is 2, the first value will be
#'     assumed as the schema for the main table and the second, for the file
#'     list. If the length is 1, the value will be recycled.
#' @param file_folder A character value showing the path to the linked documents
#'     (i.e. PDF files of publications).
#' @param simplify Logical value indicating whether empty columns should be
#'     skipped from output or not.
#' @param ... Further arguments passed among methods.
#'
#' @return A [lib_db-class] object.
#'
#' @export
db2lib_db <- function(conn, ...) {
  UseMethod("db2lib_db", conn)
}

#' @rdname db2lib_db
#' @method db2lib_db PostgreSQLConnection
#' @export
db2lib_db.PostgreSQLConnection <- function(conn, schema, file_folder,
                                           simplify = FALSE, ...) {
  # Check existence of electronic references
  if (!dbExistsTable(conn, c(schema, "main_table"))) {
    stop(paste0(
      "The requested electronic library ",
      "does not exist in the schema '", schema, "'."
    ))
  }
  # Create new object and add directions
  Refs <- new("lib_db")
  Refs@dir$connection <- conn
  Refs@dir$schema <- schema
  if (!missing(file_folder)) {
    Refs@dir$folder <- file_folder
  }
  # Add content
  Refs@main_table <- {
    mt <- dbReadTable(conn, c(schema, "main_table"))
    if (simplify) {
      mt <- mt[, apply(mt, 2, function(x) !all(is.na(x)))]
    }
    class(mt) <- c("lib_df", "data.frame")
    mt
  }
  file_list <- dbReadTable(conn, c(schema, "file_list"))
  if (!is.null(file_list)) {
    Refs@file_list <- file_list
  }
  return(Refs)
}
