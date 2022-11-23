#' @name update_data
#' @rdname update_data
#'
#' @title Update database from edited electronic libraries
#'
#' @description
#' Electronic libraries managed as bibTeX files can be stored and synchronized
#' in a relational database.
#'
#' @param object A connection to a database.
#' @param revision An electronic library, either as [lib_df-class] or
#'     [lib_db-class] object.
#' @param schema A character value indicating the schema containing the
#'     respective tables.
#' @param key Not used in these methods.
#' @param add,delete,update A logical value indicating whether the respective
#'     action have to be taken or not. If all `FALSE`, only a comparison by
#'     [compare_df()] will be carried out.
#' @param ... Further arguments passed among methods.
#'
#' @aliases update_data,PostgreSQLConnection,lib_db,missing-method
#' @exportMethod update_data
setMethod(
  "update_data",
  signature(
    object = "PostgreSQLConnection",
    revision = "lib_db",
    key = "missing"
  ),
  function(object, revision, schema, add = FALSE, delete = FALSE,
           update = FALSE, ...) {
    if (all(!c(add, delete, update))) {
      return(compare_df(x = object, y = revision, schema = schema))
    } else {
      if (update) {
        update_data(
          object = object, revision = revision@main_table,
          key = "bibtexkey", name = c(schema, "main_table"),
          update = update
        )
        update_data(
          object = object, revision = revision@file_list,
          key = "file", name = c(schema, "file_list"), update = update
        )
      }
      if (add) {
        update_data(
          object = object, revision = revision@main_table,
          key = "bibtexkey", name = c(schema, "main_table"), add = add
        )
        update_data(
          object = object, revision = revision@file_list,
          key = "file", name = c(schema, "file_list"), add = add
        )
      }
      if (delete) {
        update_data(
          object = object, revision = revision@file_list,
          key = "file", name = c(schema, "file_list"), delete = delete
        )
        update_data(
          object = object, revision = revision@main_table,
          key = "bibtexkey", name = c(schema, "main_table"),
          delete = delete
        )
      }
    }
  }
)

#' @rdname update_data
#' @aliases update_data,PostgreSQLConnection,lib_df,missing-method
setMethod(
  "update_data",
  signature(
    object = "PostgreSQLConnection",
    revision = "lib_df",
    key = "missing"
  ),
  function(object, revision, schema, ...) {
    revision <- as(revision, "lib_db")
    update_data(object = object, revision = revision, schema = schema, ...)
  }
)

#' @rdname update_data
#' @aliases update_data,lib_db,missing,missing-method
setMethod(
  "update_data",
  signature(
    object = "lib_db",
    revision = "missing",
    key = "missing"
  ),
  function(object, ...) {
    if (is.null(object@dir$connection)) {
      stop("Database connection is not set in input object.")
    }
    if (length(object@dir$schema) == 0) {
      stop("Name of schema is missing in input object.")
    }
    update_data(
      object = object@dir$connection, revision = object,
      schema = object@dir$schema, ...
    )
  }
)
