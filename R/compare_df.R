#' @name compare_df
#' @rdname compare_df
#'
#' @title Compare database tables with electronic libraries
#'
#' @description
#' Comparing database content with edited files.
#'
#' @param x A [PostgreSQLConnection-class] connecting to a database.
#' @param y Either a [lib_df-class] or [lib_db-class] object including the
#'     reviewed version of the electronic library.
#' @param key Not required in these methods.
#' @param schema A character value with the name of the schema in database
#'     containing the respective tables.
#' @param ... Further arguments passed among methods.
#'
#' @return
#' Either a [comp_df-class] or a [comp_list-class] object, depending on the
#' used method.
#'
#' @aliases compare_df,PostgreSQLConnection,lib_db,missing-method
#' @exportMethod compare_df
setMethod(
  "compare_df", signature(
    x = "PostgreSQLConnection", y = "lib_db",
    key = "missing"
  ),
  function(x, y, schema, ...) {
    c_list <- list()
    c_list$main_table <- compare_df(
      x = x, y = y@main_table, key = "bibtexkey",
      name = c(schema, "main_table"), ...
    )
    c_list$file_list <- compare_df(
      x = x, y = y@file_list, key = "file",
      name = c(schema, "file_list"), ...
    )
    class(c_list) <- c("comp_list", "list")
    return(c_list)
  }
)

#' @rdname compare_df
#' @aliases compare_df,PostgreSQLConnection,lib_df,missing-method
setMethod(
  "compare_df", signature(
    x = "PostgreSQLConnection", y = "lib_df",
    key = "missing"
  ),
  function(x, y, ...) {
    y <- as(y, "lib_db")
    return(compare_df(x = x, y = y, ...))
  }
)

#' @rdname compare_df
#' @aliases compare_df,lib_db,missing,missing-method
setMethod(
  "compare_df", signature(
    x = "lib_db", y = "missing",
    key = "missing"
  ),
  function(x, ...) {
    if (is.null(x@dir$connection)) {
      stop("Database connection is not set in input object.")
    }
    if (length(x@dir$schema) == 0) {
      stop("Name of schema is missing in input object.")
    }
    return(compare_df(
      x = x@dir$connection, y = x, schema = x@dir$schema,
      ...
    ))
  }
)
