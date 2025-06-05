#' @name check_files
#' @rdname check_files
#'
#' @title Check for file existence
#'
#' @description
#' Documents and other associated files are assumed in JabRef to exist in a
#' single folder (here indicated by 'path') and this function cross-check their
#' occurrence.
#'
#' @param obj A [lib_db-class] object including the path.
#' @param ... Further arguments passed among methods.
#'
#' @example examples/check_files.R
#'
#' @export
check_files <- function(obj, ...) {
  UseMethod("check_files", obj)
}

#' @rdname check_files
#' @method check_files lib_db
#' @export
check_files.lib_db <- function(obj, ...) {
  if (length(obj@dir$folder) == 0) {
    stop("The path to the files at 'obj@dir$folder' is not set yet.")
  }
  if (!dir.exists(obj@dir$folder)) {
    stop("The path set at 'obj@dir$folder' is wrong.")
  }
  in_folder <- list.files(obj@dir$folder)
  OUT <- list()
  OUT$not_in_db <- in_folder[!in_folder %in% obj@file_list$file]
  OUT$not_in_folder <- obj@file_list$file[!obj@file_list$file %in% in_folder]
  if (length(OUT$not_in_db) > 0) {
    cat(paste0(
      "## Files not included in database:\n   '",
      paste0(OUT$not_in_db, collapse = "'\n'"), "'\n\n"
    ))
  }
  if (length(OUT$not_in_folder) > 0) {
    cat(paste0(
      "## Files missing in the local folder:\n   '",
      paste0(OUT$not_in_folder, collapse = "'\n'"), "'\\n"
    ))
  }
  if (length(OUT$not_in_db) == 0 & length(OUT$not_in_folder) == 0) {
    cat("## Everything OK!\n")
  }
  invisible(OUT)
}
