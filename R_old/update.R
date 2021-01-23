#' @name sql_delete
#' 
#' @title Write query and execute it for deleted entries
#' 
#' @param conn A connection established by [dbConnect()].
#' @param obj A comparison object created by `compare_df()`.
#' @param name A character vector with the names of schema and table in
#'     PostgreSQL.
#' @param key A character value. The name of the primary key in the table.
#' @param ... Further arguments (not yet used).
#' 
#' @keywords internal
#' 
sql_delete <- function(conn, obj, name, key, ...) {
	Query <- paste0("DELETE FROM \"",
			paste0(name, collapse="\".\""),
			"\"\n",
			"WHERE \"", key,"\" IN ('", paste0(obj$deleted, collapse="','"),
			"');")
	dbSendQuery(conn, Query)
}

#' @name sql_update
#' 
#' @title Write query and execute it for updates
#' 
#' @description 
#' For arguments, see `sql_delete()`.
#' 
#' @keywords internal
#' 
sql_update <- function(conn, obj, name, key, ...) {
	for(i in rownames(obj$updated)) {
		Query <- paste0("UPDATE \"", paste0(name, collapse="\".\""), "\"\n",
				"SET \"", with(obj,
						paste0(paste0(colnames(updated)[updated[i,]],
										"\" = '", new_vals[i,updated[i,]]),
								collapse="', \n\"")), "'\n",
				"WHERE \"", key, "\" = '", i, "';")
		dbSendQuery(conn, Query)
	}
}

#' @name update
#' 
#' @rdname update
#' 
#' @title Compare Bibtex-files with Postgres databases and update
#' 
#' @description 
#' When changes done in the Bibtex duplicated file, they can be briefly
#' displayed before export.
#' 
#' In `update_pg` actions 'delete', 'add', and 'update' have to be accordingly
#' set as `TRUE`, otherwise only `print_report()` will be executed.
#' 
#' @param db Either a data frame imported by [read_pg()] or a connection
#'     established by [dbConnect()] to a reference database.
#' @param bib Either a data frame imported by [read_bib()] or a path to a bibtex
#'     file. This data set represent an updated version of 'db'.
#' @param print_only Logical value indicating whether the outcome will be only
#'     printed in the console or stored as a list.
#' @param name Character value indicating the name of the schema in Postgres.
#'     This argument is passed to [read_pg()].
#' @param db_args List of named arguments passed to [read_pg()].
#' @param bib_args List of named arguments passed to [read_bib()].
#' @param get_files Logical value indicating whether a list of files should be
#'     extracted from 'bib' or not. If `TRUE`, then function [get_files()] will
#'     be applied.
#' @param main_table Character value indicating the name of main table in
#'     Postgres (see [read_pg()]).
#' @param file_list Character value indicating the name of file list table in
#'     Postgres (see [read_pg()]).
#' @param delete Logical value indicating whether missing entries in 'bib' have
#'     to be deleted in 'db'.
#' @param add Logical value indicating whether new entries in 'bib' have to be
#'     inserted in 'db'.
#' @param update Logical value indicating whether entries modified in 'bib' have
#'     to be updated in 'db'.
#' @param ... Further arguments passed among methods.
#' 
#' @method update PostgreSQLConnection
#' @export
#' 
update.PostgreSQLConnection <- function(object, revision, key = "bibtexkey",
		name, db_args = list(), delete = FALSE, add = FALSE,
		update = FALSE, main_table = "main_table", file_list = "file_list",
		...) {
	db_tab <- do.call(read_pg, c(list(conn = object, name = name), db_args))
	Comp_obj <- compare_df(x = object, y = revision)
	if(all(!c(delete, add, update)))
		print(Comp_obj) else {
		db_fl <- file_list(db_tab)
		db_tab <- db_tab[ ,colnames(db_tab) != "file"]
		bib_fl <- file_list(revision)
		revision <- revision[ ,!colnames(revision) != "file"]
		
		# TODO: use compare_df in this package before
		
		
		Comp_mt <- biblio:::compare_df(db_tab, revision, "bibtexkey")
		Comp_fl <- biblio:::compare_df(db_fl, bib_fl, "file")
		if(add) {
			if(nrow(Comp_mt$added) > 0) {
				class(Comp_mt$added) <- "data.frame"
				pgInsert(object, c(name, main_table), Comp_mt$added, ...)
			}
			if(nrow(Comp_fl$added) > 0) {
				class(Comp_fl$added) <- "data.frame"
				pgInsert(object, c(name, file_list), Comp_fl$added, ...)
			}
		}
		if(delete) {
			if(length(Comp_fl$deleted) > 0)
				biblio:::sql_delete(object, Comp_fl, c(name, file_list),
						"file")
			if(length(Comp_mt$deleted) > 0)
				biblio:::sql_delete(object, Comp_mt, c(name, main_table),
						"bibtexkey")
		}
		if(update) {
			if(nrow(Comp_mt$updated) > 0) {
				for(i in colnames(Comp_mt$new_vals))
					Comp_mt$new_vals[,i] <- gsub("'", "''",
							Comp_mt$new_vals[,i], fixed=TRUE)
				biblio:::sql_update(object, Comp_mt, c(name, main_table),
						"bibtexkey")
			}
			if(nrow(Comp_fl$updated) > 0) {
				for(i in colnames(Comp_fl$new_vals))
					Comp_fl$new_vals[,i] <- gsub("'", "''",
							Comp_fl$new_vals[,i], fixed=TRUE)
				biblio:::sql_update(object, Comp_fl, c(name, file_list),
						"file")
			}
		}
	}
	if(any(c(add, delete, update)))
		message("DONE!")
}
