#' @name file_list
#' 
#' @rdname add_files
#' 
#' @title Add List of Files from Additional Table
#' 
#' @description 
#' The function `add_files<-` attempts to harmonize relational databases listing
#' files in a separated relation (data frame).
#' 
#' The reverse function is called `get_files`, where a string will be converted
#' into a data frame.
#' 
#' @param x A data frame including reference entries imported from BibTeX
#'     databases by \code{\link{read_bib}}. At least a column called 'bibtexkey'
#'     have to be included in this table.
#' @param value A data frame listing files with respective bibtexkey and
#'     MIME-Type. In this table the columns 'bibtexkey', 'file', and 'mime' are
#'     mandatory. The occurrence of all bibtexkey values in 'value' will be
#'     cross-checked and evenctually cause an error, thus you may clean this
#'     data frame before using it.
#' @param priority Character value. A keyword used as file description to define
#'     which is the main document in the entry. Files including this description
#'     will be listed first in the output bibtex file (JabRef style).
#' 
#' @return
#' In `add_files<-` the same data frame 'refs' with an inserted or updated
#' column 'file'.
#' For `get_files`, a data frame with three columns, namely 'bibtexkey', 'file',
#' 'mime', and 'description' where every file is listed in aseparated row.
#'  
#' @author Miguel Alvarez \email{kamapu78@@gmail.com}
#' 
#' @export
#' 
file_list <- function (x, ...) {
	UseMethod("file_list", x)
}

#' @rdname file_list
#' 
#' @export 
#' 
file_list.lib_df <- function(x, ...) {
	if(!"bibtexkey" %in% colnames(x))
		stop("Column 'bibtexkey' is mandatory in 'x'.")
	if(!"file" %in% colnames(x))
		stop("Column 'file' is mandatory in 'x'.")
	file_string <- strsplit(x$file, ";", fixed=TRUE)
	file_string <- data.frame(bibtexkey=rep(x$bibtexkey, sapply(file_string,
							length)),
			file=unlist(file_string),
			stringsAsFactors=FALSE)
	x <- do.call(rbind, strsplit(file_string$file, ":", fixed=TRUE))
	file_string$file <- x[,2]
	file_string$mime <- x[,3]
	file_string$description <- x[,1]
	file_string$description[file_string$description == ""] <- NA
	return(file_string[!is.na(file_string$file),])
}

#' @rdname file_list
#' 
#' @aliases file_list<-
#' 
#' @exportMethod file_list<-
#' 
setGeneric("file_list<-", function(x, ..., value)
			standardGeneric("add_files<-"))

#' @rdname file_list
#' 
#' @aliases file_list<-,lib_df,data.frame-method
#' 
setReplaceMethod("file_list", signature(x = "lib_df", value = "data.frame"),
		function(x, priority = "main text", ..., value) {
			if(!"bibtexkey" %in% colnames(x))
				stop("'bibtexkey' is a mandatory column in 'x'.")
			if(any(!c("bibtexkey", "file", "mime") %in% colnames(value)))
				stop(paste("'bibtexkey', 'file' and 'mime' are mandatory",
								"columns in 'value'."))
			if(any(!value$bibtexkey %in% x$bibtexkey))
				stop(paste("Some values of 'bibtexkey' in 'value'",
								"are not present in 'x'."))
			value <- value[order(value$description == priority,
							decreasing = TRUE),]
			value$description[is.na(value$description)] <- ""
			value$file <- with(value, paste(description, file, mime, sep = ":"))
			value <- split(value, value$bibtexkey)
			value <- do.call(rbind, lapply(value, function(x)
								c(x$bibtexkey[1], paste0(x$file,
												collapse = ";"))))
			x$file <- value[match(x$bibtexkey, value[ , 1]), 2]
			return(x)
		})
