% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_pg.R
\name{write_pg}
\alias{write_pg}
\alias{write_pg.lib_df}
\title{Writing References from Data Frame to a PostgreSQL Schema}
\usage{
write_pg(x, ...)

\method{write_pg}{lib_df}(
  x,
  conn,
  name,
  main_table = "main_table",
  file_list = "file_list",
  match_cols = FALSE,
  overwrite = FALSE,
  ...
)
}
\arguments{
\item{x}{A \linkS4class{lib_df} object containing the references to be imported.}

\item{...}{Further arguments passed to pgInsert.}

\item{conn}{A connection established with \code{\link[=dbConnect]{dbConnect()}}.}

\item{name}{A character value with the name of the schema.}

\item{main_table}{A character value indicating the name of the main table in
the database.}

\item{file_list}{A character value indicating the name of the file list in
the database.}

\item{match_cols}{A logical value indicating whether columns in 'x' should be
filtered to only matching ones (regarding the written database) or not.}

\item{overwrite}{A logical value indicating whether existing tables should be
overwritten or not.}
}
\description{
Required PostgreSQL tables will be created in a selected schema within an
existing database and populated with the entries of a \linkS4class{lib_df} object.
}
