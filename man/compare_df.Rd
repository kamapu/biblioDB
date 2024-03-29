% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compare_df.R
\name{compare_df}
\alias{compare_df}
\alias{compare_df,PostgreSQLConnection,lib_db,missing-method}
\alias{compare_df,PostgreSQLConnection,lib_df,missing-method}
\alias{compare_df,lib_db,missing,missing-method}
\title{Compare database tables with electronic libraries}
\usage{
\S4method{compare_df}{PostgreSQLConnection,lib_db,missing}(x, y, schema, ...)

\S4method{compare_df}{PostgreSQLConnection,lib_df,missing}(x, y, key, ...)

\S4method{compare_df}{lib_db,missing,missing}(x, y, key, ...)
}
\arguments{
\item{x}{A \linkS4class{PostgreSQLConnection} connecting to a database.}

\item{y}{Either a \linkS4class{lib_df} or \linkS4class{lib_db} object including the
reviewed version of the electronic library.}

\item{schema}{A character value with the name of the schema in database
containing the respective tables.}

\item{...}{Further arguments passed among methods.}

\item{key}{Not required in these methods.}
}
\value{
Either a \linkS4class{comp_df} or a \linkS4class{comp_list} object, depending on the
used method.
}
\description{
Comparing database content with edited files.
}
