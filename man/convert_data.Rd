% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{convert_data}
\alias{convert_data}
\title{This function is a wrapper for \code{\link[utils:type.convert]{utils::type.convert()}} to keep column labels}
\usage{
convert_data(dataset)
}
\arguments{
\item{dataset}{\verb{[data.frame]}

Single data.frame to convert the column data types}
}
\value{
data.frame with converted data types of data.frame columns
}
\description{
Converts data types of data.frame columns to logical, integer, numeric, complex, character or factor as appropriate.
Re-assigns column labels after converting data type.
}
