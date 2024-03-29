% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mod_simple_listing.R
\name{simple_listing}
\alias{simple_listing}
\alias{simple_listing_UI}
\alias{simple_listing_server}
\alias{mod_simple_listing}
\title{A module that displays a single dataset in a table form}
\usage{
simple_listing_UI(module_id)

simple_listing_server(module_id, dataset)

mod_simple_listing(dataset_disp, module_id)
}
\arguments{
\item{module_id}{\verb{[character(1)]} Unique module_id identifier. It can only contain alphanumeric characters and underscores.}

\item{dataset}{a data.framish dataset that will be shown as a table}

\item{dataset_disp}{An mm_dispatch object.}
}
\description{
This module shows a given dataset in a table form.
}
\section{Functions}{
\itemize{
\item \code{simple_listing_UI()}: UI

\item \code{simple_listing_server()}: server

\item \code{mod_simple_listing()}: module

}}
\concept{data_table}
