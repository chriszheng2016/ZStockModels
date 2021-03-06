% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GtaData.R, R/StockData.R
\name{fetch_table_dataset.gta_db}
\alias{fetch_table_dataset.gta_db}
\alias{fetch_table_dataset}
\title{Fetch many datasets from stock_db}
\usage{
\method{fetch_table_dataset}{gta_db}(stock_db, table_list)

fetch_table_dataset(stock_db, table_list)
}
\arguments{
\item{stock_db}{a stock database object to operate}

\item{table_list}{a character vector of table names}
}
\value{
A list of names of table fetched successfully
}
\description{
Generic function to fetch many datasets from stock_db
}
\section{Methods (by class)}{
\itemize{
\item \code{gta_db}: get many datasets from a database of gta_db class
}}

\examples{

}
