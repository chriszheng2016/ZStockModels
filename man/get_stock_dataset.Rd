% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GtaData.R, R/StockData.R
\name{get_stock_dataset.gta_db}
\alias{get_stock_dataset.gta_db}
\alias{get_stock_dataset}
\title{Get a dataset of a list of stock_cd from a table in stock_db}
\usage{
\method{get_stock_dataset}{gta_db}(stock_db, table_name, stock_cd_list = NULL)

get_stock_dataset(stock_db, table_name, stock_cd_list)
}
\arguments{
\item{stock_db}{a stock database object to operate}

\item{table_name}{name of target table}

\item{stock_cd_list}{a list of stock cd, default value of NULL means
all stock data will be returned}
}
\value{
A data frame on success, or NULL
}
\description{
Generic function to get a dataset of a list of stock_cd from table in stock
}
\section{Methods (by class)}{
\itemize{
\item \code{gta_db}: get a dataset of a list of stock_cd from table
in a database of gta_db class
}}

\examples{
ds_trd_dalyr.df <- get_stock_dataset(stock_db,
                            table_name = "TRD_Dalyr_日个股回报率",
                            stock_cd_list = c("600066", "000550"))
}
