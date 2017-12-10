

# stock_db class ----------------------------------------------------------


#' S3 generic function for stock_db
#
#' Generic function of operating stock database
#'
#' A group generic function to deal with a stock_db objec
#'
#' @name stock_db_operations
#' @examples
#' stock_db <- stock_db(gta_db, "GTA_SQLData")
#' open_stock_db(stock_db)
#' close_stock_db(stock_db)
NULL


#' Class creator of stock_db class
#'
#' Generic class creator of stock_db class
#'
#' @param stock_db_class class name of specific class of stock database,
#' e.g. gta_db
#' @param ... addition params used by specific class of stock database(e.g. a dsn string
#' "GTA_SQLData")
#'
#' @return a object of class of stock db
#' @export
#'
#' @examples
#' stock_db <- stock_db(gta_db, "GTA_SQLData")
#'
stock_db <- function(stock_db_class, ...) {

  # build class object
  class_object <- stock_db_class(...)

  return(class_object)

}

#' Open stock database
#'
#' Generic function to open a stock database
#'
#' @param stock_db a stock database object to operate
#'
#' @return TRUE if success, else FALSE
#' @export
#' @examples
#' stock_db <- stock_db(gta_db, "GTA_SQLData")
#' open_stock_db(stock_db)
#' close_stock_db(stock_db)
open_stock_db <- function(stock_db) {
  UseMethod("open_stock_db")
}

#' Close the stock database
#'
#' Generic function to close a stock database
#'
#' @param stock_db a stock database object to operate
#'
#' @return TRUE if success, else FALSE
#' @export
#'
#' @examples
#' stock_db <- stock_db(gta_db, "GTA_SQLData")
#' open_stock_db(stock_db)
#' close_stock_db(stock_db)
close_stock_db <- function(stock_db) {
  UseMethod("close_stock_db")
}


#' Init param of stock db
#'
#' Generic function to initiate params of a stock database
#'
#' @param stock_db a stock database object to operate
#'
#' @return TRUE if success, else FALSE
#' @export
#'
#' @examples
#' stock_db <- stock_db(gta_db, "GTA_SQLData")
#' open_stock_db(stock_db)
#' init_stock_db(stock_db)
init_stock_db <- function(stock_db) {
  UseMethod("init_stock_db")
}

#' List all tables of stcok_db
#'
#' Generic function to list all tables of stock_db
#'
#' @param stock_db  a stock database object to operate
#'
#' @return a vectors of characters of table names
#' @export
#'
#' @examples
#' stock_db <- stock_db(gta_db, "GTA_SQLData")
#' open_stock_db(stock_db)
#' init_stock_db(stock_db)
#' list_stock_tables(stock_db)
list_stock_tables <- function(stock_db) {
  UseMethod("list_stock_tables")
}


#' Get one dataset from stock_db
#'
#' Generic function to get one dataset from stock_db
#'
#' @param stock_db    a stock database object to operate
#' @param table_name  name of target table
#'
#' @return A data frame on success, or NULL
#' @export
#'
#' @examples
#' ds_trd_mnth.df <- get_table_dataset(stock_db, table_name = "TRD_Mnth_月个股回报率")
get_table_dataset <- function(stock_db, table_name) {
  UseMethod("get_table_dataset")
}


#' Fetch many datasets from stock_db
#'
#' Generic function to fetch many datasets from stock_db
#'
#' @param stock_db   a stock database object to operate
#' @param table_list a character vector of table names
#'
#' @return A list of names of table fetched successfully
#' @export
#'
#' @examples
#' stock_cd_list <- c(600066,000550, 600031, 000157,000651, 000333)
#' ds_stocks_mretnd.fts <- fetch_stock_dataset(ds_source.df = ds_trd_mnth.df, stock_cd_list = stock_cd_list, target_field = "mretnd", date_field = "trdmnt")

fetch_table_dataset <- function(stock_db, table_list) {
  UseMethod("fetch_table_dataset")
}

# Get a timeseries of stock data for specified stock from table datasets
get_stock_dataset <- function(ds_source.df,
                              stock_cd,
                              target_field,
                              stkcd_field = "stkcd",
                              date_field="trdmnt",
                              tseries_type = c("timeSeries", "xts"),
                              debug = FALSE) {

  # Validate parama
  if (is.null(ds_source.df) || missing(stock_cd)
      || missing(target_field) || missing(date_field)) {
    stop("ds_source.df, stock_cd, target_field, date_field  mustn't be null")
  }

  # Check whether the datafields existed
  field_list <- c(stkcd_field, target_field, date_field)
  if (!all(field_list %in% names(ds_source.df))) {
    error_fields <- NULL
    for (field_name in field_list) {
      if (!field_name %in% names(ds_source.df)) {
        error_fields <- ifelse(is.null(error_fields),
                               field_name,
                               paste(error_fields, field_name, sep = ","))
      }
    }

    error_msg <- sprintf("%s dosen't exist in dataset ", error_fields)
    stop(error_msg)
  }

  tseries_type <- match.arg(tseries_type)
  if (is.null(tseries_type)) {
    warning("teries_type should be timeSeries or xts, set as timeSeries by default")
    tseries_type <- "timeSeries"
  }

  # Get related data of the specified stock from all stock trd_mnth data table
  ds_stock_data.df <- na.omit(ds_source.df[ds_source.df$stkcd == as.numeric(stock_cd),])

  # Build result dataset for specified stock
  stkcd_string = sprintf("%06d", stock_cd)
  result_ts = NULL
  if (tseries_type == "timeSeries") {
    # timeSeries data series
    ds_name <- sprintf("ds_%s_%s.fts", stkcd_string, target_field)
    result_ts <- timeSeries::timeSeries(ds_stock_data.df[target_field],
                                        zoo::as.Date(zoo::as.yearmon(ds_stock_data.df[[date_field]])))
    colnames(result_ts) <- stkcd_string

  } else {
    # xts data series
    ds_name <- sprintf("ds_%s_%s.xts", stkcd_string, target_field)
    result_ts <- xts::xts(ds_stock_data.df['target_field'],
                          order.by = zoo::as.Date(zoo::as.yearmon(ds_stock_data.df[[date_field]])))
    colnames(result_ts) <- stkcd_string
  }

  # Return the result timeseries of stock
  if (debug) {

    # keep the result_ts in GlobalEnv for debug check
    assign(ds_name, result_ts, pos = .GlobalEnv)
    return(get(ds_name))

  }else {

    # reutrn the result_as as ususal
    return(result_ts)
  }
}

# Get several timeseries of stocks data for multiple stocks from peroidic dataset
fetch_stock_dataset <- function(ds_source.df, stock_cd_list, ...) {


  # Validate params
  if (is.null(ds_source.df) || missing(stock_cd_list) ) {
    stop("ds_source.df, stock_cd_list mustn't be null")
  }

  stopifnot(length(stock_cd_list) != 0, length(ds_source.df) != 0)

  ds_result = NULL
  for (the_stock_cd in stock_cd_list) {

    # get stock data for specified stock
    ds_stock_data <- get_stock_dataset(ds_source.df = ds_source.df, stock_cd = the_stock_cd, ...)

    # Build the result dataset
    if (is.null(ds_result)) {
      # Set stock data as result dataset
      ds_result <- ds_stock_data
    } else {
      # Merge stock data into the result dataset
      ds_result <- merge(ds_result, ds_stock_data)
    }

  }

  return(ds_result)
}

#' Translate code into name
#'
#' Generic function to translate code into name
#'
#' @param x    a object containg code/name infomation
#' @param code a code or a vector of codes to be translated
#' @param ...  other params to be provided to underlyling functions
#'
#' @return     a name or a vector of names
#' @export
#'
#' @examples
code2name <- function(x, code, ...) {
  UseMethod("code2name")
}


#' Translate name into code
#'
#' Generic function to translate name into code
#'
#' @param x     a object containg code/name infomation
#' @param name  a name or a vector of names to be translated
#' @param ...   other params to be provided to underlyling functions
#'
#' @return      a code or a vector of codes
#' @export
#'
#' @examples
name2code <- function(x, name, ...) {
  UseMethod("name2code")
}

# stock_field_list class -------------------------------------------------------

#' Class creator of stock_field class
#'
#' Generic stock_field class creator
#'
#' @param stock_db a stock database object to provide stock_field info
#'
#' @return a object of stock_filed class
#' @export
#'
#' @examples
stock_field_list <- function(stock_db) {
  UseMethod("stock_field_list")
}

# Translate field code into field name for table fields
#' @describeIn code2name Translate code into name in a object of stock_field_list  class
#' @export
code2name.stock_field_list <- function(x, code) {

  stopifnot(inherits(x, "stock_field_list") ,!is.null(code))

  match_index = match(code, x$field_code)
  name <- x$field_name[match_index]

  return(name)

}

# Translate field name into field code for table fields
#' @describeIn name2code Translate name into code in a object of stock_field_list  class
#' @export
name2code.stock_field_list <- function(x, name) {

  stopifnot(inherits(x, "stock_field_list") ,!is.null(name), is.character(name))

  match_index = match(name, x$field_name)
  code <- x$field_code[match_index]

  return(code)

}


# stock_name_list class ---------------------------------------------------------


#' Class creator of stock_name_list class
#'
#' generic stock_name_list class creator
#'
#' @param stock_db a stock database object to provide stock_name info
#'
#' @return a object of stock_name class
#' @export
#'
#' @examples
stock_name_list <- function(stock_db) {
  UseMethod("stock_name_list")
}

# Translate stock code into stock name for stock_name_list
#' @describeIn code2name Translate code into name in a object of stock_name_list  class
#' @export
code2name.stock_name_list <- function(x, code) {

  stopifnot(inherits(x, "stock_name_list") ,!is.null(code))

  match_index = match(code, x$stock_code)
  name <- x$stock_name[match_index]

  return(name)

}

# Translate stock name into stock code for stock_name_list
#' @describeIn name2code Translate name into code in a object of stock_name_list  class
#' @export
name2code.stock_name_list <- function(x, name) {

  stopifnot(inherits(x, "stock_name_list") ,!is.null(name), is.character(name))

  match_index = match(name, x$stock_name)
  code <- x$stock_code[match_index]

  return(code)

}









