
# S3 generic function for stock_db

# creator of stock_db
stock_db <- function(stock_db_class, ...) {

  # build class object
  class_object <- stock_db_class(...)

  return(class_object)

}


# Open stock database
open_stock_db <- function(stock_db) {
  UseMethod("open_stock_db")
}

# Close the stock database
close_stock_db <- function(stock_db) {
  UseMethod("close_stock_db")
}

# Init param of stock db
init_stock_db <- function(stock_db) {
  UseMethod("init_stock_db")
}


# List all datasets of stck_db
list_stock_tables <- function(stock_db) {
  UseMethod("list_stock_tables")
}


# get one dataset from stock_db
get_table_dataset <- function(stock_db, table_name, quietly = FALSE) {
  UseMethod("get_table_dataset")
}


# fetch many datasets from stock_db
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


# stock_field class -------------------------------------------------------

# stock_field class creator
stock_field_list <- function(stock_db) {
  UseMethod("stock_field_list")
}

# translate field code to field name for table fields
code2name.stock_field_list <- function(x, code) {

  stopifnot(inherits(x, "stock_field_list") ,!is.null(code))

  name <- x$field_name[x$field_code == code]

  return(name)

}

# translate field name to field code for table fields
name2code.stock_field_list <- function(x, name) {

  stopifnot(inherits(x, "stock_field_list") ,!is.null(name), is.character(name))

  code <- x$field_code[x$field_name == name]

  return(code)

}


# stock_name_list ---------------------------------------------------------


# stock_name_list class creator
stock_name_list <- function(stock_db) {
  UseMethod("stock_name_list")
}

# translate stock code to stock name for stock name list
code2name.stock_name_list <- function(x, code) {

  stopifnot(inherits(x, "stock_name_list") ,!is.null(code))

  name <- x$stock_name[x$stock_code == code]

  return(name)

}

# translate stock name to stock code for stock name list
name2code.stock_name_list <- function(x, name) {

  stopifnot(inherits(x, "stock_name_list") ,!is.null(name), is.character(name))

  code <- x$stock_code[x$stock_name == name]

  return(code)

}

# S3 generic function to translate code to name
code2name <- function(x, code) {
  UseMethod("code2name")
}

# S3 generic function to translate name to code
name2code <- function(x, name) {
  UseMethod("name2code")
}







