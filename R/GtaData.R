
# creator of gta_db
gta_db <- function(dsn = "GTA_SQLData") {

  stopifnot(!is.null(dsn))

  # use envir for class member storage
  class_env <- new.env()
  class_env$m_dsn <- dsn
  class_env$m_connection <- NULL

  #create the class object
  structure(class_env, class = "gta_db")

}

# Open the stock database
open_stock_db.gta_db <- function(stock_db) {

  stopifnot(inherits(stock_db, "gta_db"))
  success <- TRUE

  con_stock_db <- tryCatch( RODBC::odbcConnect(dsn = stock_db$m_dsn),
                            error = function(e) e)
  if (inherits(con_stock_db, "error")) {
    msg <- conditionMessage(con_stock_db)
    success <- FALSE
  } else {
    msg <- sprintf("Connect data source of %s successfully", stock_db$m_dsn)
    stock_db$m_connection <- con_stock_db
    success <- TRUE
  }

  cat(msg, "\n")
  return(invisible(success))
}

# Init param of stock db
init_stock_db.gta_db <- function(stock_db) {

  stopifnot(inherits(stock_db, "gta_db"))


  # set up field_name list


  # set up stock_name list

}

# Close the stock database
close_stock_db.gta_db <- function(stock_db) {

  stopifnot(inherits(stock_db, "gta_db"))
  success <- TRUE

  if (!is.null(stock_db$m_connection)) {
    suceess <- tryCatch(RODBC::odbcClose(stock_db$m_connection),
                        error = function(e) e)
    if (inherits(success, "error")) {

      # fail to close the connect
      msg <- sprintf("fail to close the connection of %s", stock_db$m_dsn)
      success <- FALSE

    } else {

      # close the connection succesfully
      msg <- sprintf("close the connection of %s successfully", stock_db$m_dsn)
      stock_db$m_connection <- NULL
      success <- TRUE

    }

    cat(msg, "\n")
  }

  return(invisible(success))

}

# List all datasets of stck_db
list_stock_tables.gta_db <- function(stock_db) {

  stopifnot(inherits(stock_db, "gta_db"))

  if (is.null(stock_db$m_connection)) {
    stop("Stock db isn't connected, try to connect db again")
  }

  db_tables <- RODBC::sqlTables(stock_db$m_connection, tableType = "TABLE")
  db_tables <- db_tables[db_tables$TABLE_SCHEM == "dbo", "TABLE_NAME" ]

  return(db_tables)
}


# get one dataset from stock_db
get_table_dataset <- function(con_stock_db, table_name, quietly = FALSE) {

  stopifnot(!is.null(con_stock_db))

  if (missing(table_name) || !is.character(table_name) ) {
    stop("Table name must be character string")
  }

  # Fetech datasets from datatables

  ds_result <- tryCatch (RODBC::sqlFetch(con_stock_db, table_name), error = function(e) e)
  if (inherits(ds_result, "error")) {
    msg <- conditionMessage(ds_result)
    ds_result <- NULL
  } else {
    msg <- sprintf("get data from %s successfully", table_name)
    colnames(ds_result) <- tolower(colnames(ds_result))
  }

  if (!quietly) {
    cat(msg,"\n")
  }


  return(ds_result)
}

# fetch many datasets from stock_db
fetch_table_dataset <- function(con_stock_db, table_list) {

  stopifnot(!is.null(con_stock_db), length(table_list) != 0)

  result_table_list <- list(length(table_list))
  for (table_index in seq_along(table_list)) {

    the_table <- table_list[table_index]
    # get stock data for specified stock
    ds_result <- get_table_dataset(con_stock_db = con_stock_db, table_name = the_table)

    # keep the result_ts in GlobalEnv for debug check
    if (!is.null(ds_result)) {

      # get table successfully
      ds_name <- sprintf("ds_%s.df", the_table)
      #assign(ds_name, ds_result, pos = .GlobalEnv)
      assign(ds_name, ds_result, pos = parent.frame())
      result_table_list[table_index] <- ds_name
    } else {

      # fail to get table
      result_table_list[table_index] <- NULL

    }

  }

  return( result_table_list)

}

# Get a timeseries of stock data for specified stock from table datasets
get_stock_dataset <- function(ds_source.df, stock_cd, target_field, stkcd_field = "stkcd", date_field="trdmnt", tseries_type = c("timeSeries", "xts"), debug = FALSE) {

  # Validate parama
  if (is.null(ds_source.df) || missing(stock_cd) || missing(target_field) || missing(date_field)) {
    stop("ds_source.df, stock_cd, target_field, date_field  mustn't be null")
  }

  # Check whether the datafields existed
  field_list <- c(stkcd_field, target_field, date_field)
  if (!all(field_list %in% names(ds_source.df))) {
    error_fields <- NULL
    for (field_name in field_list) {
      if (!field_name %in% names(ds_source.df)) {
        error_fields <- ifelse(is.null(error_fields), field_name, paste(error_fields, field_name, sep = ","))
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
    result_ts <- timeSeries::timeSeries(ds_stock_data.df[target_field], zoo::as.Date(zoo::as.yearmon(ds_stock_data.df[[date_field]])))
    colnames(result_ts) <- stkcd_string

  } else {
    # xts data series
    ds_name <- sprintf("ds_%s_%s.xts", stkcd_string, target_field)
    result_ts <- xts::xts(ds_stock_data.df['target_field'], order.by = zoo::as.Date(zoo::as.yearmon(ds_stock_data.df[[date_field]])))
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


