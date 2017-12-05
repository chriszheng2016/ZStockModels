
# creator of gta_db
gta_db <- function(dsn = "GTA_SQLData") {

  stopifnot(!is.null(dsn))

  # use envir for class member storage
  class_env <- new.env()
  class_env$dsn <- dsn
  class_env$connection <- NULL

  #create the class object
  structure(class_env, class = "gta_db")

}

# Open the stock database
open_stock_db.gta_db <- function(stock_db) {

  stopifnot(inherits(stock_db, "gta_db"))
  success <- TRUE

  con_stock_db <- tryCatch( RODBC::odbcConnect(dsn = stock_db$dsn),
                            error = function(e) e)
  if (inherits(con_stock_db, "error")) {
    msg <- conditionMessage(con_stock_db)
    success <- FALSE
  } else {
    msg <- sprintf("Connect data source of %s successfully", stock_db$dsn)
    stock_db$connection <- con_stock_db
    success <- TRUE
  }

  cat(msg, "\n")
  return(invisible(success))
}

# Init param of stock db
init_stock_db.gta_db <- function(stock_db) {

  stopifnot(inherits(stock_db, "gta_db"))


  # set up field_name list
  stock_db$stock_field_list <- stock_field_list(stock_db)


  # set up stock_name list
  #stock_db$stock_name_list <- stock_name_list(stock_db)
  stock_db$stock_name_list <- NULL

}

# Close the stock database
close_stock_db.gta_db <- function(stock_db) {

  stopifnot(inherits(stock_db, "gta_db"))
  success <- TRUE

  if (!is.null(stock_db$connection)) {
    success <- tryCatch(RODBC::odbcClose(stock_db$connection),
                        error = function(e) e)
    if (inherits(success, "error")) {

      # fail to close the connect
      msg <- sprintf("fail to close the connection of %s", stock_db$dsn)
      success <- FALSE

    } else {

      # close the connection succesfully
      msg <- sprintf("close the connection of %s successfully", stock_db$dsn)
      stock_db$connection <- NULL
      success <- TRUE

    }

    cat(msg, "\n")
  }

  return(invisible(success))

}

# List all datasets of stck_db
list_stock_tables.gta_db <- function(stock_db) {

  stopifnot(inherits(stock_db, "gta_db"))

  if (is.null(stock_db$connection)) {
    stop("Stock db isn't connected, try to connect db again")
  }

  db_tables <- RODBC::sqlTables(stock_db$connection, tableType = "TABLE")
  db_tables <- db_tables[db_tables$TABLE_SCHEM == "dbo", "TABLE_NAME" ]

  return(db_tables)
}


# get one dataset from stock_db
get_table_dataset.gta_db <- function(stock_db, table_name, quietly = FALSE) {

  # Validate params
  stopifnot(inherits(stock_db, "gta_db"))

  if (is.null(stock_db$connection)) {
    stop("Stock db isn't connected, try to connect db again")
  }

  if (missing(table_name) || !is.character(table_name) ) {
    stop("Table name must be character string")
  }

  # Fetech datasets from datatables
  ds_result <- tryCatch(RODBC::sqlFetch(stock_db$connection, table_name),
                         error = function(e) e)
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

  return(invisible(ds_result))
}

# fetch many datasets from stock_db
fetch_table_dataset.gta_db <- function(stock_db, table_list) {

  # validate params
  stopifnot(inherits(stock_db, "gta_db"))

  if (is.null(stock_db$connection)) {
    stop("Stock db isn't connected, try to connect db again")
  }

  if (missing(table_list) || length(table_list) == 0 ) {
    stop("table_list must contain one table at least")
  }

  # get datasets from stock db
  result_table_list <- list(length(table_list))
  for (table_index in seq_along(table_list)) {

    the_table <- table_list[table_index]
    # get stock data for specified stock
    ds_result <- get_table_dataset.gta_db(stock_db,
                                          table_name = the_table)

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

# stock_field_list class of gta -------------------------------------------------------

# stock_field class creator

stock_field_list.gta_db <- function(stock_db) {

  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))

  # build field_name list
  field_name_list <- NULL
  field_list.df <- read.csv("R/gta_fieldname.csv", stringsAsFactors = FALSE)
  field_list <- field_list.df[, c(1, 2)]
  colnames(field_list) <- c("field_code", "field_name")
  field_list["field_code"] <- lapply(field_list["field_code"], tolower)
  field_name_list <- structure(field_list, class = "stock_field_list")

  return(field_name_list)

}


# stock_name_list class of gta ---------------------------------------------------


# stock_name_list class creator
stock_name_list.gta_db <- function(stock_db) {

  stopifnot(!is.null(stock_db), inherits(stock_db, "gta_db"))

  #build stock_name_list
  stock_name_list <- NULL

  ds_trd_company.df <- get_table_dataset.gta_db(stock_db,
                                         table_name = I("TRD_Co_公司基本情况"))
  if (!is.null(ds_trd_company.df)) {
    stock_name_list <- ds_trd_company.df[,c("stockcd", "sktnme")]
    names(stock_name_list) <- c("stock_code","stock_name")
    stock_name_list <- structure(stock_name_list, class = "stock_name_list")
  } else {
    stop("can't gett data from stock db")
  }

  return(stock_name_list)

}



