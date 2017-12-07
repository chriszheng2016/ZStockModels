
# CONSTANT DEFINATION -----------------------------------------------------

.PACKAGE_NAME <- "ZStockModels"

# profile variable defination
# Since R devtools dosen't suport loading Chinese charaters in script file, we
# have to use reference name of varible in profile for refering Chinese table
# name in DB
.GTA_RPROFILE_DIR <- "etc"
.GTA_PROFILE_FILE <- "gta_profile.csv"
.GTA_TABLE_FIELDNAME_LIST <- "gta_fieldname_list"
.GTA_TABLE_TRD_COMPANY <- "TRD_Co"
.GTA_XXXX <- "中文字符"  # not supported


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

  # set up table name mapping for referece
  gta_profile_name <- system.file(.GTA_RPROFILE_DIR, .GTA_PROFILE_FILE, package = .PACKAGE_NAME )

  stock_db$table_fieldname_list <- .get_db_profile(gta_profile_name, .GTA_TABLE_FIELDNAME_LIST)
  stock_db$table_trd_company    <- .get_db_profile(gta_profile_name, .GTA_TABLE_TRD_COMPANY)

  # set up field_name list
  stock_db$stock_field_list <- stock_field_list(stock_db)

  # set up stock_name list
  stock_db$stock_name_list <- stock_name_list(stock_db)


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

# translate name to code for field or stock
name2code.gta_db <- function(stock_db, name, type=c("field", "stock")) {

    stopifnot(inherits(stock_db, "gta_db"), !missing(name))

    target_type <- match.arg(type)
    code = switch(target_type,
              field = name2code(stock_db$stock_field_list, name = name),
              stock = name2code(stock_db$stock_name_list, name = name)
    )

    return(code)

}

# translate code to name for field or stock
code2name.gta_db <- function(stock_db, code, type=c("field", "stock")) {

  stopifnot(inherits(stock_db, "gta_db"), !missing(code))

  target_type <- match.arg(type)
  name = switch(target_type,
                field = code2name(stock_db$stock_field_list, code = code),
                stock = code2name(stock_db$stock_name_list, code = code)
  )

  return(name)
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
  ds_result <- tryCatch(RODBC::sqlFetch(stock_db$connection, table_name,
                                        stringsAsFactors = FALSE),
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
  stopifnot(!is.null(stock_db$table_fieldname_list))

  # build field_name list
  field_name_list <- NULL
  field_list.df <- get_table_dataset.gta_db(stock_db,
                                            table_name = stock_db$table_fieldname_list)
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
  stopifnot(!is.null(stock_db$table_trd_company))

  #build stock_name_list
  stock_name_list <- NULL
  ds_trd_company.df <- get_table_dataset.gta_db(stock_db,
                                         table_name = stock_db$table_trd_company)
  if (!is.null(ds_trd_company.df)) {
    stock_name_list <- ds_trd_company.df[,c("stkcd", "stknme")]
    names(stock_name_list) <- c("stock_code","stock_name")
    stock_name_list <- structure(stock_name_list, class = "stock_name_list")
  } else {
    stop("can't get data from stock db")
  }

  return(stock_name_list)

}



