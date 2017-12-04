
m_stock_name_list = list()
m_field_name_list = list()


# Open the stock database
open_stock_db <- function(dsn = "GTA_SQLData") {

  stopifnot(!is.null(dsn))

  con_stock_db <- RODBC::odbcConnect(dsn = "GTA_SQLData")


  return(con_stock_db)
}

# Init param of stock db
init_stock_db <- function(con_stock_db, env) {

  stopifnot(!is.null(con_stock_db))

  # set up field_name list


  # set up stock_name list

}

# Close the stock database
close_stock_db <- function(con_stock_db) {

  if(!is.null(con_stock_db)) {
    RODBC::odbcClose(con_stock_db)
    }
}

# List all datasets of stck_db
list_stock_tables <- function(con_stock_db) {

  stopifnot(!is.null(con_stock_db))

  db_tables <- RODBC::sqlTables(con_stock_db, tableType = "TABLE")
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

# S3 generic function to translate code to name
code2name <- function(x, code) {
  UseMethod("code2name")
}

# S3 generic function to translate name to code
name2code <- function(x, name) {
  UseMethod("name2code")
}

# default function to translate code to name
code2name.default <- function(x, code) {
  "Unknow class"
}

#default function to translate name to code
name2code.default <- function(x, name) {
  "Unknow class"
}


# stock_field_list class -------------------------------------------------------

# stock_field class creator
stock_field_list <- function(con_stock_db) {

  stopifnot(!is.null(con_stock_db))

  field_list.df <- read.csv("R/gta_fieldname.csv", stringsAsFactors = FALSE)
  field_name_list <- field_list.df[, c(1, 2)]
  colnames(field_name_list) <- c("field_code", "field_name")
  field_name_list["field_code"] <- lapply(field_name_list["field_code"], tolower)
  structure(field_name_list, class = "stock_field_list")

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


# stock_name_list class ---------------------------------------------------



# stock_name_list class creator
stock_name_list <- function(con_stock_db) {

  stopifnot(!is.null(con_stock_db))

  #build stock_name_list

  ds_trd_company.df <- get_table_dataset(con_stock_db,
                                         table_name = "TRD_Co_公司基本情况")

  stock_name_list <- ds_trd_company.df[,c("stockcd", "sktnme")]
  names(stock_name_list) <- c("stock_code","stock_name")
  structure(stock_name_list, class = "stock_name_list")

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







