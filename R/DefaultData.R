
# default functions for S3 generic functions of stock db ------------------------------


# default function of open stock database
open_stock_db.default <- function(stock_db) {
  "Unknow class"
}


# default function to close stock database
close_stock_db <- function(stock_db) {
  "Unknow class"
}

# default function to init param of stock db
init_stock_db.default <- function(stock_db) {
  "Unknow class"
}

# default function to transalte name to code
name2code <- function(x, name, ...) {
  "Unknow class"
}

# default function to transalte code to name
code2name <- function(x, code, ...) {
  "Unknow class"
}

# default function to list all datasets of stck_db
list_stock_tables.default <- function(stock_db) {
  "Unknow class"
}


# default function to get one dataset from stock_db
get_table_dataset.default <- function(stock_db, table_name, quietly = FALSE) {
  "Unknow class"
}


# default function to fetch many datasets from stock_db
fetch_table_dataset.default <- function(stock_db, table_list) {
  "Unknow class"
}

# default function to create stock_field_list
stock_field_list.default <- function(stock_db) {
  "Unknow class"
}

# default function to create stock_name_list
stock_name_list.default <- function(stock_db) {
  "Unknow class"
}

# default function to translate code to name
code2name.default <- function(x, code) {
  "Unknow class"
}

#default function to translate name to code
name2code.default <- function(x, name) {
  "Unknow class"
}
