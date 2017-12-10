# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

#' Basic arithmeatic functions
#'
#' @param x, y numeric vectors
#' @name arith
NULL


hello <- function() {
  print("Hello, world!")
}

#' Add together two numbers.
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' add(1, 1)
#' add(10, 1)
add <- function(x, y) {
  x + y
}

#' @rdname  add
add2 <- function(x, y ) x + y

#' @rdname  arith
add3 <- function(x, y ) x +y

#' Sum of vector elements.
#'
#' \code{sum} returns the sum of all the values present in its arguments.
#'
#' This is a generic function: methods can be defined for it directly
#' or via the \code{\link{Summary}} group generic. For this to work properly,
#' the arguments \code{...} should be unnamed, and dispatch is on the
#' first argument.
#'
#' @param ... Numeric, complex, or logical vectors.
#' @param na.rm A logical scalar. Should missing values (including NaN)
#' be removed?
#' @return If all inputs are integer and logical, then the output
#' will be an integer. If integer overflow
#' \url{http://en.wikipedia.org/wiki/Integer_overflow} occurs, the output
#' will be NA with a warning. Otherwise it will be a length-one numeric or
#' complex vector.
#'
#' Zero-length vectors have sum 0 by definition. See
#' \url{http://en.wikipedia.org/wiki/Empty_sum} for more details.
#' @examples
#' sum(1:10)
#' sum(1:5, 6:10)
#' sum(F, F, F, T, T)
#'
#' sum(.Machine$integer.max, 1L)
#' sum(.Machine$integer.max, 1)
#'
#' \dontrun{
#' sum("a")
#' }
sum <- function(..., na.rm = TRUE) {}

#' Test foo function.
#'
#' @param a This is the first argument
foo <- function(a) a + 10

#' Test bar function.
#'
#' @param b This is the second argument
#' @inheritParams foo
bar <- function(a, b) {
  foo(a) * 10
}

#' Foo bar generic
#'
#' @param x Object to foo.
foobar <- function(x) UseMethod("foobar")
#' @describeIn foobar Difference between the mean and the median
foobar.numeric <- function(x) abs(mean(x) - median(x))
#' @describeIn foobar First and last values pasted together in a string.
foobar.character <- function(x) paste0(x[1], "-", x[length(x)])

tabular <- function(df, ...) {
  stopifnot(is.data.frame(df))
  align <- function(x) if (is.numeric(x)) "r" else "l"
  col_align <- vapply(df, align, character(1))
  cols <- lapply(df, format, ...)
  contents <- do.call("paste",
                      c(cols, list(sep = " \\tab ", collapse = "\\cr\n ")))
  paste("\\tabular{", paste(col_align, collapse = ""), "}{\n ",
        contents, "\n}\n", sep = "")
}

# cat(tabular(mtcars[1:5, 1:5]))
#> \tabular{rrrrr}{
#> 21.0 \tab 6 \tab 160 \tab 110 \tab 3.90\cr
#> 21.0 \tab 6 \tab 160 \tab 110 \tab 3.90\cr
#> 22.8 \tab 4 \tab 108 \tab 93 \tab 3.85\cr
#> 21.4 \tab 6 \tab 258 \tab 110 \tab 3.08\cr
#> 18.7 \tab 8 \tab 360 \tab 175 \tab 3.15
#> }
