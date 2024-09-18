#' @title Calculate Day of the Year (DOY)
#'
#' @description
#' \code{doy} calculates the day of the year (DOY) for a given date. January 1st is represented as 1, and December 31st is 365 (or 366 in leap years).
#'
#' @param dat A date-like object or numeric input. This can be a \code{Date} object, a date-like string, or a numeric value representing Julian days.
#'
#' @return A numeric vector representing the day(s) of the year. If the input contains invalid date formats, an error will be thrown.
#'
#' @details
#' If the input is numeric, the function treats it as Julian days. If the input is a \code{POSIXct} or \code{POSIXlt} object, it will be automatically coerced to a \code{Date} object.
#'
#' The function uses \code{\link[base]{format}} to extract the day of the year from valid date objects.
#'
#' @seealso \code{\link[base]{as.Date}}, \code{\link[base]{format}}
#'
#' @export
#'
#' @examples
#' doy("2024-05-03") # Using a date-like string
#' doy(as.Date("2024-05-03 13:14:00")) # Using a Date object with time
#' doy(c("2023-01-01", "2023-12-31")) # Vectorized input with multiple dates
#' doy(150) # Using a numeric value (Julian day)
#'
doy <- function(dat) {
  # Handle different input types: character, numeric, Date, POSIXct/POSIXlt

  # Convert character vector to Date
  if (is.character(dat)) {
    dat <- as.Date(dat)
  }

  # If input is already Date object, return DOY directly
  if (inherits(dat, "Date")) {
    return(as.numeric(format(dat, "%j")))
  }

  # Handle POSIXct/POSIXlt objects and retain the original warning message
  if (inherits(dat, c("POSIXct", "POSIXlt"))) {
    warning("It appears you have provided a time series object. Please ensure the time zone is correct.")
    dat <- as.Date(dat)
    return(as.numeric(format(dat, "%j")))
  }

  # For numeric input
  if (is.numeric(dat)) {
    dat <- as.Date(dat)
    return(as.numeric(format(dat, "%j")))
  }

  # If input type is invalid, throw an error
  stop("Invalid input type. Please provide a date-like string, Date, POSIXct/POSIXlt object, or numeric value.")
}
