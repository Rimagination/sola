#' @title Calculate Day of the Year (DOY)
#'
#' @description
#' \code{doy} calculates the day of the year (DOY) for a given date. January 1st is represented as 1, and December 31st is 365 (or 366 in leap years).
#'
#' @param dat A date-like object or numeric input. This can be a \code{Date} object, a date-like string, or a numeric value representing Julian days. See \code{\link[base]{as.Date}} for supported formats.
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
#' doy("2024-05-03")                          # Using a date-like string
#' doy(as.Date("2024-05-03 13:14:00"))         # Using a Date object with time
#' doy(c("2023-01-01", "2023-12-31"))          # Vectorized input with multiple dates
#' doy(150)                                    # Using a numeric value (Julian day)
#'
doy <- function(dat) {
  # Handle NA input
  if (all(is.na(dat))) return(NA)

  # Check for numeric input and provide a warning
  if (is.numeric(dat)) {
    warning(if (length(dat) == 1) {
      "Numeric values are treated as Julian days. Consider using date-like strings or Date objects for clarity."
    } else {
      "Numeric vectors are treated as Julian days. Please provide a single date-like string or Date object."
    })
    dat <- as.Date(dat)
  }

  # Attempt to coerce to Date and handle errors
  dat <- tryCatch(as.Date(dat), error = function(e) stop("Invalid date input."))

  # Handle POSIXct and POSIXlt objects
  if (inherits(dat, c("POSIXct", "POSIXlt"))) {
    warning("It appears you have provided a time series object. Please ensure the time zone is correct.")
    dat <- as.Date(dat)
  }

  # Return the day of the year
  as.numeric(format(dat, "%j"))
}
