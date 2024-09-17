#' @title Solar Declination Calculation
#'
#' @description
#' \code{sol_dec} calculates the solar declination angle (delta) for a given date.
#'
#' @param dat A Date object or a date-like string that can be coerced to Date.
#' @param in_radians Logical. If \code{TRUE}, returns the declination in radians. If \code{FALSE} (default), returns in degrees.
#'
#' @return A numeric value representing the solar declination angle in degrees (default) or radians.
#'
#' @references
#' Allen, R. G., Pereira, L. S., Raes, D., & Smith, M. (1998).
#' \emph{Crop Evapotranspiration: Guidelines for Computing Crop Water Requirements}.
#' FAO Irrigation and drainage paper 56. Food and Agriculture Organization of the United Nations, Rome.
#'
#' @seealso \code{\link{doy}}
#'
#' @export
#'
#' @examples
#' sol_dec(as.Date("2024-03-15"))          # Returns in degrees (default)
#' sol_dec(as.Date("2024-03-15"), TRUE)    # Returns in radians
#' sol_dec(75)                             # Julian day in degrees
#'
sol_dec <- function(dat, in_radians = FALSE) {
  # Handle NA input
  if (all(is.na(dat))) return(NA)

  # Check for numeric input and provide a warning
  if (is.numeric(dat)) {
    warning(if (length(dat) == 1) {
      "Numeric values are treated as Julian dates. Consider using date-like strings or Date objects for clarity."
    } else {
      "Numeric vectors are treated as Julian dates. Please provide a single date-like string or Date object."
    })
    dat <- as.Date(dat, origin = "1970-01-01")  # Assuming numeric values represent Julian dates from 1970-01-01
  }

  # Attempt to coerce to Date and handle errors
  dat <- tryCatch(as.Date(dat), error = function(e) stop("Invalid date input."))

  # Handle POSIXct and POSIXlt objects
  if (inherits(dat, c("POSIXct", "POSIXlt"))) {
    warning("Time series input detected. Ensure the time zone is correct.")
    dat <- as.Date(dat)
  }

  # Calculate the day of year (J)
  J <- as.numeric(format(dat, "%j"))

  # Calculate solar declination using FAO56 formula
  delta_angle <- 23.45 * sin(pi / 180 * (360 / 365) * (J - 81))

  # Return either in degrees or radians
  if (in_radians) {
    return(delta_angle * pi / 180)  # Convert to radians if in_radians is TRUE
  } else {
    return(delta_angle)  # Return in degrees (default)
  }
}
