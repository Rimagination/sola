#' @title Solar Declination Calculation
#'
#' @description
#' \code{sol_dec} calculates the solar declination angle (delta) for a given date. The solar declination is the angle between the rays of the sun and the plane of the Earth's equator.
#'
#' @param dat A Date object, a date-like string, or a numeric value that can be coerced to Date.
#' @param in_radians Logical. If \code{TRUE}, returns the declination in radians. If \code{FALSE} (default), returns in degrees.
#'
#' @return A numeric vector representing the solar declination angle in degrees (default) or radians.
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
#' sol_dec(as.Date("2024-03-15")) # Returns in degrees (default)
#' sol_dec(as.Date("2024-03-15"), TRUE) # Returns in radians
#' sol_dec(75) # Julian day in degrees
#' # Handling a large dataset of dates (e.g., 100,000 dates)
#' large_dates <- as.Date("2024-01-01") + 0:99999
#' result_large <- sol_dec(large_dates)
#'
sol_dec <- function(dat, in_radians = FALSE) {
  # Convert input to Date if necessary
  if (is.character(dat)) {
    dat <- as.Date(dat)
  } else if (is.numeric(dat)) {
    dat <- as.Date(dat) # Numeric values are treated as Julian dates.
  } else if (inherits(dat, c("POSIXct", "POSIXlt"))) {
    warning("Time series input detected. Ensure the time zone is correct.")
    dat <- as.Date(dat)
  } else if (!inherits(dat, "Date")) {
    stop("Invalid date input. Please provide a date-like string, Date, POSIXct/POSIXlt object, or numeric value.")
  }

  # Calculate the day of the year (J)
  J <- as.numeric(format(dat, "%j"))

  # Calculate solar declination using FAO56 formula
  rad_conversion <- pi / 180
  delta_angle <- 23.45 * sin(rad_conversion * (360 / 365) * (J - 81))

  # Return either in degrees or radians
  if (in_radians) {
    return(delta_angle * rad_conversion) # Convert to radians if in_radians is TRUE
  } else {
    return(delta_angle) # Return in degrees (default)
  }
}
