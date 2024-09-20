#' @title Solar Declination Calculation
#'
#' @description
#' \code{declination} calculates the solar declination (delta) for a given date.
#' @param dat A Date object, a date-like string, or a numeric value that can be coerced to Date.
#' @param in_degrees Logical. If \code{TRUE}, returns the declination in degrees. If \code{FALSE} (default), returns in radians.
#'
#' @return A numeric vector representing the solar declination angle in radians (default) or degrees.
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
#' declination(as.Date("2024-05-03")) # Returns in radians (default)
#' declination("2024-05-03", TRUE) # Returns in degrees
#' declination(75) # Input Julian day
#' # Handling a large dataset of dates (e.g., 100,000 dates)
#' large_dates <- as.Date("2024-01-01") + 0:99999
#' result_large <- declination(large_dates)
#'
declination <- function(date, in_degrees = FALSE) {

  # Calculate the day of the year (J)
  J <- doy(date)

  # Calculate solar declination using FAO56 formula
  delta_angle <- 0.409 * sin(2 * pi / 365 * J - 1.39)

  # Return either in degrees or radians
  if (in_degrees) {
    return(delta_angle * (180 / pi)) # Convert to degrees
  } else {
    return(delta_angle) # Return in radians (default)
  }
}
