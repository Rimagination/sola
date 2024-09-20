#' @title Sunset Hour Angle Calculation
#'
#' @description
#' \code{sunset_ha} calculates the sunset hour angle for a given date and latitude.
#'
#' @param dates A Date object, a date-like string, or a numeric value that can be coerced to Date.
#' @param latitudes Numeric vector. Latitudes in degrees (positive for northern hemisphere, negative for southern).
#' @param in_degrees Logical. If \code{TRUE}, returns the angle in degrees. If \code{FALSE} (default), returns in radians.
#'
#' @return A numeric vector representing the sunset hour angle in radians (default) or degrees.
#'
#' @examples
#' sunset_ha("2023-03-15", latitudes = 35) # Returns in radians (default)
#' sunset_ha(c("2023-03-15","2023-04-20"), latitudes = c(35, 45), in_degrees = TRUE) # Returns in degrees
#'
#' @export
#'
sunset_ha <- function(dates, latitudes, in_degrees = FALSE) {
  # Ensure dates is a Date object or a date-like string vector
  if (!is.numeric(latitudes)) stop("Latitudes must be numeric.")
  if (any(latitudes < -90 | latitudes > 90)) stop("Latitudes must be between -90 and 90 degrees.")

  # Convert latitudes from degrees to radians
  latitudes_rad <- latitudes * (pi / 180)

  # Calculate the declination angle (delta) in radians
  delta <- declination(dates, in_degrees = FALSE)

  # Calculate the sunset hour angle (H) in radians
  x <- -tan(latitudes_rad) * tan(delta)
  x <- pmax(pmin(x, 1), -1) # Ensure x is within the domain of acos
  angle_rad <- acos(x)

  # Ensure the value is within the range [0, pi] radians
  angle_rad[angle_rad < 0] <- 0
  angle_rad[angle_rad > pi] <- pi

  # Return either in degrees or radians
  if (in_degrees) {
    return(angle_rad * (180 / pi)) # Convert to degrees
  } else {
    return(angle_rad) # Return in radians (default)
  }
}
