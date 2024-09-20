#' @title Calculate Day Length
#'
#' @description
#' \code{daylength} calculates the maximum possible day length (sunshine duration) based on latitude and date.
#'
#' @param lat Numeric vector. Latitude in degrees. Use positive values for north, negative for south.
#' @param date A Date object or a date-like string vector that can be coerced to Date.
#'
#' @return Numeric vector. Day length in hours.
#' @export
#'
#' @examples
#' daylength(lat = 35, date = "2023-03-15")
#' daylength(lat = c(-15, 45), date = c("2024-06-21", "2023-12-21"))
#'
daylength <- function(lat, date) {
  # Ensure lat is numeric and valid
  if (!is.numeric(lat)) stop("Latitude must be numeric.")
  if (any(lat < -90 | lat > 90)) stop("Latitude must be between -90 and 90 degrees.")

  # Convert latitude from degrees to radians
  lat_rad <- lat * (pi / 180)

  # Calculate solar declination for all dates at once
  delta <- declination(date, in_degrees = FALSE)

  # Calculate sunset hour angle for all latitudes and dates at once
  omega_s <- sunset_ha(date, lat)

  # Define a small tolerance for floating point comparisons
  tolerance <- 1e-6

  # Calculate maximum possible day length in hours
  day_length <- mapply(function(lat_rad, delta, omega_s) {
    ifelse(lat_rad + delta > pi / 2 - tolerance,
           24,
           ifelse(lat_rad + delta < -pi / 2 + tolerance,
                  0,
                  (24 / pi) * omega_s))
  }, lat_rad, delta, omega_s)

  return(day_length)
}




