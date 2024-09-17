#' Calculate Day Length
#'
#' @description
#' \code{day_length} calculates the maximum possible day length (sunshine duration) based on latitude and date.
#'
#' @param lat Numeric. Latitude in degrees. Use positive values for north, negative for south.
#' @param date A Date object or a date-like string that can be coerced to Date.
#'
#' @return Numeric. Day length in hours.
#' @export
#'
#' @examples
#' day_length(lat = 35, date = as.Date("2023-03-15"))
#'
#' day_length(lat = -15, date = "2024-06-21")
#'
day_length <- function(lat, date) {
  if (!is.numeric(lat)) {stop("Latitude must be numeric.")}
  if (lat < -90 || lat > 90) {stop("Latitude must be between -90 and 90 degrees.")}

  lat_rad <- lat * pi / 180  # Convert latitude to radians

  # Calculate solar declination in radians
  delta <- sol_dec(date, in_radians = TRUE)

  # Calculate sunset hour angle
  omega_s <- acos(-tan(lat_rad) * tan(delta))

  # Calculate maximum possible day length in hours
  day_length <- (24 / pi) * omega_s

  day_length
}
