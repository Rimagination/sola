#' @title Calculate Day Length
#'
#' @description
#' \code{day_length} calculates the maximum possible day length (sunshine duration) based on latitude and date.
#'
#' @param lat Numeric vector. Latitude in degrees. Use positive values for north, negative for south.
#' @param date A Date object or a date-like string vector that can be coerced to Date.
#'
#' @return Numeric vector. Day length in hours.
#' @export
#'
#' @examples
#' day_length(lat = 35, date = as.Date("2023-03-15"))
#' day_length(lat = c(-15, 45), date = c("2024-06-21", "2023-12-21"))
#'
day_length <- function(lat, date) {
  # Ensure lat and date are vectors of the same length
  len <- max(length(lat), length(date))
  lat <- rep(lat, length.out = len)
  date <- rep(date, length.out = len)

  if (!is.numeric(lat)) stop("Latitude must be numeric.")
  if (any(lat < -90 | lat > 90)) stop("Latitude must be between -90 and 90 degrees.")

  lat_rad <- lat * pi / 180 # Convert latitude to radians
  date <- tryCatch(as.Date(date), error = function(e) stop("Invalid date input."))

  # Calculate solar declination in radians
  delta <- sol_dec(date, in_radians = TRUE)

  # Calculate sunset hour angle with vectorized operations
  x <- -tan(lat_rad) * tan(delta)
  x <- pmin(pmax(x, -1), 1) # Clip values to ensure within [-1, 1] range
  omega_s <- acos(x)

  # Calculate maximum possible day length in hours
  day_length <- (24 / pi) * omega_s

  return(day_length)
}
