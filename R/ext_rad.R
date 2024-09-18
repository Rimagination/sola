#' @title Extraterrestrial Solar Radiation (Ra) Calculation
#'
#' @description
#' \code{ext_rad} calculates extraterrestrial radiation based on latitude and date.
#'
#' @param lat Numeric vector. Latitude in degrees, where positive values are for the northern hemisphere and negative for the southern hemisphere.
#' @param date Date or date-like string vector. A Date object or a string that can be coerced to a Date.
#'
#' @return Numeric vector. Extraterrestrial radiation in MJ/m²/day.
#'
#' @details
#' The function computes the extraterrestrial solar radiation on a horizontal surface using the latitude and day of the year.
#' It ensures valid calculations by keeping sunset hour angle values within the range of the \code{acos} function.
#'
#' @export
#'
#' @examples
#' ext_rad(lat = 35.0, date = as.Date("2023-03-15"))
#' ext_rad(lat = -15.0, date = "2024-06-21")
#'
#' # Example with a large dataset
#' set.seed(123)
#' test_lat <- runif(10000, -89, 89) # Generate random latitudes within recommended range
#' test_date <- as.Date("2023-01-01") + sample(0:364, 10000, replace = TRUE) # Generate random dates in 2023
#' result_ext_rad <- ext_rad(test_lat, test_date)
#' print(head(result_ext_rad))
#'
ext_rad <- function(lat, date) {
  if (!is.numeric(lat)) stop("Latitude must be numeric.")
  if (any(lat < -90 | lat > 90)) stop("Latitude must be between -90 and 90 degrees.")
  lat_rad <- lat * pi / 180 # Convert latitude to radians
  date <- tryCatch(as.Date(date), error = function(e) stop("Invalid date input."))

  # Calculate day of year
  J <- as.numeric(format(date, "%j"))

  # Calculate solar declination using FAO56 formula
  delta_angle <- 23.45 * sin(pi / 180 * (360 / 365) * (J - 81))
  delta_rad <- delta_angle * pi / 180 # Convert to radians

  # Calculate sunset hour angle using SunHA method
  x <- -tan(lat_rad) * tan(delta_rad)
  x <- pmin(pmax(x, -1), 1) # Clip values to ensure within [-1, 1] range
  omega_s <- acos(x)

  # Calculate inverse relative distance Earth-Sun
  dr <- 1 + 0.033 * cos(2 * pi * J / 365)

  # Calculate extraterrestrial radiation
  Gsc <- 0.0820 # Solar constant (MJ/m²/min)
  Ra <- (24 * 60 / pi) * Gsc * dr * (cos(lat_rad) * cos(delta_rad) * sin(omega_s) +
    sin(lat_rad) * sin(delta_rad) * omega_s) # MJ/m²/day

  return(Ra)
}
