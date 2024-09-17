#' Calculate Extraterrestrial Solar Radiation (Ra)
#'
#' @description
#' \code{ext_rad} calculates extraterrestrial radiation based on latitude and date.
#'
#' @param lat Numeric. Latitude in degrees. Use positive values for north, negative for south.
#' @param date Date or date-like string. A Date object or a string that can be coerced to a Date.
#'
#' @return Numeric. Extraterrestrial radiation in MJ/m²/day.
#'
#' @export
#'
#' @examples
#' ext_rad(lat = 35.0, date = as.Date("2023-03-15"))
#' ext_rad(lat = -15.0, date = "2024-06-21")
#'
ext_rad <- function(lat, date) {
  if (!is.numeric(lat)) stop("Latitude must be numeric.")
  lat_rad <- lat * pi / 180  # Convert latitude to radians
  date <- tryCatch(as.Date(date), error = function(e) stop("Invalid date input."))

  # Calculate day of year
  J <- doy(date)

  # Calculate solar declination
  delta_angle <- sol_dec(date, in_radians = TRUE) # Return in radians

  # Calculate sunset hour angle
  omega_s <- 2 * acos(-tan(lat_rad) * tan(delta_angle))

  # Calculate inverse relative distance Earth-Sun
  dr <- 1 + 0.033 * cos(2 * pi * J / 365)

  # Calculate extraterrestrial radiation
  Gsc <- 0.0820  # Solar constant (MJ/m²/min)
  Ra <- (24 * 60 / pi) * Gsc * dr * (cos(lat_rad) * cos(delta_angle) * sin(omega_s) +
                                       sin(lat_rad) * sin(delta_angle) * cos(omega_s))  # MJ/m²/day

  return(Ra)
}
