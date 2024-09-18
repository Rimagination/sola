#' @title Estimate Solar Radiation using the Angstrom-Prescott Model
#'
#' @description
#' The \code{sol_rad} function estimates daily solar radiation on a horizontal surface using the Angstrom-Prescott model, based on sunshine duration, latitude, and date. The function can internally calculate extraterrestrial radiation (\code{ext_rad}) and maximum possible sunshine duration (\code{N}) if these are not provided as inputs.
#'
#' @param lat Numeric vector. Latitude in degrees. Positive values indicate locations north of the equator, and negative values indicate locations south of the equator.
#' @param date Date or character vector. The date for which the solar radiation is estimated. It accepts Date objects or date-like strings that can be coerced to Date.
#' @param ssd Numeric vector. Sunshine duration in hours, representing the actual hours of sunshine received on the given day.
#' @param N Optional numeric vector. The maximum possible sunshine duration (day length) in hours. If not provided, it is calculated based on latitude and date.
#' @param ext_rad Optional numeric vector. Extraterrestrial radiation in MJ/m²/day. If not provided, it is calculated based on latitude and date.
#' @param A Numeric. Angstrom-Prescott coefficient A, defaulting to 0.25. This coefficient is used in the calculation of solar radiation.
#' @param B Numeric. Angstrom-Prescott coefficient B, defaulting to 0.50. This coefficient is used in the calculation of solar radiation.
#'
#' @return A numeric vector of estimated solar radiation values in MJ/m²/day for the given latitude, date, and sunshine duration.
#'
#' @details
#' The function uses the Angstrom-Prescott equation to estimate solar radiation:
#' \deqn{R_s = (A + B \times (ssd / N)) \times ext\_rad}
#' where \code{R_s} is the estimated solar radiation, \code{ssd} is the actual sunshine duration, \code{N} is the maximum possible sunshine duration, and \code{ext_rad} is the extraterrestrial radiation.
#'
#' If either \code{N} or \code{ext_rad} is not provided, the function calculates them using the input latitude and date. Values of \code{N} that are zero or negative are replaced with a very small positive number (1e-6) to avoid division by zero errors.
#'
#' @export
#'
#' @examples
#' # Estimate solar radiation for a single location and date
#' sol_rad(lat = 35.0, date = as.Date("2023-03-15"), ssd = 8)
#'
#' # Estimate for another date and location
#' sol_rad(lat = -15.0, date = "2024-06-21", ssd = 10)
#'
#' # Estimate solar radiation for multiple locations and dates
#' sol_rad(lat = c(35, 20), date = c("2023-03-15", "2022-02-15"), ssd = c(8, 5))
#'
#' # Performance test with large datasets
#' set.seed(123)
#' test_lat <- runif(10000, -90, 90)
#' test_date <- as.Date("2023-01-01") + sample(1:365, 10000, replace = TRUE)
#' test_ssd <- runif(10000, 0, 24)
#' system.time({
#'   sol_rad(lat = test_lat, date = test_date, ssd = test_ssd)
#' })
#'
sol_rad <- function(lat, date, ssd, N = NULL, ext_rad = NULL, A = 0.25, B = 0.50) {
  # Ensure inputs are vectors of the same length
  len <- max(length(lat), length(date), length(ssd))
  lat <- rep(lat, length.out = len)
  date <- rep(date, length.out = len)
  ssd <- rep(ssd, length.out = len)

  # Validate inputs
  if (any(!is.numeric(lat))) stop("Latitude must be numeric.")
  if (any(lat < -90 | lat > 90)) stop("Latitude must be between -90 and 90 degrees.")
  if (any(!is.numeric(ssd) | ssd < 0 | ssd > 24)) stop("Sunshine duration must be a number between 0 and 24.")

  # Convert date if necessary
  date <- tryCatch(as.Date(date), error = function(e) stop("Invalid date input."))

  # Calculate extraterrestrial radiation if not provided
  if (is.null(ext_rad)) {
    ext_rad <- ext_rad(lat, date) # Assuming ext_rad function is vectorized
  }

  # Calculate day length (N) if not provided
  if (is.null(N)) {
    N <- day_length(lat, date) # Assuming day_length function is vectorized
  }

  # Check for NaNs and invalid values in N and ext_rad
  N[is.na(N) | N <= 0] <- 1e-6 # Set a very small number if N is zero or NaN
  ssd <- pmin(ssd, N) # Ensure ssd does not exceed N

  # Function to calculate radiation using Angstrom-Prescott equation
  calc_rad <- function(A, B, ssd, N, ext_rad) {
    result <- (A + B * (ssd / N)) * ext_rad
    # Vectorized NaN check and handling
    result[is.nan(result)] <- 0 # Replace NaN with zero or handle as needed
    return(result)
  }

  # Standard sequential calculation using vectorized operations
  rad_estimate <- calc_rad(A, B, ssd, N, ext_rad)

  # Return results
  return(rad_estimate)
}
