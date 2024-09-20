#' @title Estimate Solar Radiation using the Angstrom-Prescott Model
#'
#' @description
#' The \code{calc_Rs} function estimates daily solar radiation on a horizontal surface using the Angstrom-Prescott model, based on sunshine duration, latitude, and date.
#'
#' @param lat Numeric vector. Latitude in degrees.
#' @param date Date or character vector. The date for which the solar radiation is estimated.
#' @param ssd Numeric vector. Sunshine duration in hours.
#' @param N Optional numeric vector. The maximum possible sunshine duration (day length) in hours.
#' @param Ra Optional numeric vector. Extraterrestrial radiation in MJ/m²/day.
#' @param A Numeric. Angstrom-Prescott coefficient A, defaulting to 0.25.
#' @param B Numeric. Angstrom-Prescott coefficient B, defaulting to 0.50.
#'
#' @return A numeric vector of estimated solar radiation values in MJ/m²/day.
#'
#' @details
#' The function uses the Angstrom-Prescott equation to estimate solar radiation.
#'
#' @examples
#' # Estimate solar radiation for a single location and date
#' calc_Rs(lat = 35.0, date = as.Date("2023-03-15"), ssd = 8)
#'
#' # Estimate for another date and location
#' calc_Rs(lat = -15.0, date = "2024-06-21", ssd = 10)
#'
#' # Estimate solar radiation for multiple locations and dates
#' calc_Rs(lat = c(35, 20), date = c(as.Date("2023-03-15"), as.Date("2022-02-15")), ssd = c(8, 5))
#'
#' # Performance test with large datasets
#' set.seed(123)
#' test_lat <- runif(10000, -90, 90)
#' test_date <- as.Date("2023-01-01") + sample(1:365, 10000, replace = TRUE)
#' test_ssd <- runif(10000, 0, 24)
#' system.time({
#'   calc_Rs(lat = test_lat, date = test_date, ssd = test_ssd)
#' })
#'
#' @export
#'
calc_Rs <- function(lat, date, ssd, N = NULL, Ra = NULL, A = 0.25, B = 0.50) {
  # Ensure inputs are vectors of the same length
  len <- max(length(lat), length(date), length(ssd))
  lat <- rep(lat, length.out = len)
  date <- rep(date, length.out = len)
  ssd <- rep(ssd, length.out = len)

  # Validate inputs
  if (any(!is.numeric(lat))) stop("Latitude must be numeric.")
  if (any(!is.numeric(ssd))) stop("Sunshine duration must be numeric.")

  # Handle NA values for lat and ssd
  na_mask <- is.na(ssd) | is.na(lat)
  if (all(na_mask)) {
    return(rep(NA, len))  # Return NA for all entries if all ssd or lat are NA
  }

  # Set invalid SSD values (outside 0-24 range) to NA
  ssd[ssd < 0 | ssd > 24] <- NA

  # Set invalid latitude values (outside -90 to 90 range) to NA
  lat[lat < -90 | lat > 90] <- NA
  na_mask <- na_mask | is.na(lat)

  # Convert date if necessary
  date <- tryCatch(as.Date(date), error = function(e) stop("Invalid date input."))

  # Calculate extraterrestrial radiation if not provided
  if (is.null(Ra)) {
    Ra <- calc_Ra(lat, date)
  }

  # Calculate day length (N) if not provided
  if (is.null(N)) {
    N <- daylength(lat, date)
  }

  # Check for NaNs and invalid values in N and Ra
  N[is.na(N) | N <= 0] <- 1e-6  # Set a very small number if N is zero or NaN

  # Function to calculate radiation using Angstrom-Prescott equation
  calc_rad <- function(A, B, ssd, N, Ra) {
    result <- (A + B * (ssd / N)) * Ra
    result[na_mask] <- NA  # Ensure that if lat or ssd is NA, the result is also NA
    return(result)
  }

  # Standard sequential calculation using vectorized operations
  rad_estimate <- calc_rad(A, B, ssd, N, Ra)

  # Return results
  return(rad_estimate)
}
