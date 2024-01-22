#' Add NAs to a vector
#'
#' @param x Vector to which NAs will be added.
#' @param n_left Number of NAs to add before x.
#' @param n_right Number of NAs to add after x.
#'
#' @return A vector containing x with the requested number of NA values before and after.
#'
#' @export
#' @examples
#' pad_with_NAs(1:5, n_left = 0, n_right = 3)
#' pad_with_NAs(c("spider", "mouse", "cat", "dog"), n_left = 1, n_right = 2)
#'
pad_with_NAs <- function(x, n_left, n_right){
  # Input checks
  stopifnot(n_left >= 0)
  stopifnot(n_right >= 0)
  stopifnot(class(x) %in% c("character", "complex", "integer", "logical", "numeric", "factor"))

  # Function Body
  c(rep(NA, n_left), x, rep(NA, n_right))
}

rolling_mean <- function(x, window_width, ...){
  # -----Input Checks ----------------------------------------------------------
  # Check that x is a vector with numerical interpretation
  stopifnot(is.logical(x) | is.integer(x) | is.double(x) | is.complex(x))
  stopifnot(length(x) > 0)

  # Check window_width is an odd, positive integer
  stopifnot(length(window_width) == 1)
  stopifnot(window_width %% 1 == 0)
  stopifnot((window_width / 2) %% 1 != 0)
  stopifnot(window_width > 0)

  # ----- Function Body --------------------------------------------------------

  # number of values left and right to include in each mean
  half_width <- floor(window_width / 2)
  x_padded <- pad_with_NAs(x, n_left = half_width, n_right = half_width)
  evaluation_locations <- seq_along(x) + half_width

  output <- rep(NA, length(x))

  for (index in evaluation_locations) {
    # Extract relevant values from x_padded
    indices_in_window <- seq(index - half_width, index + half_width, by = 1)
    values_in_window <- x_padded[indices_in_window]

    # Calculate and store mean
    output[index - half_width] <- mean(values_in_window, ...)
  }

  return(output)
}

calculate_geometric_mean <- function(vector) {
  if (length(vector) == 0) {
    stop("Vector must not be empty.")
  }

  # Ensure all elements are positive
  if (any(vector <= 0)) {
    stop("All elements in the vector must be positive.")
  }

  # Calculate the geometric mean
  result <- exp(mean(log(vector)))

  return(result)
}
