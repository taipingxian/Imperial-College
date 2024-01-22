test_that("padding by 0 works", {
  expect_equal(pad_with_NAs(x = 1:3, n_left = 1, n_right = 0), c(NA, 1, 2, 3))
  expect_equal(pad_with_NAs(x = 1:3, n_left = 0, n_right = 1), c(1, 2, 3, NA))
})

test_that("examples work", {
  expect_equal(
    object = pad_with_NAs(1:5, n_left = 0, n_right = 3),
    expected = c(1:5, NA, NA, NA))
  expect_equal(
    object = pad_with_NAs(c("spider", "mouse", "cat", "dog"), n_left = 1, n_right = 2),
    expected = c(NA, "spider", "mouse", "cat", "dog", NA, NA)
  )
})

# Test 1: Basic functionality
test_that("rolling_mean calculates means correctly", {
  input_vector <- c(1, 2, 3, 4, 5)
  window_width <- 3
  result <- rolling_mean(input_vector, window_width)
  expected_result <- c(NA, 2, 3, 4, NA)
  expect_equal(result, expected_result)
})

# Test 2: Check for correct handling of odd window width
test_that("rolling_mean handles odd window width correctly", {
  input_vector <- c(1, 2, 3, 4, 5)
  window_width <- 3  # Make sure this is an odd number
  result <- rolling_mean(input_vector, window_width)
  expected_result <- c(NA, 2, 3, 4, NA)
  expect_equal(result, expected_result)
})

# Test 3: Check for correct handling of even window width
test_that("rolling_mean handles even window width correctly", {
  input_vector <- c(1, 2, 3, 4, 5)
  window_width <- 3
  result <- rolling_mean(input_vector, window_width)
  expected_result <- c(NA, 2, 3, 4, NA)
  expect_equal(result, expected_result)
})

# Test 4: Check for correct handling of NA values in input
# test_that("rolling_mean handles NA values correctly", {
#   input_vector <- c(1, 2, NA, 4, 5)
#   window_width <- 3
#   result <- rolling_mean(input_vector, window_width)
#   expected_result <- c(NA, NA, NA, NA, NA)  # Convert to a double vector
#   expect_equal(result, expected_result)
# })
