# Import needed packages and functions to use for testing
# source("../import.R")
# source("../dataCleaningFunctions.R")

# Keyring entry comes in format expected
testthat::test_that("Data cleaning testing", {
  # Set some key ring values
  expect_true(TRUE)
})

produce_test_loading_table1_data <- function(){
  dplyr::tribble(
    ~group1, ~group2, ~measure1_char, ~measure2_char, ~measure3_num, ~measure_data,
    NA_character_, NA_character_, NA_character_, NA_character_, NA_integer_, lubridate::NA_Date_,
    'g1', 'group1', '1', '1', 1, as.Date('01/01/2021', '%d/%m/%Y'),
    'g2', NA_character_, '2', '2', 2, as.Date('02/01/2021', '%d/%m/%Y'),
    'g4', 'group2', '3b', '3b', 3, as.Date('03/01/2021', '%d/%m/%Y'),
    'g3', 'group3', '3', '3', 3, as.Date('04/01/2021', '%d/%m/%Y')
  )
}

produce_test_loading_table2_data <- function(){
  dplyr::tribble(
    ~group1, ~group2, ~measure1_char, ~measure2_char, ~measure3_num, ~measure_data,
    NA_character_, NA_character_, NA_character_, NA_character_, NA_integer_, lubridate::NA_Date_,
    'g1', 'group1', '1', '1', 1, as.Date('01/01/2021', '%d/%m/%Y'),
    'g2', NA_character_, '2', '2', 2, as.Date('02/01/2021', '%d/%m/%Y'),
    'g4', 'group2', '3', '3', 3, as.Date('03/01/2021', '%d/%m/%Y'),
    'g3', 'group3', '3', '3', 3, as.Date('04/01/2021', '%d/%m/%Y'),
    'g1', 'group1', '1', '1', 1, as.Date('01/01/2021', '%d/%m/%Y'),
    'g2', NA_character_, '2', '2', 2, as.Date('02/01/2021', '%d/%m/%Y'),
    'g4', 'group1', '3', '3', 3, as.Date('03/01/2021', '%d/%m/%Y'),
    'g3', 'group1', '3', '3', 3, as.Date('04/01/2021', '%d/%m/%Y'),
    'g1', 'group1', '1', '1', 1, as.Date('01/01/2021', '%d/%m/%Y'),
    'g2', NA_character_, '2', '2', 2, as.Date('02/01/2021', '%d/%m/%Y'),
    'g4', 'group1', '3', '3', 3, as.Date('03/01/2021', '%d/%m/%Y'),
    'g3', 'group1', '3', '3', 3, as.Date('04/01/2021', '%d/%m/%Y')
  )
}

produce_test_loading_table3_data_simpler <- function(){
  dplyr::tribble(
    ~group1, ~group2, ~measure3_num, ~measure_data,
    'g0', 'group0', 0, as.Date('01/01/2021', '%d/%m/%Y'),
    'g1', 'group1', 1, as.Date('01/01/2021', '%d/%m/%Y'),
    'g2', 'group2', 2, as.Date('02/01/2021', '%d/%m/%Y'),
    'g4', 'group2', 3, as.Date('03/01/2021', '%d/%m/%Y'),
    'g3', 'group3', 3, as.Date('04/01/2021', '%d/%m/%Y')
  )
}

# Test the function for different offsets
test_that("dates_year_with_offset returns the correct year for different offsets", {
  library('lubridate')
  expect_equal(dates_year_with_offset(as.Date("2023-04-07"), 3), 2023)
  expect_equal(dates_year_with_offset(as.Date("2024-01-01"), 7), 2023)
  expect_equal(dates_year_with_offset(as.Date("2022-06-15"), 15), 2021)
})

# Test the function for dates on January 1st
test_that("dates_year_with_offset returns the correct year for dates on January 1st", {
  library('lubridate')
  expect_equal(dates_year_with_offset(as.Date("2022-01-01"), 0), 2022)
  expect_equal(dates_year_with_offset(as.Date("2023-01-01"), 1), 2022)
  expect_equal(dates_year_with_offset(as.Date("2024-01-01"), 2), 2023)
  expect_equal(dates_year_with_offset(as.Date("2024-01-01"), 10), 2023)
})

# Test the function for invalid input
test_that("dates_year_with_offset returns an error for invalid input", {
  library('lubridate')
  expect_error(dates_year_with_offset("2022-01-01", 0))
  expect_error(dates_year_with_offset(as.Date("2022-01-01"), "0"))
})


# Define the test cases
test_that("days_difference_in_dates returns the correct number of days between two dates", {
  
  # Test case 1: Check if the function returns the correct number of days between two dates
  expect_equal(days_difference_in_dates(as.Date("2023-04-07"), as.Date("2023-03-31")), 7)
  
  # Test case 2: Check if the function returns the correct number of days between two dates
  expect_equal(days_difference_in_dates(as.Date("2023-04-07"), as.Date("2022-12-31")), 97)
  
  # Test case 3: Check if the function returns 0 if both dates are the same
  expect_equal(days_difference_in_dates(as.Date("2023-04-07"), as.Date("2023-04-07")), 0)
  
  # Test case 4: Check if the function returns the correct number of days if the start date is after the end date
  expect_equal(days_difference_in_dates(as.Date("2023-03-31"), as.Date("2023-04-07")), -7)
  
  # Test case 5: Check if the function returns an error if one of the input is not a date object
  expect_error(days_difference_in_dates("2023-04", as.Date("2022-12-31")), "character string is not in a standard unambiguous format")
  expect_error(days_difference_in_dates("a", as.Date("2022-12-31")), "character string is not in a standard unambiguous format")
})


# Testing the approximate_years_difference function
context("Testing the approximate_years_difference function")

# Test case 1: Difference between two dates within same year
test_that("Difference between two dates within same year is less than 1", {
  end_date <- as.Date("2023-04-09")
  start_date <- as.Date("2023-01-01")
  expected_result <- 98/365.25
  actual_result <- approxmate_years_difference(end_date, start_date)
  expect_equal(actual_result, expected_result)
})

# Test case 2: Difference between two dates across multiple years
test_that("Difference between two dates across multiple years is greater than 1", {
  end_date <- as.Date("2023-04-09")
  start_date <- as.Date("2010-01-01")
  expected_result <- 4846 / 365.25
  actual_result <- approxmate_years_difference(end_date, start_date)
  expect_equal(actual_result, expected_result)
})

# Test case 3: Difference between two dates is exactly 1 year
test_that("Difference between two dates is exactly 1 year", {
  end_date <- as.Date("2024-04-09")
  start_date <- as.Date("2023-04-09")
  expected_result <- 366 / 365.25
  actual_result <- approxmate_years_difference(end_date, start_date)
  expect_equal(actual_result, expected_result)
})

# Test case 4: Difference between two dates is less than 1 year but more than 1 day
test_that("Difference between two dates is less than 1 year but more than 1 day", {
  end_date <- as.Date("2023-04-02")
  start_date <- as.Date("2023-03-31")
  expected_result <- 2 / 365.25
  actual_result <- approxmate_years_difference(end_date, start_date)
  expect_equal(actual_result, expected_result)
})

# Test case 5: Check if the function returns an error if one of the input is not a date object
test_that("Difference between two dates is less than 1 year but more than 1 day", {
  end_date <- "2023-04"
  start_date <- as.Date("2023-03-31")
  expect_error(approxmate_years_difference(end_date, start_date), "character string is not in a standard unambiguous format")
})

# Test case with non-empty vector and default value
test_that("max_value_with_default returns maximum value from non-empty vector", {
  x <- c(1, 2, 3, 4, 5)
  expect_equal(max_value_with_default(x), max(x))
})

# Test case with empty vector and default value
test_that("max_value_with_default returns default value for empty vector", {
  x <- c()
  expect_warning(max_value_with_default(x, default_val = -1), "no non-missing arguments to max; returning -Inf")
  expect_equal(max_value_with_default(x, default_val = -1), -1)
})

# Test case with vector containing missing values and default value
test_that("max_value_with_default returns max value for vector containing only some missing values", {
  x <- c(1, 2, NA, 4, NA)
  expect_equal(max_value_with_default(x, default_val = -1), 4)
})

# Test case with vector containing all missing values and no default value
test_that("max_value_with_default returns 0 for vector containing entirely missing values", {
  x <- c(NA, NA, NA, NA, NA)
  expect_warning(max_value_with_default(x), "no non-missing arguments to max; returning -Inf")
  expect_true(is.na(max_value_with_default(x)))
})

# Test case with vector containing all missing values and providing default val
test_that("max_value_with_default returns default value for vector containing entirely missing values", {
  x <- c(NA, NA, NA, NA, NA)
  expect_warning(max_value_with_default(x, default_val = -1), "no non-missing arguments to max; returning -Inf")
  expect_equal(max_value_with_default(x, default_val = -1), -1)
})

# Test with a non-empty vector
test_that("max_value_default_zero returns the correct maximum value with non-empty vector", {
  vec <- c(1, 2, 3, 4, 5)
  expect_equal(max_value_default_zero(vec), max(vec))
})

# Test with an empty vector
test_that("max_value_default_zero returns 0 with empty vector", {
  vec <- c()
  expect_warning(max_value_default_zero(vec), "no non-missing arguments to max; returning -Inf")
  expect_equal(max_value_default_zero(vec), 0)
})

# Test with a vector containing only missing values
test_that("max_value_default_zero returns 0 with vector containing only missing values", {
  vec <- c(NA, NA, NA)
  expect_warning(max_value_default_zero(vec), "no non-missing arguments to max; returning -Inf")
  expect_equal(max_value_default_zero(vec), 0)
})

# Test with a vector containing both missing and non-missing values
test_that("max_value_default_zero returns the correct maximum value with vector containing missing and non-missing values", {
  vec <- c(1, 2, NA, 4, 5)
  expect_equal(max_value_default_zero(vec), max(vec, na.rm = TRUE))
})

# Test if values above the upper bound are correctly replaced with the upper bound
test_that("bound_continuous_var Values above upper bound are replaced with upper bound", {
  upper_bound <- 5
  lower_bound <- -5
  input_vec <- c(-10, -5, 0, 5, 10)
  expected_output <- c(-5, -5, 0, 5, 5)
  output <- bound_continuous_var(input_vec, upper_bound, lower_bound)
  expect_equal(output, expected_output)
})

# Test if values below the lower bound are correctly replaced with the lower bound
test_that("bound_continuous_var Values below lower bound are replaced with lower bound", {
  upper_bound <- 10
  lower_bound <- -10
  input_vec <- c(-10, -5, 0, 5, 10)
  expected_output <- c(-10, -5, 0, 5, 10)
  output <- bound_continuous_var(input_vec, upper_bound, lower_bound)
  expect_equal(output, expected_output)
})

# Test if values within the bounds are not changed
test_that("bound_continuous_var Values within bounds are not changed", {
  upper_bound <- 10
  lower_bound <- -10
  input_vec <- c(-5, 0, 5)
  expected_output <- c(-5, 0, 5)
  output <- bound_continuous_var(input_vec, upper_bound, lower_bound)
  expect_equal(output, expected_output)
})

# Test if empty input vector returns an empty vector
test_that("bound_continuous_var Empty input vector returns an empty vector", {
  upper_bound <- 10
  lower_bound <- -10
  input_vec <- c()
  expect_error(bound_continuous_var(input_vec, upper_bound, lower_bound))
})

# Test if missing values come through properly as their original missing value
test_that("bound_continuous_varEmpty input vector returns an empty vector", {
  upper_bound <- 10
  lower_bound <- -10
  input_vec <- c(NA, 4, NA)
  expected_output <- c(NA, 4, NA)
  output <- bound_continuous_var(input_vec, upper_bound, lower_bound)
  expect_equal(output, expected_output)
})

# Test that the function correctly replaces missing values with "missing"
test_that("missing_char_replacer replaces missing values with 'missing'", {
  df <- data.frame(x = c("a", NA, "c"), y = c("d", "e", NA), z = c(1, 2, 3))
  expected_output <- data.frame(x = c("a", "missing", "c"), y = c("d", "e", "missing"), z = c(1, 2, 3))
  output <- missing_char_replacer(df)
  expect_identical(output, expected_output)
})

# Test that the function does not modify non-character columns
test_that("missing_char_replacer does not modify non-character columns", {
  df2 <- data.frame(x = c(1, 2, NA), y = c("a", "b", "c"))
  output <- missing_char_replacer(df2)
  expect_identical(df2, output)
})

# Define test cases
test_that("compute_or_load_function correctly computes and saves data or loads from disk", {
  
  check_and_make_dir('./temp')
  
  # Create test data
  test_data <- data.frame(a = 1:10, b = 11:20)
  output_data <- data.frame(a = 1:10, b = 11:20, z=12:21) 
  
  # Define test function
  test_fun <- function(x) { x %>% dplyr::mutate(z=b + 1) }
  
  # Test case 1: function correctly computes and saves data
  filepath1 <- "./temp/test1.parquet"
  result1 <- compute_or_load_function(filepath1, test_fun, refresh_if_exists = TRUE, test_data)
  expect_equal(result1, output_data)
  expect_true(base::file.exists(filepath1))
  
  # Test case 2: function correctly loads data from disk
  filepath2 <- "./temp/test2.parquet"
  arrow::write_parquet(test_data, filepath2)
  result2 <- compute_or_load_function(filepath2, test_fun)
  expect_equal(result2, test_data)
  
  # Test case 3: function correctly loads data from disk if refresh_if_exists is set to FALSE
  filepath3 <- "./temp/test3.parquet"
  arrow::write_parquet(output_data, filepath3)
  result3 <- compute_or_load_function(filepath3, test_fun, refresh_if_exists = FALSE)
  expect_equal(result3, output_data)
  
  # Test case 4: function correctly computes and data even if the data exists
  filepath4 <- "./temp/test4.parquet"
  arrow::write_parquet(test_data, filepath4)  # Write the wrong data -> if loaded from disk, test will fail
  result4 <- compute_or_load_function(filepath4, test_fun, refresh_if_exists = TRUE, test_data)
  expect_equal(result4, output_data)
  expect_true(base::file.exists(filepath4))
  
  # Clean up test files
  file.remove(filepath1, filepath2, filepath3, filepath4)
  unlink("./temp", recursive=T)
})

