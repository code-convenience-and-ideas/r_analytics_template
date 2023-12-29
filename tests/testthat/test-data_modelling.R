# Import needed packages and functions to use for testing
# source("../import.R")
# source("../dataModellingFunctions.R")

# Keyring entry comes in format expected
testthat::test_that("Data modelling testing", {
  # Set some key ring values
  expect_true(TRUE)
})

# Test the escape_spaced_var function
test_that("escape_spaced_var replaces spaces with backticks", {
  
  # Test case 1: no spaces
  input1 <- c("my_var", "another_var")
  expected_output1 <- c("my_var", "another_var")
  output1 <- escape_spaced_var(input1)
  expect_identical(output1, expected_output1, 
                   info = "should return the same vector if there are no spaces")
  
  # Test case 2: spaces present
  input2 <- c("my var", "another var")
  expected_output2 <- c("`my var`", "`another var`")
  output2 <- escape_spaced_var(input2)
  expect_identical(output2, expected_output2, 
                   info = "should replace spaces with backticks")
  
  # Test case 3: empty vector
  input3 <- character(0)
  expected_output3 <- character(0)
  output3 <- escape_spaced_var(input3)
  expect_identical(output3, expected_output3, 
                   info = "should return an empty vector if the input is empty")
  
})


# Test the col_var_renamers function
test_that("col_var_renamers replaces spaces with underscores", {
  
  # Test case 1: no spaces
  input1 <- c("column_1", "column_2", "column_3")
  expected_output1 <- c("column_1", "column_2", "column_3")
  output1 <- col_var_renamers(input1)
  expect_identical(output1, expected_output1, 
                   info = "should return the same vector if there are no spaces")
  
  # Test case 2: spaces present
  input2 <- c("column 1", "column 2", "column 3")
  expected_output2 <- c("column_1", "column_2", "column_3")
  output2 <- col_var_renamers(input2)
  expect_identical(output2, expected_output2, 
                   info = "should replace spaces with underscores")
  
  # Test case 3: empty vector
  input3 <- character(0)
  expected_output3 <- character(0)
  output3 <- col_var_renamers(input3)
  expect_identical(output3, expected_output3, 
                   info = "should return an empty vector if the input is empty")
  
})

# Define a test suite for the formula_builder function
test_that("formula_builder function works as expected", {
  
  # Test 1: The function returns a string
  output <- formula_builder("y", c("x1", "x2", "x3"))
  expect_is(output, "character")
  
  # Test 2: The function returns the correct formula for linear models
  output <- formula_builder("y", c("x1", "x2", "x3"))
  expect_equal(output, "y ~ x1 + x2 + x3")
  
  # Test 3: The function returns the correct formula for survival models without censoring variable
  output <- formula_builder("time", c("x1", "x2", "x3"), is_surv = TRUE)
  expect_equal(output, "Surv( time ) ~ x1 + x2 + x3")
  
  # Test 4: The function returns the correct formula for survival models with censoring variable
  output <- formula_builder("time", c("x1", "x2", "x3"), is_surv = TRUE, censor_var = "status")
  expect_equal(output, "Surv( time, status ) ~ x1 + x2 + x3")
  
  # Test 5: The function returns the correct formula for survival models with censoring variable and additional independent variable
  output <- formula_builder("y", c("x1", "x2", "x3"), is_surv = TRUE, is_add = TRUE, censor_var = "z")
  expect_equal(output, "y + z ~ x1 + x2 + x3")
})
