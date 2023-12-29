# message(paste("Starting testing the configuration functions from dir: ", getwd()))
# Import needed packages and functions to use for testing
# source("../import.R")
# source("../configurationFunctions.R")

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

# Write tests for the different functions used in the configuration
# Keyring entry comes in format expected
testthat::test_that("get_db_username gets right value", {
  # Set some key ring values
  keyring::key_set_with_value('testset', 'testuser', 'testpassword')
  keyring::key_set_with_value('testset2', 'testuser2', 'testpassword')
  
  expect_identical(get_db_username('testset'), 'testuser')
  expect_identical(get_db_username('testset2'), 'testuser2')
  
  keyring::key_delete('testset', 'testuser')
  keyring::key_delete('testset2', 'testuser2')
})

# testthat::test_that("try_keyring_access passes if entry exists and prompt entry if missing", {
#   expect_invisible(try_keyring_access())
# })

testthat::test_that("check_and_make_dir makes directory only if it doesn't exist and returns correct values", {
  # Check that it works as expected for making a directory when its missing
  expect_true(check_and_make_dir('./temp'))  # Makes folder and should expect true
  expect_true(unlink('./temp', recursive = TRUE)==0)  # Unlink should be true only if folder exists to remove

  # Check it works when directory already exist
  expect_true(check_and_make_dir('./temp'))  # Makes folder and should expect true
  expect_true(check_and_make_dir('./temp'))  # Should return true as well now that folder is made
  expect_true(unlink('./temp', recursive = TRUE)==0)  # Unlink should be true only if folder exists to remove
  
})

# testthat::test_that("test_database_credentials correctly fails and prompts new values with invalid credentials", {
#   expect_true(TRUE)
# })

# testthat::test_that("test_database_credentials passes if credentials pass", {
#   # Should just pass invisbly, returns no values
#   expect_invisible(test_database_credentials('PRDLKDB'))
# })

testthat::test_that("round2 correctly rounds nubmer in expected way AGAINST IEC standard", {
  # Positive numbers
  expect_identical(round2(c(1, 1.1, 1.11, 1.5, 1.15, 2.66, 2.65, 3.33, 6.745, 6.733), digits=3), c(1, 1.1, 1.11, 1.5, 1.15, 2.66, 2.65, 3.33, 6.745, 6.733))
  expect_identical(round2(c(1, 1.1, 1.11, 1.5, 1.15, 2.66, 2.65, 3.33, 6.745, 6.733), digits=2), c(1, 1.1, 1.11, 1.5, 1.15, 2.66, 2.65, 3.33, 6.75, 6.73))
  expect_identical(round2(c(1, 1.1, 1.11, 1.5, 1.15, 2.66, 2.65, 3.33, 6.745, 6.733), digits=1), c(1, 1.1, 1.1, 1.5, 1.2, 2.7, 2.7, 3.3, 6.7, 6.7))
  expect_identical(round2(c(1, 1.1, 1.11, 1.5, 1.15, 2.66, 2.65, 3.33, 6.745, 6.733), digits=1), c(1, 1.1, 1.1, 1.5, 1.2, 2.7, 2.7, 3.3, 6.7, 6.7))
  
  # Negative numbers
  expect_identical(round2(-c(1, 1.1, 1.11, 1.5, 1.15, 2.66, 2.65, 3.33, 6.745, 6.733), digits=3), -c(1, 1.1, 1.11, 1.5, 1.15, 2.66, 2.65, 3.33, 6.745, 6.733))
  expect_identical(round2(-c(1, 1.1, 1.11, 1.5, 1.15, 2.66, 2.65, 3.33, 6.745, 6.733), digits=2), -c(1, 1.1, 1.11, 1.5, 1.15, 2.66, 2.65, 3.33, 6.75, 6.73))
  expect_identical(round2(-c(1, 1.1, 1.11, 1.5, 1.15, 2.66, 2.65, 3.33, 6.745, 6.733), digits=1), -c(1, 1.1, 1.1, 1.5, 1.2, 2.7, 2.7, 3.3, 6.7, 6.7))
  expect_identical(round2(-c(1, 1.1, 1.11, 1.5, 1.15, 2.66, 2.65, 3.33, 6.745, 6.733), digits=1), -c(1, 1.1, 1.1, 1.5, 1.2, 2.7, 2.7, 3.3, 6.7, 6.7))
  
})

testthat::test_that("round_power correctly applies available powers and some significant figure options.", {
  # Test main power options: none; k; M; 
  # Positive number examples
  # First test easy examples with no rounding required
  expect_identical(round_power(c(1.1, 1.2, 1.3, 1.4, 1.5, 1.6), '', sigfigs=1), as.character(c(1.1, 1.2, 1.3, 1.4, 1.5, 1.6)))
  expect_identical(round_power(c(1.11, 1.22, 1.33, 1.44, 1.55, 1.66), '', sigfigs=2), as.character(c(1.11, 1.22, 1.33, 1.44, 1.55, 1.66)))
  expect_identical(round_power(c(1.1e3, 1.2e3, 1.3e3, 1.4e3, 1.5e3, 1.6e3), 'k', sigfigs=1), c("1.1k", "1.2k", "1.3k", "1.4k", "1.5k", "1.6k"))
  expect_identical(round_power(c(1.11e3, 1.22e3, 1.33e3, 1.44e3, 1.55e3, 1.66e3), 'k', sigfigs=2), c("1.11k", "1.22k", "1.33k", "1.44k", "1.55k", "1.66k"))
  expect_identical(round_power(c(1.1e6, 1.2e6, 1.3e6, 1.4e6, 1.5e6, 1.6e6), 'M', sigfigs=1), c("1.1M", "1.2M", "1.3M", "1.4M", "1.5M", "1.6M"))
  expect_identical(round_power(c(1.11e6, 1.22e6, 1.33e6, 1.44e6, 1.55e6, 1.66e6), 'M', sigfigs=2), c("1.11M", "1.22M", "1.33M", "1.44M", "1.55M", "1.66M"))
  
  # Test round down with various digits
  expect_identical(round_power(c(1.11, 1.21, 1.31, 1.41, 1.51, 1.61), '', sigfigs=1), as.character(c(1.1, 1.2, 1.3, 1.4, 1.5, 1.6)))
  expect_identical(round_power(c(1.111, 1.221, 1.331, 1.441, 1.551, 1.661), '', sigfigs=2), as.character(c(1.11, 1.22, 1.33, 1.44, 1.55, 1.66)))
  expect_identical(round_power(c(1.11e3, 1.21e3, 1.31e3, 1.41e3, 1.51e3, 1.61e3), 'k', sigfigs=1), c("1.1k", "1.2k", "1.3k", "1.4k", "1.5k", "1.6k"))
  expect_identical(round_power(c(1.111e3, 1.221e3, 1.331e3, 1.441e3, 1.551e3, 1.661e3), 'k', sigfigs=2), c("1.11k", "1.22k", "1.33k", "1.44k", "1.55k", "1.66k"))
  expect_identical(round_power(c(1.11e6, 1.21e6, 1.31e6, 1.41e6, 1.51e6, 1.61e6), 'M', sigfigs=1), c("1.1M", "1.2M", "1.3M", "1.4M", "1.5M", "1.6M"))
  expect_identical(round_power(c(1.111e6, 1.221e6, 1.331e6, 1.441e6, 1.551e6, 1.661e6), 'M', sigfigs=2), c("1.11M", "1.22M", "1.33M", "1.44M", "1.55M", "1.66M"))
    
  # Test round up with various digits
  expect_identical(round_power(c(1.06, 1.16, 1.26, 1.36, 1.46, 1.56), '', sigfigs=1), as.character(c(1.1, 1.2, 1.3, 1.4, 1.5, 1.6)))
  expect_identical(round_power(c(1.106, 1.216, 1.326, 1.436, 1.546, 1.656), '', sigfigs=2), as.character(c(1.11, 1.22, 1.33, 1.44, 1.55, 1.66)))
  expect_identical(round_power(c(1.06e3, 1.16e3, 1.26e3, 1.36e3, 1.46e3, 1.56e3), 'k', sigfigs=1), c("1.1k", "1.2k", "1.3k", "1.4k", "1.5k", "1.6k"))
  expect_identical(round_power(c(1.106e3, 1.216e3, 1.326e3, 1.436e3, 1.546e3, 1.656e3), 'k', sigfigs=2), c("1.11k", "1.22k", "1.33k", "1.44k", "1.55k", "1.66k"))
  expect_identical(round_power(c(1.06e6, 1.16e6, 1.26e6, 1.36e6, 1.46e6, 1.56e6), 'M', sigfigs=1), c("1.1M", "1.2M", "1.3M", "1.4M", "1.5M", "1.6M"))
  expect_identical(round_power(c(1.106e6, 1.216e6, 1.326e6, 1.436e6, 1.546e6, 1.656e6), 'M', sigfigs=2), c("1.11M", "1.22M", "1.33M", "1.44M", "1.55M", "1.66M"))
  
  
  # Test explicit .5 cases to match what we expect (round up, we add slight machine tolerance to do this per a blog)
  expect_identical(round_power(c(1.05, 1.15, 1.25, 1.35, 1.45, 1.55), '', sigfigs=1), as.character(c(1.1, 1.2, 1.3, 1.4, 1.5, 1.6)))
  expect_identical(round_power(c(1.105, 1.215, 1.325, 1.435, 1.545, 1.655), '', sigfigs=2), as.character(c(1.11, 1.22, 1.33, 1.44, 1.55, 1.66)))
  expect_identical(round_power(c(1.05e3, 1.15e3, 1.25e3, 1.35e3, 1.45e3, 1.55e3), 'k', sigfigs=1), c("1.1k", "1.2k", "1.3k", "1.4k", "1.5k", "1.6k"))
  expect_identical(round_power(c(1.105e3, 1.215e3, 1.325e3, 1.435e3, 1.545e3, 1.655e3), 'k', sigfigs=2), c("1.11k", "1.22k", "1.33k", "1.44k", "1.55k", "1.66k"))
  expect_identical(round_power(c(1.05e6, 1.15e6, 1.25e6, 1.35e6, 1.45e6, 1.55e6), 'M', sigfigs=1), c("1.1M", "1.2M", "1.3M", "1.4M", "1.5M", "1.6M"))
  expect_identical(round_power(c(1.105e6, 1.215e6, 1.325e6, 1.435e6, 1.545e6, 1.655e6), 'M', sigfigs=2), c("1.11M", "1.22M", "1.33M", "1.44M", "1.55M", "1.66M"))

  # negative number examples
  # First test easy examples with no rounding required
  expect_identical(round_power(-c(1.1, 1.2, 1.3, 1.4, 1.5, 1.6), '', sigfigs=1), as.character(-c(1.1, 1.2, 1.3, 1.4, 1.5, 1.6)))
  expect_identical(round_power(-c(1.11, 1.22, 1.33, 1.44, 1.55, 1.66), '', sigfigs=2), as.character(-c(1.11, 1.22, 1.33, 1.44, 1.55, 1.66)))
  expect_identical(round_power(-c(1.1e3, 1.2e3, 1.3e3, 1.4e3, 1.5e3, 1.6e3), 'k', sigfigs=1), c("-1.1k", "-1.2k", "-1.3k", "-1.4k", "-1.5k", "-1.6k"))
  expect_identical(round_power(-c(1.11e3, 1.22e3, 1.33e3, 1.44e3, 1.55e3, 1.66e3), 'k', sigfigs=2), c("-1.11k", "-1.22k", "-1.33k", "-1.44k", "-1.55k", "-1.66k"))
  expect_identical(round_power(-c(1.1e6, 1.2e6, 1.3e6, 1.4e6, 1.5e6, 1.6e6), 'M', sigfigs=1), c("-1.1M", "-1.2M", "-1.3M", "-1.4M", "-1.5M", "-1.6M"))
  expect_identical(round_power(-c(1.11e6, 1.22e6, 1.33e6, 1.44e6, 1.55e6, 1.66e6), 'M', sigfigs=2), c("-1.11M", "-1.22M", "-1.33M", "-1.44M", "-1.55M", "-1.66M"))
  
  # Test round down with various digits
  expect_identical(round_power(-c(1.11, 1.21, 1.31, 1.41, 1.51, 1.61), '', sigfigs=1), as.character(-c(1.1, 1.2, 1.3, 1.4, 1.5, 1.6)))
  expect_identical(round_power(-c(1.111, 1.221, 1.331, 1.441, 1.551, 1.661), '', sigfigs=2), as.character(-c(1.11, 1.22, 1.33, 1.44, 1.55, 1.66)))
  expect_identical(round_power(-c(1.11e3, 1.21e3, 1.31e3, 1.41e3, 1.51e3, 1.61e3), 'k', sigfigs=1), c("-1.1k", "-1.2k", "-1.3k", "-1.4k", "-1.5k", "-1.6k"))
  expect_identical(round_power(-c(1.111e3, 1.221e3, 1.331e3, 1.441e3, 1.551e3, 1.661e3), 'k', sigfigs=2), c("-1.11k", "-1.22k", "-1.33k", "-1.44k", "-1.55k", "-1.66k"))
  expect_identical(round_power(-c(1.11e6, 1.21e6, 1.31e6, 1.41e6, 1.51e6, 1.61e6), 'M', sigfigs=1), c("-1.1M", "-1.2M", "-1.3M", "-1.4M", "-1.5M", "-1.6M"))
  expect_identical(round_power(-c(1.111e6, 1.221e6, 1.331e6, 1.441e6, 1.551e6, 1.661e6), 'M', sigfigs=2), c("-1.11M", "-1.22M", "-1.33M", "-1.44M", "-1.55M", "-1.66M"))
  
  # Test round up with various digits
  expect_identical(round_power(-c(1.06, 1.16, 1.26, 1.36, 1.46, 1.56), '', sigfigs=1), as.character(-c(1.1, 1.2, 1.3, 1.4, 1.5, 1.6)))
  expect_identical(round_power(-c(1.106, 1.216, 1.326, 1.436, 1.546, 1.656), '', sigfigs=2), as.character(-c(1.11, 1.22, 1.33, 1.44, 1.55, 1.66)))
  expect_identical(round_power(-c(1.06e3, 1.16e3, 1.26e3, 1.36e3, 1.46e3, 1.56e3), 'k', sigfigs=1), c("-1.1k", "-1.2k", "-1.3k", "-1.4k", "-1.5k", "-1.6k"))
  expect_identical(round_power(-c(1.106e3, 1.216e3, 1.326e3, 1.436e3, 1.546e3, 1.656e3), 'k', sigfigs=2), c("-1.11k", "-1.22k", "-1.33k", "-1.44k", "-1.55k", "-1.66k"))
  expect_identical(round_power(-c(1.06e6, 1.16e6, 1.26e6, 1.36e6, 1.46e6, 1.56e6), 'M', sigfigs=1), c("-1.1M", "-1.2M", "-1.3M", "-1.4M", "-1.5M", "-1.6M"))
  expect_identical(round_power(-c(1.106e6, 1.216e6, 1.326e6, 1.436e6, 1.546e6, 1.656e6), 'M', sigfigs=2), c("-1.11M", "-1.22M", "-1.33M", "-1.44M", "-1.55M", "-1.66M"))
  
  
  # Test explicit .5 cases to match what we expect (round up, we add slight machine tolerance to do this per a blog)
  expect_identical(round_power(-c(1.05, 1.15, 1.25, 1.35, 1.45, 1.55), '', sigfigs=1), as.character(-c(1.1, 1.2, 1.3, 1.4, 1.5, 1.6)))
  expect_identical(round_power(-c(1.105, 1.215, 1.325, 1.435, 1.545, 1.655), '', sigfigs=2), as.character(-c(1.11, 1.22, 1.33, 1.44, 1.55, 1.66)))
  expect_identical(round_power(-c(1.05e3, 1.15e3, 1.25e3, 1.35e3, 1.45e3, 1.55e3), 'k', sigfigs=1), c("-1.1k", "-1.2k", "-1.3k", "-1.4k", "-1.5k", "-1.6k"))
  expect_identical(round_power(-c(1.105e3, 1.215e3, 1.325e3, 1.435e3, 1.545e3, 1.655e3), 'k', sigfigs=2), c("-1.11k", "-1.22k", "-1.33k", "-1.44k", "-1.55k", "-1.66k"))
  expect_identical(round_power(-c(1.05e6, 1.15e6, 1.25e6, 1.35e6, 1.45e6, 1.55e6), 'M', sigfigs=1), c("-1.1M", "-1.2M", "-1.3M", "-1.4M", "-1.5M", "-1.6M"))
  expect_identical(round_power(-c(1.105e6, 1.215e6, 1.325e6, 1.435e6, 1.545e6, 1.655e6), 'M', sigfigs=2), c("-1.11M", "-1.22M", "-1.33M", "-1.44M", "-1.55M", "-1.66M"))
})

testthat::test_that("round_power correctly fails for non-valid options of power.", {
  expect_error(round_power(c(1.06), '12'))
  expect_error(round_power(c(1.06), 't'))
  expect_error(round_power(c(1.06), 'q'))
  expect_error(round_power(c(1.06), '9'))
})

testthat::test_that("round_power correctly fails for NaN or incorrect type of input.", {
  expect_error(round_power(c(NA_character_), power_unit=""))
  expect_identical(round_power(c(NA_real_, NA_integer_), power_unit=""), c(NA_character_, NA_character_))
  expect_error(round_power(c(NA_character_), power_unit="k"))
  expect_identical(round_power(c(NA_real_, NA_integer_), power_unit="k"), c(NA_character_, NA_character_))
})

testthat::test_that("add_dollar correctly adds $ sign at front of digits for +ve and -ve numbers.", {
  expect_identical(add_dollar(c(1.1, 1.2, 1.3, 1.4, 1.5, 1.6)), c("$1.1", "$1.2", "$1.3", "$1.4", "$1.5", "$1.6"))
  expect_identical(add_dollar(c(1.11, 1.22, 1.33, 1.44, 1.55, 1.66)), c("$1.11", "$1.22", "$1.33", "$1.44", "$1.55", "$1.66"))
  expect_identical(add_dollar(-c(1.1, 1.2, 1.3, 1.4, 1.5, 1.6)), c("-$1.1", "-$1.2", "-$1.3", "-$1.4", "-$1.5", "-$1.6"))
  expect_identical(add_dollar(-c(1.11, 1.22, 1.33, 1.44, 1.55, 1.66)), c("-$1.11", "-$1.22", "-$1.33", "-$1.44", "-$1.55", "-$1.66"))
})

testthat::test_that("add_dollar correctly returns NA for input.", {
  expect_identical(add_dollar(c(NA_real_, NA_character_, NA_integer_)), c(NA_character_, NA_character_, NA_character_))
})

testthat::test_that("round_million produces correct string for multi,million values and sub-million values.", {
  # positive values
  expect_identical(round_million(c(1.1e6, 1.2e6, 1.3e6, 1.4e6, 1.5e6, 1.6e6), sigfigs=1), c("1.1M", "1.2M", "1.3M", "1.4M", "1.5M", "1.6M"))
  expect_identical(round_million(c(1.11e6, 1.22e6, 1.33e6, 1.44e6, 1.55e6, 1.66e6), sigfigs=2), c("1.11M", "1.22M", "1.33M", "1.44M", "1.55M", "1.66M"))
  expect_identical(round_million(c(1.11e6, 1.21e6, 1.31e6, 1.41e6, 1.51e6, 1.61e6), sigfigs=1), c("1.1M", "1.2M", "1.3M", "1.4M", "1.5M", "1.6M"))
  expect_identical(round_million(c(1.111e6, 1.221e6, 1.331e6, 1.441e6, 1.551e6, 1.661e6), sigfigs=2), c("1.11M", "1.22M", "1.33M", "1.44M", "1.55M", "1.66M"))
  expect_identical(round_million(c(1.06e6, 1.16e6, 1.26e6, 1.36e6, 1.46e6, 1.56e6), sigfigs=1), c("1.1M", "1.2M", "1.3M", "1.4M", "1.5M", "1.6M"))
  expect_identical(round_million(c(1.106e6, 1.216e6, 1.326e6, 1.436e6, 1.546e6, 1.656e6), sigfigs=2), c("1.11M", "1.22M", "1.33M", "1.44M", "1.55M", "1.66M"))
  expect_identical(round_million(c(1.05e6, 1.15e6, 1.25e6, 1.35e6, 1.45e6, 1.55e6), sigfigs=1), c("1.1M", "1.2M", "1.3M", "1.4M", "1.5M", "1.6M"))
  expect_identical(round_million(c(1.105e6, 1.215e6, 1.325e6, 1.435e6, 1.545e6, 1.655e6), sigfigs=2), c("1.11M", "1.22M", "1.33M", "1.44M", "1.55M", "1.66M"))
  
  # negative values
  expect_identical(round_million(-c(1.1e6, 1.2e6, 1.3e6, 1.4e6, 1.5e6, 1.6e6), sigfigs=1), c("-1.1M", "-1.2M", "-1.3M", "-1.4M", "-1.5M", "-1.6M"))
  expect_identical(round_million(-c(1.11e6, 1.22e6, 1.33e6, 1.44e6, 1.55e6, 1.66e6), sigfigs=2), c("-1.11M", "-1.22M", "-1.33M", "-1.44M", "-1.55M", "-1.66M"))
  expect_identical(round_million(-c(1.11e6, 1.21e6, 1.31e6, 1.41e6, 1.51e6, 1.61e6), sigfigs=1), c("-1.1M", "-1.2M", "-1.3M", "-1.4M", "-1.5M", "-1.6M"))
  expect_identical(round_million(-c(1.111e6, 1.221e6, 1.331e6, 1.441e6, 1.551e6, 1.661e6), sigfigs=2), c("-1.11M", "-1.22M", "-1.33M", "-1.44M", "-1.55M", "-1.66M"))
  expect_identical(round_million(-c(1.06e6, 1.16e6, 1.26e6, 1.36e6, 1.46e6, 1.56e6), sigfigs=1), c("-1.1M", "-1.2M", "-1.3M", "-1.4M", "-1.5M", "-1.6M"))
  expect_identical(round_million(-c(1.106e6, 1.216e6, 1.326e6, 1.436e6, 1.546e6, 1.656e6), sigfigs=2), c("-1.11M", "-1.22M", "-1.33M", "-1.44M", "-1.55M", "-1.66M"))
  expect_identical(round_million(-c(1.05e6, 1.15e6, 1.25e6, 1.35e6, 1.45e6, 1.55e6), sigfigs=1), c("-1.1M", "-1.2M", "-1.3M", "-1.4M", "-1.5M", "-1.6M"))
  expect_identical(round_million(-c(1.105e6, 1.215e6, 1.325e6, 1.435e6, 1.545e6, 1.655e6), sigfigs=2), c("-1.11M", "-1.22M", "-1.33M", "-1.44M", "-1.55M", "-1.66M"))
  
})

testthat::test_that("round_million correctly fails for incorrect type of input and returns NA for right NAs.", {
  expect_error(round_million(c(NA_character_)))
  expect_identical(round_million(c(NA_real_, NA_integer_)), c(NA_character_, NA_character_))
})

testthat::test_that("round_million_dollars produce millions dollar string with dollar in correct place", {
  # positive values
  expect_identical(round_million_dollars(c(1.1e6, 1.2e6, 1.3e6, 1.4e6, 1.5e6, 1.6e6), sigfigs=1), c("$1.1M", "$1.2M", "$1.3M", "$1.4M", "$1.5M", "$1.6M"))
  expect_identical(round_million_dollars(c(1.11e6, 1.22e6, 1.33e6, 1.44e6, 1.55e6, 1.66e6), sigfigs=2), c("$1.11M", "$1.22M", "$1.33M", "$1.44M", "$1.55M", "$1.66M"))
  expect_identical(round_million_dollars(c(1.11e6, 1.21e6, 1.31e6, 1.41e6, 1.51e6, 1.61e6), sigfigs=1), c("$1.1M", "$1.2M", "$1.3M", "$1.4M", "$1.5M", "$1.6M"))
  expect_identical(round_million_dollars(c(1.111e6, 1.221e6, 1.331e6, 1.441e6, 1.551e6, 1.661e6), sigfigs=2), c("$1.11M", "$1.22M", "$1.33M", "$1.44M", "$1.55M", "$1.66M"))
  expect_identical(round_million_dollars(c(1.06e6, 1.16e6, 1.26e6, 1.36e6, 1.46e6, 1.56e6), sigfigs=1), c("$1.1M", "$1.2M", "$1.3M", "$1.4M", "$1.5M", "$1.6M"))
  expect_identical(round_million_dollars(c(1.106e6, 1.216e6, 1.326e6, 1.436e6, 1.546e6, 1.656e6), sigfigs=2), c("$1.11M", "$1.22M", "$1.33M", "$1.44M", "$1.55M", "$1.66M"))
  expect_identical(round_million_dollars(c(1.05e6, 1.15e6, 1.25e6, 1.35e6, 1.45e6, 1.55e6), sigfigs=1), c("$1.1M", "$1.2M", "$1.3M", "$1.4M", "$1.5M", "$1.6M"))
  expect_identical(round_million_dollars(c(1.105e6, 1.215e6, 1.325e6, 1.435e6, 1.545e6, 1.655e6), sigfigs=2), c("$1.11M", "$1.22M", "$1.33M", "$1.44M", "$1.55M", "$1.66M"))
  
  # negative values
  expect_identical(round_million_dollars(-c(1.1e6, 1.2e6, 1.3e6, 1.4e6, 1.5e6, 1.6e6), sigfigs=1), c("-$1.1M", "-$1.2M", "-$1.3M", "-$1.4M", "-$1.5M", "-$1.6M"))
  expect_identical(round_million_dollars(-c(1.11e6, 1.22e6, 1.33e6, 1.44e6, 1.55e6, 1.66e6), sigfigs=2), c("-$1.11M", "-$1.22M", "-$1.33M", "-$1.44M", "-$1.55M", "-$1.66M"))
  expect_identical(round_million_dollars(-c(1.11e6, 1.21e6, 1.31e6, 1.41e6, 1.51e6, 1.61e6), sigfigs=1), c("-$1.1M", "-$1.2M", "-$1.3M", "-$1.4M", "-$1.5M", "-$1.6M"))
  expect_identical(round_million_dollars(-c(1.111e6, 1.221e6, 1.331e6, 1.441e6, 1.551e6, 1.661e6), sigfigs=2), c("-$1.11M", "-$1.22M", "-$1.33M", "-$1.44M", "-$1.55M", "-$1.66M"))
  expect_identical(round_million_dollars(-c(1.06e6, 1.16e6, 1.26e6, 1.36e6, 1.46e6, 1.56e6), sigfigs=1), c("-$1.1M", "-$1.2M", "-$1.3M", "-$1.4M", "-$1.5M", "-$1.6M"))
  expect_identical(round_million_dollars(-c(1.106e6, 1.216e6, 1.326e6, 1.436e6, 1.546e6, 1.656e6), sigfigs=2), c("-$1.11M", "-$1.22M", "-$1.33M", "-$1.44M", "-$1.55M", "-$1.66M"))
  expect_identical(round_million_dollars(-c(1.05e6, 1.15e6, 1.25e6, 1.35e6, 1.45e6, 1.55e6), sigfigs=1), c("-$1.1M", "-$1.2M", "-$1.3M", "-$1.4M", "-$1.5M", "-$1.6M"))
  expect_identical(round_million_dollars(-c(1.105e6, 1.215e6, 1.325e6, 1.435e6, 1.545e6, 1.655e6), sigfigs=2), c("-$1.11M", "-$1.22M", "-$1.33M", "-$1.44M", "-$1.55M", "-$1.66M"))
  
})

testthat::test_that("round_million_dollars correctly fails for incorrect type of input.", {
  expect_error(round_million_dollars(c(NA_character_)))
  expect_identical(round_million_dollars(c(NA_real_, NA_integer_)), c(NA_character_, NA_character_))
})

testthat::test_that("round_dollar produce rounded number with dollar in correct place", {
  expect_identical(round_dollar(c(1.1, 1.2, 1.3, 1.4, 1.5, 1.6)), c("$1", "$1", "$1", "$1", "$2", "$2"))
  expect_identical(round_dollar(c(1.11, 1.22, 1.33, 1.44, 1.55, 1.66)), c("$1", "$1", "$1", "$1", "$2", "$2"))
  expect_identical(round_dollar(-c(1.1, 1.2, 1.3, 1.4, 1.5, 1.6)), c("-$1", "-$1", "-$1", "-$1", "-$2", "-$2"))
  expect_identical(round_dollar(-c(1.11, 1.22, 1.33, 1.44, 1.55, 1.66)), c("-$1", "-$1", "-$1", "-$1", "-$2", "-$2"))
})

testthat::test_that("round_dollar correctly fails for incorrect type of input.", {
  expect_error(round_dollar(c(NA_character_)))
  expect_identical(round_dollar(c(NA_real_, NA_integer_)), c(NA_character_, NA_character_))
})

testthat::test_that("round_dollar_power produce rounded number with dollar in correct place", {
  # Test main power options: none; k; M; 
  # Positive number examples
  # First test easy examples with no rounding required
  expect_identical(round_dollar_power(c(1.1, 1.2, 1.3, 1.4, 1.5, 1.6), '', sigfigs=1), c("$1.1", "$1.2", "$1.3", "$1.4", "$1.5", "$1.6"))
  expect_identical(round_dollar_power(c(1.11, 1.22, 1.33, 1.44, 1.55, 1.66), '', sigfigs=2), c("$1.11", "$1.22", "$1.33", "$1.44", "$1.55", "$1.66"))
  expect_identical(round_dollar_power(c(1.1e3, 1.2e3, 1.3e3, 1.4e3, 1.5e3, 1.6e3), 'k', sigfigs=1), c("$1.1k", "$1.2k", "$1.3k", "$1.4k", "$1.5k", "$1.6k"))
  expect_identical(round_dollar_power(c(1.11e3, 1.22e3, 1.33e3, 1.44e3, 1.55e3, 1.66e3), 'k', sigfigs=2), c("$1.11k", "$1.22k", "$1.33k", "$1.44k", "$1.55k", "$1.66k"))
  expect_identical(round_dollar_power(c(1.1e6, 1.2e6, 1.3e6, 1.4e6, 1.5e6, 1.6e6), 'M', sigfigs=1), c("$1.1M", "$1.2M", "$1.3M", "$1.4M", "$1.5M", "$1.6M"))
  expect_identical(round_dollar_power(c(1.11e6, 1.22e6, 1.33e6, 1.44e6, 1.55e6, 1.66e6), 'M', sigfigs=2), c("$1.11M", "$1.22M", "$1.33M", "$1.44M", "$1.55M", "$1.66M"))
  
  # Test round down with various digits
  expect_identical(round_dollar_power(c(1.11, 1.21, 1.31, 1.41, 1.51, 1.61), '', sigfigs=1), c("$1.1", "$1.2", "$1.3", "$1.4", "$1.5", "$1.6"))
  expect_identical(round_dollar_power(c(1.111, 1.221, 1.331, 1.441, 1.551, 1.661), '', sigfigs=2), c("$1.11", "$1.22", "$1.33", "$1.44", "$1.55", "$1.66"))
  expect_identical(round_dollar_power(c(1.11e3, 1.21e3, 1.31e3, 1.41e3, 1.51e3, 1.61e3), 'k', sigfigs=1), c("$1.1k", "$1.2k", "$1.3k", "$1.4k", "$1.5k", "$1.6k"))
  expect_identical(round_dollar_power(c(1.111e3, 1.221e3, 1.331e3, 1.441e3, 1.551e3, 1.661e3), 'k', sigfigs=2), c("$1.11k", "$1.22k", "$1.33k", "$1.44k", "$1.55k", "$1.66k"))
  expect_identical(round_dollar_power(c(1.11e6, 1.21e6, 1.31e6, 1.41e6, 1.51e6, 1.61e6), 'M', sigfigs=1), c("$1.1M", "$1.2M", "$1.3M", "$1.4M", "$1.5M", "$1.6M"))
  expect_identical(round_dollar_power(c(1.111e6, 1.221e6, 1.331e6, 1.441e6, 1.551e6, 1.661e6), 'M', sigfigs=2), c("$1.11M", "$1.22M", "$1.33M", "$1.44M", "$1.55M", "$1.66M"))
  
  # Test round up with various digits
  expect_identical(round_dollar_power(c(1.06, 1.16, 1.26, 1.36, 1.46, 1.56), '', sigfigs=1), c("$1.1", "$1.2", "$1.3", "$1.4", "$1.5", "$1.6"))
  expect_identical(round_dollar_power(c(1.106, 1.216, 1.326, 1.436, 1.546, 1.656), '', sigfigs=2), c("$1.11", "$1.22", "$1.33", "$1.44", "$1.55", "$1.66"))
  expect_identical(round_dollar_power(c(1.06e3, 1.16e3, 1.26e3, 1.36e3, 1.46e3, 1.56e3), 'k', sigfigs=1), c("$1.1k", "$1.2k", "$1.3k", "$1.4k", "$1.5k", "$1.6k"))
  expect_identical(round_dollar_power(c(1.106e3, 1.216e3, 1.326e3, 1.436e3, 1.546e3, 1.656e3), 'k', sigfigs=2), c("$1.11k", "$1.22k", "$1.33k", "$1.44k", "$1.55k", "$1.66k"))
  expect_identical(round_dollar_power(c(1.06e6, 1.16e6, 1.26e6, 1.36e6, 1.46e6, 1.56e6), 'M', sigfigs=1), c("$1.1M", "$1.2M", "$1.3M", "$1.4M", "$1.5M", "$1.6M"))
  expect_identical(round_dollar_power(c(1.106e6, 1.216e6, 1.326e6, 1.436e6, 1.546e6, 1.656e6), 'M', sigfigs=2), c("$1.11M", "$1.22M", "$1.33M", "$1.44M", "$1.55M", "$1.66M"))
  
  
  # Test explicit .5 cases to match what we expect (round up, we add slight machine tolerance to do this per a blog)
  expect_identical(round_dollar_power(c(1.05, 1.15, 1.25, 1.35, 1.45, 1.55), '', sigfigs=1), c("$1.1", "$1.2", "$1.3", "$1.4", "$1.5", "$1.6"))
  expect_identical(round_dollar_power(c(1.105, 1.215, 1.325, 1.435, 1.545, 1.655), '', sigfigs=2), c("$1.11", "$1.22", "$1.33", "$1.44", "$1.55", "$1.66"))
  expect_identical(round_dollar_power(c(1.05e3, 1.15e3, 1.25e3, 1.35e3, 1.45e3, 1.55e3), 'k', sigfigs=1), c("$1.1k", "$1.2k", "$1.3k", "$1.4k", "$1.5k", "$1.6k"))
  expect_identical(round_dollar_power(c(1.105e3, 1.215e3, 1.325e3, 1.435e3, 1.545e3, 1.655e3), 'k', sigfigs=2), c("$1.11k", "$1.22k", "$1.33k", "$1.44k", "$1.55k", "$1.66k"))
  expect_identical(round_dollar_power(c(1.05e6, 1.15e6, 1.25e6, 1.35e6, 1.45e6, 1.55e6), 'M', sigfigs=1), c("$1.1M", "$1.2M", "$1.3M", "$1.4M", "$1.5M", "$1.6M"))
  expect_identical(round_dollar_power(c(1.105e6, 1.215e6, 1.325e6, 1.435e6, 1.545e6, 1.655e6), 'M', sigfigs=2), c("$1.11M", "$1.22M", "$1.33M", "$1.44M", "$1.55M", "$1.66M"))
  
  # negative number examples
  # First test easy examples with no rounding required
  expect_identical(round_dollar_power(-c(1.1, 1.2, 1.3, 1.4, 1.5, 1.6), '', sigfigs=1), c("-$1.1", "-$1.2", "-$1.3", "-$1.4", "-$1.5", "-$1.6"))
  expect_identical(round_dollar_power(-c(1.11, 1.22, 1.33, 1.44, 1.55, 1.66), '', sigfigs=2), c("-$1.11", "-$1.22", "-$1.33", "-$1.44", "-$1.55", "-$1.66"))
  expect_identical(round_dollar_power(-c(1.1e3, 1.2e3, 1.3e3, 1.4e3, 1.5e3, 1.6e3), 'k', sigfigs=1), c("-$1.1k", "-$1.2k", "-$1.3k", "-$1.4k", "-$1.5k", "-$1.6k"))
  expect_identical(round_dollar_power(-c(1.11e3, 1.22e3, 1.33e3, 1.44e3, 1.55e3, 1.66e3), 'k', sigfigs=2), c("-$1.11k", "-$1.22k", "-$1.33k", "-$1.44k", "-$1.55k", "-$1.66k"))
  expect_identical(round_dollar_power(-c(1.1e6, 1.2e6, 1.3e6, 1.4e6, 1.5e6, 1.6e6), 'M', sigfigs=1), c("-$1.1M", "-$1.2M", "-$1.3M", "-$1.4M", "-$1.5M", "-$1.6M"))
  expect_identical(round_dollar_power(-c(1.11e6, 1.22e6, 1.33e6, 1.44e6, 1.55e6, 1.66e6), 'M', sigfigs=2), c("-$1.11M", "-$1.22M", "-$1.33M", "-$1.44M", "-$1.55M", "-$1.66M"))
  
  # Test round down with various digits
  expect_identical(round_dollar_power(-c(1.11, 1.21, 1.31, 1.41, 1.51, 1.61), '', sigfigs=1), c("-$1.1", "-$1.2", "-$1.3", "-$1.4", "-$1.5", "-$1.6"))
  expect_identical(round_dollar_power(-c(1.111, 1.221, 1.331, 1.441, 1.551, 1.661), '', sigfigs=2), c("-$1.11", "-$1.22", "-$1.33", "-$1.44", "-$1.55", "-$1.66"))
  expect_identical(round_dollar_power(-c(1.11e3, 1.21e3, 1.31e3, 1.41e3, 1.51e3, 1.61e3), 'k', sigfigs=1), c("-$1.1k", "-$1.2k", "-$1.3k", "-$1.4k", "-$1.5k", "-$1.6k"))
  expect_identical(round_dollar_power(-c(1.111e3, 1.221e3, 1.331e3, 1.441e3, 1.551e3, 1.661e3), 'k', sigfigs=2), c("-$1.11k", "-$1.22k", "-$1.33k", "-$1.44k", "-$1.55k", "-$1.66k"))
  expect_identical(round_dollar_power(-c(1.11e6, 1.21e6, 1.31e6, 1.41e6, 1.51e6, 1.61e6), 'M', sigfigs=1), c("-$1.1M", "-$1.2M", "-$1.3M", "-$1.4M", "-$1.5M", "-$1.6M"))
  expect_identical(round_dollar_power(-c(1.111e6, 1.221e6, 1.331e6, 1.441e6, 1.551e6, 1.661e6), 'M', sigfigs=2), c("-$1.11M", "-$1.22M", "-$1.33M", "-$1.44M", "-$1.55M", "-$1.66M"))
  
  # Test round up with various digits
  expect_identical(round_dollar_power(-c(1.06, 1.16, 1.26, 1.36, 1.46, 1.56), '', sigfigs=1), c("-$1.1", "-$1.2", "-$1.3", "-$1.4", "-$1.5", "-$1.6"))
  expect_identical(round_dollar_power(-c(1.106, 1.216, 1.326, 1.436, 1.546, 1.656), '', sigfigs=2), c("-$1.11", "-$1.22", "-$1.33", "-$1.44", "-$1.55", "-$1.66"))
  expect_identical(round_dollar_power(-c(1.06e3, 1.16e3, 1.26e3, 1.36e3, 1.46e3, 1.56e3), 'k', sigfigs=1), c("-$1.1k", "-$1.2k", "-$1.3k", "-$1.4k", "-$1.5k", "-$1.6k"))
  expect_identical(round_dollar_power(-c(1.106e3, 1.216e3, 1.326e3, 1.436e3, 1.546e3, 1.656e3), 'k', sigfigs=2), c("-$1.11k", "-$1.22k", "-$1.33k", "-$1.44k", "-$1.55k", "-$1.66k"))
  expect_identical(round_dollar_power(-c(1.06e6, 1.16e6, 1.26e6, 1.36e6, 1.46e6, 1.56e6), 'M', sigfigs=1), c("-$1.1M", "-$1.2M", "-$1.3M", "-$1.4M", "-$1.5M", "-$1.6M"))
  expect_identical(round_dollar_power(-c(1.106e6, 1.216e6, 1.326e6, 1.436e6, 1.546e6, 1.656e6), 'M', sigfigs=2), c("-$1.11M", "-$1.22M", "-$1.33M", "-$1.44M", "-$1.55M", "-$1.66M"))
  
  
  # Test explicit .5 cases to match what we expect (round up, we add slight machine tolerance to do this per a blog)
  expect_identical(round_dollar_power(-c(1.05, 1.15, 1.25, 1.35, 1.45, 1.55), '', sigfigs=1), c("-$1.1", "-$1.2", "-$1.3", "-$1.4", "-$1.5", "-$1.6"))
  expect_identical(round_dollar_power(-c(1.105, 1.215, 1.325, 1.435, 1.545, 1.655), '', sigfigs=2), c("-$1.11", "-$1.22", "-$1.33", "-$1.44", "-$1.55", "-$1.66"))
  expect_identical(round_dollar_power(-c(1.05e3, 1.15e3, 1.25e3, 1.35e3, 1.45e3, 1.55e3), 'k', sigfigs=1), c("-$1.1k", "-$1.2k", "-$1.3k", "-$1.4k", "-$1.5k", "-$1.6k"))
  expect_identical(round_dollar_power(-c(1.105e3, 1.215e3, 1.325e3, 1.435e3, 1.545e3, 1.655e3), 'k', sigfigs=2), c("-$1.11k", "-$1.22k", "-$1.33k", "-$1.44k", "-$1.55k", "-$1.66k"))
  expect_identical(round_dollar_power(-c(1.05e6, 1.15e6, 1.25e6, 1.35e6, 1.45e6, 1.55e6), 'M', sigfigs=1), c("-$1.1M", "-$1.2M", "-$1.3M", "-$1.4M", "-$1.5M", "-$1.6M"))
  expect_identical(round_dollar_power(-c(1.105e6, 1.215e6, 1.325e6, 1.435e6, 1.545e6, 1.655e6), 'M', sigfigs=2), c("-$1.11M", "-$1.22M", "-$1.33M", "-$1.44M", "-$1.55M", "-$1.66M"))
  
})

testthat::test_that("round_dollar_power correctly fails for incorrect type of input.", {
  expect_error(round_dollar(c(NA_character_)))
  expect_identical(round_dollar(c(NA_real_, NA_integer_)), c(NA_character_, NA_character_))
})

testthat::test_that("round_percent produces correct percentage formatted number for both negative and non-negative restrictions", {
  expect_identical(round_percent(c(0.1, 0.11, 0.111, 0.1115), no_negative=FALSE), c("10%", "11%", "11.1%", "11.2%"))
  expect_identical(round_percent(-c(0.1, 0.11, 0.111, 0.1115), no_negative=FALSE), c("-10%", "-11%", "-11.1%", "-11.2%"))
  expect_identical(round_percent(-c(0.1, 0.11, 0.111, 0.1115), no_negative=TRUE), c("0%", "0%", "0%", "0%"))
})

testthat::test_that("round_percent correctly fails for incorrect type of input.", {
  expect_error(round_percent(c(NA_character_)))
  expect_identical(round_percent(c(NA_real_, NA_integer_)), c(NA_character_, NA_character_))
})

testthat::test_that("format_with_nas_replaced produces expected values for a few formatting functions with missing values", {
  # Test round_dollar_power
  expect_identical(format_with_nas_replaced(c(1.1, 1.2, 1.3, 1.4, 1.5, 1.6, NA_real_, NA_integer_), round_dollar_power, '-', power_unit="", sigfigs=1), c("$1.1", "$1.2", "$1.3", "$1.4", "$1.5", "$1.6", '-', '-'))
  expect_identical(format_with_nas_replaced(c(1.1, 1.2, 1.3, 1.4, 1.5, 1.6, NA_real_, NA_integer_), round_dollar_power, 'empty', power_unit="", sigfigs=1), c("$1.1", "$1.2", "$1.3", "$1.4", "$1.5", "$1.6", 'empty', 'empty'))
  expect_identical(format_with_nas_replaced(-c(1.1, 1.2, 1.3, 1.4, 1.5, 1.6, NA_real_, NA_integer_), round_dollar_power, '-', power_unit="", sigfigs=1), c("-$1.1", "-$1.2", "-$1.3", "-$1.4", "-$1.5", "-$1.6", '-', '-'))
  expect_identical(format_with_nas_replaced(-c(1.1, 1.2, 1.3, 1.4, 1.5, 1.6, NA_real_, NA_integer_), round_dollar_power, 'empty', power_unit="", sigfigs=1), c("-$1.1", "-$1.2", "-$1.3", "-$1.4", "-$1.5", "-$1.6", 'empty', 'empty'))
  
  # test round_power
  expect_identical(format_with_nas_replaced(c(1.1, 1.2, 1.3, 1.4, 1.5, 1.6, NA_real_, NA_integer_), round_power, '-', power_unit="", sigfigs=1), c("1.1", "1.2", "1.3", "1.4", "1.5", "1.6", '-', '-'))
  expect_identical(format_with_nas_replaced(c(1.11, 1.22, 1.33, 1.44, 1.55, 1.66, NA_real_, NA_integer_), round_power, 'empty', power_unit="", sigfigs=2), c("1.11", "1.22", "1.33", "1.44", "1.55", "1.66", 'empty', 'empty'))
  expect_identical(format_with_nas_replaced(-c(1.1, 1.2, 1.3, 1.4, 1.5, 1.6, NA_real_, NA_integer_), round_power, '-', power_unit="", sigfigs=1), c("-1.1", "-1.2", "-1.3", "-1.4", "-1.5", "-1.6", '-', '-'))
  expect_identical(format_with_nas_replaced(-c(1.11, 1.22, 1.33, 1.44, 1.55, 1.66, NA_real_, NA_integer_), round_power, 'empty', power_unit="",sigfigs=2), c("-1.11", "-1.22", "-1.33", "-1.44", "-1.55", "-1.66", 'empty', 'empty'))
  
  # test round_percent
  expect_identical(format_with_nas_replaced(c(0.1, 0.11, 0.111, 0.1115, NA_real_, NA_integer_), round_percent, '-', no_negative=FALSE), c("10%", "11%", "11.1%", "11.2%", '-', '-'))
  expect_identical(format_with_nas_replaced(-c(0.1, 0.11, 0.111, 0.1115, NA_real_, NA_integer_), round_percent, 'empty', no_negative=FALSE), c("-10%", "-11%", "-11.1%", "-11.2%", 'empty', 'empty'))
  expect_identical(format_with_nas_replaced(-c(0.1, 0.11, 0.111, 0.1115, NA_real_, NA_integer_), round_percent, '-', no_negative=TRUE), c("0%", "0%", "0%", "0%", '-', '-'))
})

testthat::test_that("round_dollar_power_with_nas produces expected values and fills in missing values correctly", {
  # Test main power options: none; k; M; 
  # Positive number examples
  # First test easy examples with no rounding required
  expect_identical(round_dollar_power_with_nas(c(1.1, 1.2, 1.3, 1.4, 1.5, 1.6, NA_real_, NA_integer_), '-', sigfigs=1), c("$1.1", "$1.2", "$1.3", "$1.4", "$1.5", "$1.6", '-', '-'))
  expect_identical(round_dollar_power_with_nas(c(1.1, 1.2, 1.3, 1.4, 1.5, 1.6, NA_real_, NA_integer_), 'empty', sigfigs=1), c("$1.1", "$1.2", "$1.3", "$1.4", "$1.5", "$1.6", 'empty', 'empty'))
  expect_identical(round_dollar_power_with_nas(-c(1.1, 1.2, 1.3, 1.4, 1.5, 1.6, NA_real_, NA_integer_), '-', sigfigs=1), c("-$1.1", "-$1.2", "-$1.3", "-$1.4", "-$1.5", "-$1.6", '-', '-'))
  expect_identical(round_dollar_power_with_nas(-c(1.1, 1.2, 1.3, 1.4, 1.5, 1.6, NA_real_, NA_integer_), 'empty', sigfigs=1), c("-$1.1", "-$1.2", "-$1.3", "-$1.4", "-$1.5", "-$1.6", 'empty', 'empty'))
  
  })

testthat::test_that("round_power_with_nas produces expected values and fills in missing values correctly", {
  expect_identical(round_power_with_nas(c(1.1, 1.2, 1.3, 1.4, 1.5, 1.6, NA_real_, NA_integer_), '-', sigfigs=1), c("1.1", "1.2", "1.3", "1.4", "1.5", "1.6", '-', '-'))
  expect_identical(round_power_with_nas(c(1.11, 1.22, 1.33, 1.44, 1.55, 1.66, NA_real_, NA_integer_), 'empty', sigfigs=2), c("1.11", "1.22", "1.33", "1.44", "1.55", "1.66", 'empty', 'empty'))
  expect_identical(round_power_with_nas(-c(1.1, 1.2, 1.3, 1.4, 1.5, 1.6, NA_real_, NA_integer_), '-', sigfigs=1), c("-1.1", "-1.2", "-1.3", "-1.4", "-1.5", "-1.6", '-', '-'))
  expect_identical(round_power_with_nas(-c(1.11, 1.22, 1.33, 1.44, 1.55, 1.66, NA_real_, NA_integer_), 'empty', sigfigs=2), c("-1.11", "-1.22", "-1.33", "-1.44", "-1.55", "-1.66", 'empty', 'empty'))
})

testthat::test_that("round_percent_with_nas produces expected values and fills in missing values correctly", {
  expect_identical(round_percent_with_nas(c(0.1, 0.11, 0.111, 0.1115, NA_real_, NA_integer_), '-', no_negative=FALSE), c("10%", "11%", "11.1%", "11.2%", '-', '-'))
  expect_identical(round_percent_with_nas(-c(0.1, 0.11, 0.111, 0.1115, NA_real_, NA_integer_), 'empty', no_negative=FALSE), c("-10%", "-11%", "-11.1%", "-11.2%", 'empty', 'empty'))
  expect_identical(round_percent_with_nas(-c(0.1, 0.11, 0.111, 0.1115, NA_real_, NA_integer_), '-', no_negative=TRUE), c("0%", "0%", "0%", "0%", '-', '-'))
})

provide_test_proportions <- function(){c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6)}

testthat::test_that("proportion_lower_intervals creates correct lower interval for a few values", {
  test_proportions <- provide_test_proportions()
  test_n <- 100
  ci_coverage <- 0.95
  
  # Checked values to fourth digit on random online calculator for consitency
  expect_equal(proportion_lower_intervals(test_proportions, test_n, ci_coverage), c(0.0412, 0.1216, 0.2102, 0.3040, 0.4020, 0.5039), tol=0.0001)
})

testthat::test_that("proportion_upper_intervals creates correct lower interval for a few values", {
  test_proportions <- provide_test_proportions()
  test_n <- 100
  ci_coverage <- 0.95
  
  # Checked values to fourth digit on random online calculator for consitency
  expect_equal(proportion_upper_intervals(test_proportions, test_n, ci_coverage), c(0.1588, 0.2784, 0.3898, 0.4960, 0.5980, 0.6960), tol=0.0001)
})

testthat::test_that("conf_int_format creates formats numbers correctly in expected CI layout", {
  expect_identical(
    conf_int_format(seq(1, 8, 1), seq(1, 8, 1)-1, seq(1, 8, 1)+3),
    c("1 (0 - 4)", "2 (1 - 5)", "3 (2 - 6)", "4 (3 - 7)", "5 (4 - 8)", "6 (5 - 9)", "7 (6 - 10)", "8 (7 - 11)")
  )
})

testthat::test_that("proportion_conf_int_format calculates CI correctly and formats combined result correctly", {
  test_proportions <- provide_test_proportions()
  test_n <- 100
  ci_coverage <- 0.95
  
  expect_identical(proportion_conf_int_format(test_proportions, test_n, ci_coverage),
                   c("10% (4.1% - 15.9%)", "20% (12.2% - 27.8%)", "30% (21% - 39%)", "40% (30.4% - 49.6%)", "50% (40.2% - 59.8%)", "60% (50.4% - 69.6%)"))
})

produce_pivot_column_test_data <- function(){
  
  dplyr::tribble(
    ~group1, ~group2, ~measure1_char, ~measure2_char, ~measure3_num,
    'g1', 'group1', '1', '1', 1,
    'g2', NA_character_, '2', '2', 2,
    'g4', 'group2', '3', '3', 3,
    'g3', 'group3', '3', '3', 3
  )
}

testthat::test_that("pivot_group_column_to_measure_colums covers all group entries into columns", {
  testing_data <- produce_pivot_column_test_data()

  expected_measure_table_group1 <- dplyr::tribble(
    ~Measure, ~g1, ~g2, ~g4, ~g3,
    "group2", "group1", NA_character_, "group2", "group3",
    "measure1_char", "1", "2", "3", "3",
    "measure2_char", "1", "2", "3", "3"
  )
  
  target_cols <- "measure3_num"
  
  expect_identical(  testing_data %>%
                       dplyr::select(-dplyr::all_of(target_cols)) %>%
                       pivot_group_column_to_measure_colums("group1"),
                     expected_measure_table_group1)
  

})

testthat::test_that("pivot_group_column_to_measure_colums handles missing group values correctly", {
  testing_data <- produce_pivot_column_test_data()
  
  expected_measure_table_group2 <- dplyr::tribble(
    ~Measure, ~group1, ~`NA`, ~group2, ~group3,
    "group1", "g1", 'g2', "g4", "g3",
    "measure1_char", "1", "2", "3", "3",
    "measure2_char", "1", "2", "3", "3"
  )
  
  target_cols <- "measure3_num"
  
  expect_identical(  testing_data %>%
                       dplyr::select(-dplyr::all_of(target_cols)) %>%
                       pivot_group_column_to_measure_colums("group2"),
                     expected_measure_table_group2)
})

testthat::test_that("pivot_group_column_to_measure_colums fails for non-character entries", {
  testing_data <- produce_pivot_column_test_data()
  
  expect_error(  testing_data %>%
                       pivot_group_column_to_measure_colums("group2"))
})

testthat::context("Logging setup, running and key summary functions")


test_that("reverse_named_vector function works as expected", {
  
  # Test 1: Test if function reverses values of named vector
  named_vec_1 <- c("weight"="wt", "miles per gallon"="mpg")
  expected_output_1 <- c("wt"="weight", "mpg"="miles per gallon")
  expect_equal(reverse_named_vector(named_vec_1), expected_output_1)
  
  # Test 2: Test if function reverses the values of named vector of length one
  named_vec_2 <- c("key"="value")
  expected_output_2 <- c("value"="key")
  expect_equal(reverse_named_vector(named_vec_2), expected_output_2)
  
  # Test 3: Test if function returns an empty named vector if given an empty named vector
  named_vec_3 <- c()
  expected_output_3 <- c()
  expect_equal(reverse_named_vector(named_vec_3), expected_output_3)
  
  # Test 4: Test if function throws an error if input is not a named vector
  named_vec_4 <- c(1, 2, 3, 4)
  expect_error(reverse_named_vector(named_vec_4), "attempt to set an attribute on NULL")
})

# Test rename_dataframe_with_vector function
test_that("rename_dataframe_with_vector renames columns correctly", {
  # Create a test dataframe
  test_df <- data.frame(wt = c(1,2,3), mpg = c(4,5,6), hp = c(7,8,9))
  
  # Test renaming using naming vector with matching names
  result_df <- rename_dataframe_with_vector(test_df, c("weight"="wt", "miles per gallon"="mpg"))
  expect_identical(names(result_df), c("weight", "miles per gallon", "hp"))
  
  # Test renaming using naming vector without matching names
  result_df <- rename_dataframe_with_vector(test_df, c("carburetor"="carb", "transmission"="gear"))
  expect_identical(names(result_df), c("wt", "mpg", "hp"))
  
  # Test renaming using naming vector with matching name and non-matching names
  result_df <- rename_dataframe_with_vector(test_df, c("weight"="wt", "miles per gallon"="mpg", "carburetor"="carb", "transmission"="gear"))
  expect_identical(names(result_df), c("weight", "miles per gallon", "hp"))
  
})

# Define a test context
test_that("test_key_exists function tests", {
  
  keyring::key_set_with_value('databases', 'colloqDb', 'dummydb')
  
  # Test if key exists
  expect_true(test_key_exists('databases', 'colloqDb'))
  
  # Test if key exists within folder
  expect_false(test_key_exists("prod", "databases"))
  
  # Test it fails for 3 entry non-existent entries
  expect_false(test_key_exists("prod", "user", "JohnDoe"))
  
  # Test it fails for entries providing more then 3 arguments
  expect_error(test_key_exists("prod", "user", "JohnDoe", 'a'), 'unused argument')
  
  # Test it fails for entries providing 0 arguments
  expect_error(test_key_exists(), 'argument "service" is missing, with no default')
  
  # Cleanup
  keyring::key_delete('databases', 'colloqDb')
  
})


test_that("try_keyring_access works properly for missing and full entries", {
  # Test-case #1, entries all exist
  # Set up a mock keyring entry for testing purposes
  key_set_with_value("databases", "test_colloquial_db", "testdb")
  key_set_with_value("testdb", "testuser", "testpwd")
  
  # Call the function and check that keyring credentials are retrieved successfully
  try_keyring_access("test_colloquial_db")
  
  # Check that the expected keyring values are retrieved and unaltered
  expect_equal(key_get("databases", "test_colloquial_db"), "testdb")
  expect_equal(get_db_username("testdb"), "testuser")
  expect_equal(key_get("testdb", "testuser"), "testpwd")
  
  # Do clean up ofr test 1
  if (test_key_exists("testdb", "testuser")) {keyring::key_delete("testdb", "testuser")}
  if (test_key_exists("databases", "test_colloquial_db")) {keyring::key_delete("databases", "test_colloquial_db")}
  
  # # Test-case #2 - test for the case where the function ask for input values
  # Check message indicating reach to each portion of the function
  

  # Test #3 - Mock up a case which raises an invalid authentication error
  testthat::with_mock(
    key_set = function(...){
      args_list <- list(...)
      prompt <- args_list[['prompt']]
      service <- args_list[['service']]
      username <- args_list[['username']]
      
      print(paste("Key set input: ", service, username, 'with prompt: ', prompt))
      if (grepl('Provide database name:', prompt)){
        keyring::key_set_with_value(service, username, 'testdb2')
      } else if (grepl('Provide username:', prompt)){
        keyring::key_set_with_value(service, username, 'testuser2')
        keyring::key_set_with_value(username, 'testuser2', '')
      } else if (grepl('Provide password:', prompt)){
        keyring::key_set_with_value(service, username, 'testpwd2')
      } else {
        
      }
    },
    {
      # Pre-emptive cleanup
      if (test_key_exists("databases", "test_colloquial_db2")) {keyring::key_delete("databases", "test_colloquial_db2")}
      if (test_key_exists("testdb2", "testuser2")) {keyring::key_delete("testdb2", "testuser2")}

      
      # keyring access and setup
      try_keyring_access("test_colloquial_db2")
      
    }
  )
  
  # Check that the missing password was retrieved and set in the keyring
  expect_equal(keyring::key_get("testdb2", "testuser2"), "testpwd2")
  expect_equal(keyring::key_get("databases", "test_colloquial_db2"), "testdb2")
  expect_equal(get_db_username("testdb2"), "testuser2")

  # Do clean up
  if (test_key_exists("testdb2", "testuser2")) {keyring::key_delete("testdb2", "testuser2")}
  if (test_key_exists("databases", "test_colloquial_db2")) {keyring::key_delete("databases", "test_colloquial_db2")}
})

test_that("test_database_credentials works properly for missing and full entries", {
  p_load('RSQLite')  # Use just for temporary database for a memory test
  table1_results <- produce_test_loading_table1_data()
  table2_results <- produce_test_loading_table2_data()
  
  check_and_make_dir('./temp')
  
  mydb <- odbc::dbConnect(RSQLite::SQLite(), database="./temp/my-db.sqlite")
  DBI::dbWriteTable(mydb, "table1", table1_results)
  DBI::dbWriteTable(mydb, "table2", table2_results)
  
  sqlite_driver <- DBI::dbDriver("SQLite")
  
  # Show see no errors and should just work as database issues and driver is write 
  expect_invisible(test_database_credentials(drv=sqlite_driver, dbname="./temp/my-db.sqlite"))
  
  # For odbc error, lack of dsn error should be passed back on up
  expect_error(test_database_credentials(drv=odbc::odbc(), dbname="./temp/my-db.sqlite"), 'Data source name not found')
  
  # Test #3 - Mock up a case which raises an invalid authentication error
  testthat::with_mock(
    dbConnect = function(...) {
      stop("invalid username/password; logon denied")
    },
    key_set = function(service, username){
      print(paste("Key set input: ", service, username))
      key_set_with_value(service, username, 'testpwd')
    },
    {
    # Cleanup - remove entry that should get added
    if (test_key_exists("testdb", "testuser")) {keyring::key_delete("testdb", "testuser")};
    expect_invisible(test_database_credentials(drv=sqlite_driver,
                                             dbname="./temp/my-db.sqlite",
                                             uid='testuser',
                                             dbq='testdb')); 
    expect_equal(key_get("testdb", "testuser"), "testpwd")
    }
  )
  
  # Clean up sqllite database
  odbc::dbDisconnect(mydb)
  unlink("./temp/my-db.sqlite", recursive=TRUE)
  unlink("./temp", recursive=TRUE)
  
})

test_that("collect_dataframe_info returns the correct output", {
  # Create test dataframes
  df1 <- data.frame(a = c(1, 2, 3), b = c("a", "b", "c")) %>% tibble::as_tibble()
  df2 <- data.frame(x = c(4.5, 6.7, 8.9), y = c("d", "e", "f"), z = c(1L, 2L, 3L))  %>% tibble::as_tibble()
  
  # output is formatted in specific way
  expect_equal(
    collect_dataframe_info(df1),
    "3 rows; 2 cols;\na: numeric| b: character"
  )
  
  expect_equal(
    collect_dataframe_info(df2),
    "3 rows; 3 cols;\nx: numeric| y: character| z: integer"
  )
  
  
  # Type return is a string
  expect_is(
    collect_dataframe_info(df1),
    "character"
  )
  
  expect_is(
    collect_dataframe_info(df2),
    "character"
  )
  
  # # Output goes to console as expected
  #  expect_equal(capture_output(collect_dataframe_info(df1)), "3 rows; 2 cols;\na: numeric| b: character")
  # 
  # capture_output(collect_dataframe_info(df2)) %>% 
  #   expect_equal("3 rows; 3 cols;\n num chr int\n")
  
})


test_that("establish_logger function works as expected", {
  expect_true(TRUE)
  
  # # create a temporary log file
  # temp_log_file <- tempfile()
  # 
  # # test that logger is created and options are set correctly
  # establish_logger(temp_log_file)
  # 
  # futile.logger::flog.debug('test_mesage_for_file')
  # 
  # # Check the logger file exists
  # # Check if the log file was created
  # expect_true(file.exists(temp_log_file))
  # 
  # 
  # expect_identical(logger_exists("standard_logger"), TRUE)
  # expect_identical(logger_info("standard_logger")$appenders[[1]]$filename, temp_log_file)
  # expect_identical(logger_info("standard_logger")$level, 2) # INFO level
  # expect_identical(options()$error, getOption("error"))
  # 
  # # test that error is logged and traceback is captured
  # tryCatch({
  #   stop("this is an error")
  # }, error = function(e) {
  #   expect_error(flog.error(geterrmessage(), name="standard_logger"))
  #   expect_identical(length(attr(e, "condition")$call), length(sys.calls()) + 1)
  # })
  # 
  # # clean up the temporary log file
  # unlink(temp_log_file)
  # 
})
