# Import needed packages and functions to use for testing
# source("../import.R")
# source("../dataAnalysisFunctions.R")

# Keyring entry comes in format expected
testthat::test_that("Data analysis testing", {
  # Set some key ring values
  expect_true(TRUE)
})

# Test create_blank_dataset_row function
test_that("create_blank_dataset_row function works as expected", {
  
  # Simple dataframe to use for testing
  test_df <- data.frame(a = c(1,2,3), b = c("x", "y", "z"), stringsAsFactors = FALSE)
  
  # Test that function returns a data frame
  expect_is(create_blank_dataset_row(test_df), "data.frame")
  
  # Test that function returns a data frame with one row
  expect_equal(nrow(create_blank_dataset_row(test_df)), 1)
  
  # Test that function returns a data frame with the same number of columns as the input data frame
  expect_equal(ncol(create_blank_dataset_row(test_df)), ncol(test_df))
  
  # Test that function returns a data frame with the same column names as the input data frame
  expect_equal(colnames(create_blank_dataset_row(test_df)), colnames(test_df))
  
  # Test that function returns a data frame with all empty cells
  expect_true(all(is.na(create_blank_dataset_row(test_df))))
  
  # Test that function returns a data frame with correct types
  expect_equal(sapply(create_blank_dataset_row(test_df), class), c("a"="numeric", "b"="character"))
  
})


# Call the test function using test_that()
test_that("add_blank_dataset_row function tests", {
  # Create a test dataset
  test_dataset <- data.frame(x = c(1, 2, 3), y = c("a", "b", "c"))
  
  # Test that the output is a dataframe
  expect_is(add_blank_dataset_row(test_dataset), "data.frame")
  
  # Test that the output has an additional row
  expect_equal(nrow(add_blank_dataset_row(test_dataset)), nrow(test_dataset) + 1)
  
  # Test that the output has the correct column names
  expect_equal(colnames(add_blank_dataset_row(test_dataset)), colnames(test_dataset))
})

# Define test cases
test_that("create_blank_dataset_char_col", {
  
  # Test that the function returns a tibble
  dataset <- mtcars
  result <- create_blank_dataset_char_col(dataset)
  expect_is(result, "tbl_df")
  
  # Test that the tibble has a single column
  expect_equal(ncol(result), 1)
  
  # Test that the tibble has as many rows as the dataset
  expect_equal(nrow(result), nrow(dataset))
  
  # Test that the tibble column is character type
  expect_equal(class(result$blank), "character")
  
  # Test that the column values are blank
  expect_true(all(result$blank == ""))
})

test_that("add_blank_dataset_columns function works as expected", {
  
  # Create a test dataset
  test_data <- tibble::tibble(a = 1:3, b = c("a", "b", "c"))
  
  # Test that a single column is added
  result <- add_blank_dataset_columns(test_data)
  expect_equal(ncol(result), ncol(test_data) + 1)
  
  # Test that multiple columns can be added and names follow expect convention
  result <- add_blank_dataset_columns(test_data)
  result <- add_blank_dataset_columns(result)
  expect_equal(ncol(result), ncol(test_data) + 2)
  expect_equal(sum(result$`blank...3` == ""), nrow(result))
  expect_equal(sum(result$`blank...4` == ""), nrow(result))
  
  # Test that the added columns are all blank
  result <- add_blank_dataset_columns(test_data)
  expect_equal(sum(result$blank == ""), nrow(result))
  
})



# test cases for the reverse_nested_list_with_shared_sublist_names function
test_that("reverse_nested_list_with_shared_sublist_names works as expected", {
  # create a nested list with shared sublist names
  list1 <- list(a = list(a = "hello", b = 1),
                b = list(a = "world", b = 2),
                c = list(a = "!", b = 3))
  
  # expected result after reversing the nested list with shared sublist names
  expected <- list(a = list(a = "hello", b = "world", c = "!"),
                   b = list(a = 1, b = 2, c = 3))
  
  # test if the function returns the expected output
  expect_equal(reverse_nested_list_with_shared_sublist_names(list1), expected)
  
  # test if the function returns a list
  expect_is(reverse_nested_list_with_shared_sublist_names(list1), "list")
  
  # test if the function returns a nested list
  expect_is(reverse_nested_list_with_shared_sublist_names(list1)[[1]], "list")
  
  # test if the function works for an empty list - beware result making little sense
  expect_equal(reverse_nested_list_with_shared_sublist_names(list(list())), list())
})



# create a test case
test_that("unpack_comparisons_list returns correct output", {
  # create a list containing comparison data frames
  list1 <- list(a = list(p = data.frame(z=c(1,2,NA), q=c(1, 1, 1)), r = data.frame(z=c(2,3,NA), q=c(2, 2, 2))),
                b = list(p = data.frame(z=c(3,4,NA), q=c(3, 3, 3)), r = data.frame(z=c(4,5,NA), q=c(4, 4, 4))))
  
  # Provide the expected output
  output_list <- list(p=data.frame(z=c(1, 2, 3, 4), q=c(1, 1, 3, 3)),
                      r=data.frame(z=c(2, 3, 4, 5), q=c(2, 2, 4, 4)))
  
  
  # test that unpack_comparisons_list returns a list
  expect_is(unpack_comparisons_list(list1), "list")
  
  # test that unpack_comparisons_list returns the correct number of rows
  expect_equal(nrow(unpack_comparisons_list(list1)[[1]]), 4)
  
  # test that unpack_comparisons_list returns the correct number of columns
  expect_equal(ncol(unpack_comparisons_list(list1)[[1]]), 2)
  
  # test that unpack_comparisons_list removes NA values
  expect_false(any(is.na(unpack_comparisons_list(list1)[[1]])))
})


test_that("return_named_vector returns  correct output", {
  # Test if function returns a named vector
  result <- return_named_vector(c(1,2,3), c("a","b","c"))
  expect_named(result)
  
  # Test if function returns correct named vector
  result <- return_named_vector(c(1,2,3), c("a","b","c"))
  expect_equal(result, c(a=1, b=2, c=3))
  
  # Test if function returns vector with correct length
  result <- return_named_vector(c(1,2,3), c("a","b","c"))
  expect_equal(length(result), 3)
  
  # Test if function returns NA for missing names
  result <- return_named_vector(c(1,2,3), c("a","b"))
  expect_true(is.na(names(result)[3]))
})

# Define test cases
test_that("generate_varValue_colnames function works as expected", {
  
  # Test case 1: check column names generated correctly
  expect_equal(generate_varValue_colnames(c(1,2,3)), c("Var1Values", "Var2Values", "Var3Values"))
  
  # Test case 2: check column names generates expected undesired output
  expect_equal(generate_varValue_colnames(c()), "VarValues")
  
  # Test case 3: check column names generated for a vector of length 1
  expect_equal(generate_varValue_colnames(c('a')), "Var1Values")
  
  # Test case 4: check column names generated for a vector of length 10
  expect_equal(generate_varValue_colnames(1:10), paste0("Var", 1:10, "Values"))
})


test_that("create_generic_varValue_colnames returns correct output", {
  # Test case 1: Empty input vector
  input_vec <- character()
  expected_output <- character()
  # Should fial - paste still creates a result despite zero level but one vector stay zero length - mismatch causes failure
  expect_error(create_generic_varValue_colnames(input_vec))  #  "'names' attribute [1] must be the same length as the vector [0]"
  
  # Test case 2: Non-empty input vector
  input_vec <- c("Species", "Type", "Color")
  expected_output <- c("Var1Values"="Species", "Var2Values"="Type", "Var3Values"="Color")
  output <- create_generic_varValue_colnames(input_vec)
  expect_equal(output, expected_output)
  
  # Test case 3: Input vector of length 1
  input_vec <- "Species"
  expected_output <- c("Var1Values"="Species")
  output <- create_generic_varValue_colnames(input_vec)
  expect_equal(output, expected_output)
})

# define the test cases
test_that("create_generic_var_to_group_mapping_df returns the correct output", {
  test_columns <- c("Species", "Type", "Color")
  
  # test case 1: test that function returns a dataframe
  output <- create_generic_var_to_group_mapping_df(test_columns)
  expect_true(is.data.frame(output))
  
  # test case 2: test that function returns a dataframe with the correct number of rows and columns
  output <- create_generic_var_to_group_mapping_df(test_columns)
  expect_equal(nrow(output), 1)
  expect_equal(ncol(output), length(test_columns))
  
  # test case 3: test that function returns a dataframe with the correct column names
  output <- create_generic_var_to_group_mapping_df(test_columns)
  expect_equal(names(output), paste("var", seq(test_columns), sep=""))
  
  # test case 4: test that function returns a dataframe with the correct values
  output <- create_generic_var_to_group_mapping_df(c("Species", "Type", "Color"))
  expected_output <- tibble::tibble(var1 = c("Species"), var2=c("Type"), var3=c("Color"))
  expect_equal(output, expected_output)
})

test_that("generate_group_value_key works as expected", {
  # Test one - simple case all results exist
  expect_equal(generate_group_value_key(c("Male", "20-30")), "Male; 20-30")
  
  # Behaviour for missing values
  key <- generate_group_value_key(character(0))
  expect_equal(key, "")
  
  # How it handle missing values
  key <- generate_group_value_key(c("Male", NA, "20-30"))
  expect_equal(key, "Male; NA; 20-30")
})

# define test cases
test_that("apply_function_on_group_vars_and_rename works as expected", {
  
  # create a test dataset
  test_data <- tibble::tibble(
    id = 1:6,
    group1 = c("a", "a", "b", "b", "c", "c"),
    group2 = c("x", "x", "y", "y", "z", "z"),
    value = c(1, 2, 3, 4, 5, 6)
  )
  
  # test case 1: check that the function returns a data frame
  output <- apply_function_on_group_vars_and_rename(test_data, c("group1", "group2"), function(x) x %>% dplyr::summarize(mean_value = mean(value)))
  expect_is(output, "tbl_df")
  
  # test case 2: check that the output has the correct number of rows
  output <- apply_function_on_group_vars_and_rename(test_data, c("group1", "group2"), function(x) x %>% dplyr::summarize(mean_value = mean(value)))
  expect_equal(nrow(output), 3)
  
  # test case 3: check that the output has the expected columns
  output <- apply_function_on_group_vars_and_rename(test_data, c("group1", "group2"), function(x) x %>% dplyr::summarize(mean_value = mean(value)))
  expect_identical(colnames(output), c("Var1Values", "Var2Values", "mean_value", "var1", "var2", "group_key", "nvars"))
  
  # test case 4: check that the output has the expected values
  output <- apply_function_on_group_vars_and_rename(test_data, c("group1", "group2"), function(x) x %>% dplyr::summarize(mean_value = mean(value)))
  expected_output <- tibble::tibble(
    Var1Values = c("a", "b", "c"),
    Var2Values = c("x", "y", "z"),
    mean_value = c(1.5, 3.5, 5.5),
    var1 = c("group1", "group1", "group1"),
    var2 = c("group2", "group2", "group2"),
    group_key = c("group1; group2", "group1; group2", "group1; group2"),
    nvars = c(2, 2, 2)
  )
  expect_equal(output, expected_output)
  
})


# Define the test cases in a single call to test_that
test_that("matrix_to_tibble function works as expected", {
  
  # Test that a matrix is converted to a tibble
  mat <- matrix(1:9, ncol=3)
  tb <- matrix_to_tibble(mat)
  expect_is(tb, "tbl_df")
  
  # Test that a single-row matrix is converted to a tibble
  mat <- matrix(1:3, ncol=3)
  tb <- matrix_to_tibble(mat)
  expect_equal(nrow(tb), 1)
  
  # Test that a single-column matrix is converted to a tibble
  mat <- matrix(1:3, nrow=3)
  tb <- matrix_to_tibble(mat)
  expect_equal(ncol(tb), 1)
  
  # Test that the number of rows and columns are preserved
  mat <- matrix(1:9, ncol=3)
  tb <- matrix_to_tibble(mat)
  expect_equal(nrow(tb), nrow(mat))
  expect_equal(ncol(tb), ncol(mat))
  
  # Test that the number of rows and columns are preserved
  mat <- matrix(1:9, ncol=3)
  tb <- matrix_to_tibble(mat)
  expected_tibble <- tibble::tibble(V1=c(1, 2, 3), V2=c(4, 5, 6), V3=c(7, 8, 9))
  expect_equal(expected_tibble, tb)
  
})

# Define test cases
test_that("nwise_permutation_matrix returns the correct output", {
  
  # Test case 1: n is greater than the length of char_vector
  expect_error(nwise_permutation_matrix(c("A", "B", "C"), 4), "n < m")
  
  # Test case 2: n equals the length of char_vector
  expected_output <- matrix(c("A", "B", "C", "C", "B", "A"), nrow = 2, ncol = 3, byrow=TRUE)
  # colnames(expected_output) <- c("V1", "V2", "V3")
  expect_equal(nwise_permutation_matrix(c("A", "B", "C"), 3), expected_output)
  
  # Test case 3: n is less than the length of char_vector
  expected_output <- matrix(c("A", "B", "A", "C", "B", "C", "C", "B", "C", "A", "B", "A"), nrow = 6, ncol = 2, byrow=TRUE)
  expect_equal(nwise_permutation_matrix(c("A", "B", "C"), 2), expected_output)
})


test_that("nwise_combination_matrix function returns expected output", {
  char_vector <- c("A", "B", "C")
  n <- 2
  
  # Test output is a matrix
  expect_true(is.matrix(nwise_combination_matrix(char_vector, n)))
  
  # Test output dimensions
  expect_equal(dim(nwise_combination_matrix(char_vector, n)), c(3, 2))
  
  # Test output values
  expect_equal(nwise_combination_matrix(char_vector, n)[1, ], c("A", "B"))
  expect_equal(nwise_combination_matrix(char_vector, n)[2, ], c("A", "C"))
  
  # Test 1 sample case, just vector as matrix
  expect_equal(nwise_combination_matrix(char_vector, 1), char_vector %>% matrix())
})

# Test the basic functionality of the function
test_that("vector_to_nwise_comb_tibble returns the expected tibble", {
  expected_output <- tibble::tribble(
    ~V1, ~V2,
    "A", "B",
    "A", "C",
    "B", "C"
  )
  output <- vector_to_nwise_comb_tibble(c("A", "B", "C"), 2)
  expect_equal(output, expected_output)
  
  # Test the function with a longer input vector
  expected_output <- tibble::tribble(
    ~V1, ~V2,
    "A", "B",
    "A", "C",
    "A", "D",
    "B", "C",
    "B", "D",
    "C", "D"
  )
  output <- vector_to_nwise_comb_tibble(c("A", "B", "C", "D"), 2)
  expect_equal(output, expected_output)
  
  # Test the function with a larger n value
  expected_output <- tibble::tribble(
    ~V1, ~V2, ~V3,
    "A", "B", "C",
    "A", "B", "D",
    "A", "C", "D",
    "B", "C", "D"
  )
  output <- vector_to_nwise_comb_tibble(c("A", "B", "C", "D"), 3)
  expect_equal(output, expected_output)
  
  # Test the function with an n value larger than the length of the input vector
  expect_error(vector_to_nwise_comb_tibble(c("A", "B", "C"), 4), "n < m")
})


# Test the basic functionality of the function
test_that("vector_to_nwise_perm_tibble returns the expected tibble", {
  expected_output <- tibble::tribble(
    ~V1, ~V2,
    "A", "B",
    "A", "C",
    "B", "C",
    "C", "B",
    "C", "A",
    "B", "A"
  )
  output <- vector_to_nwise_perm_tibble(c("A", "B", "C"), 2)
  expect_equal(output, expected_output)
  
  # Test the function with a longer input vector
  expected_output <- tibble::tribble(
    ~V1, ~V2,
    "A", "B",
    "A", "C",
    "A", "D",
    "B", "C",
    "B", "D",
    "C", "D",
    "D", "C",
    "D", "B",
    "D", "A",
    "C", "B",
    "C", "A",
    "B", "A"
  )
  output <- vector_to_nwise_perm_tibble(c("A", "B", "C", "D"), 2)
  expect_equal(output, expected_output)
  
  # Test the function with a larger n value
  expected_output <- tibble::tribble(
    ~V1, ~V2, ~V3,
    "A", "B", "C",
    "A", "B", "D",
    "A", "C", "D",
    "B", "C", "D",
    "D", "C", "B",
    "D", "C", "A",
    "D", "B", "A",
    "C", "B", "A"
  )
  output <- vector_to_nwise_perm_tibble(c("A", "B", "C", "D"), 3)
  expect_equal(output, expected_output)
  
  # Test the function with an n value larger than the length of the input vector
  expect_error(vector_to_nwise_perm_tibble(c("A", "B", "C"), 4), "n < m")
})


# Create a test context using `test_that()`
test_that("generate_ordered_col_names function tests", {
  
  # Test 1: check if the function returns the correct output for a numeric vector
  expected_output1 <- c("First variable considered", "Second variable considered", "Third variable considered")
  generated_output1 <- generate_ordered_col_names(c(5, 10, 15))
  expect_equal(generated_output1, expected_output1)
  
  # Test 2: check if the function returns the correct output for a character vector
  expected_output2 <- c("First variable considered", "Second variable considered", "Third variable considered")
  generated_output2 <- generate_ordered_col_names(c("low", "medium", "high"))
  expect_equal(generated_output2, expected_output2)
  
})


test_that("renamer_df_to_vector gives expected output", {

  # test function input validation
  mapping_df <- data.frame(old_name = c("x", "y"), new_name = c("X1", "Y1"))
  expect_error(renamer_df_to_vector(NULL, "new_name", "old_name"))
  expect_error(renamer_df_to_vector(mapping_df, NULL, "old_name"))
  expect_error(renamer_df_to_vector(mapping_df, "new_name", NULL))
  
  # test function output format
  res <- renamer_df_to_vector(mapping_df, "new_name", "old_name")
  expect_is(res, "character")
  expect_true(all(names(res) %in% c("x", "y")))
  
  # Expected values
  expected_res <- c("x" = "X1", "y" = "Y1")
  expect_equal(res, expected_res)
})


test_that("rename_succesive_factors renames columns correctly", {
  # Create a test dataset
  df <- data.frame(x = c("a", "b", "c", "d"), y = c("A", "B", "C", "D"), z = c("A", "A", "A", "A"))
  # Create a renaming vector
  renaming_vector <- c("A"="new_a", "B"="new_b", "C"="new_c", "D"="new_d")
  
  
  
  # Test that renaming works correctly for one column
  df1 <- rename_succesive_factors(df, "y", renaming_vector)
  expect_equal(df1$y, c("new_a", "new_b", "new_c", "new_d"))
  
  # Test that renaming works correctly for multiple columns
  df2 <- rename_succesive_factors(df, c("y", "z"), renaming_vector)
  expect_equal(df2$y, c("new_a", "new_b", "new_c", "new_d"))
  expect_equal(df2$z, c("new_a", "new_a", "new_a", "new_a"))
  
  # Test that renaming works correctly with missing values
  df3 <- data.frame(x = c("a", "b", "c", "d"), y = c("A", NA, "C", "D"), z = c("A", "A", "A", "A"))
  df4 <- rename_succesive_factors(df3, c("y", "z"), renaming_vector)
  expect_equal(df4$y, c("new_a", NA, "new_c", "new_d"))
  expect_equal(df4$z, c("new_a", "new_a", "new_a", "new_a"))
})
