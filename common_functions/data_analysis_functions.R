#######################################################################
#
#######################################################################
# Logger setup
data_analysis_func_logger <- lgr::get_logger_glue('root/analytics/functions/data_analysis')

data_analysis_func_logger$debug('Entering data analysis functions')


#######################################################################
# Heavily reused and shared functions
#######################################################################
# Function for generating a highly formatted output table similar to what you see in papers which has measures
# Across many groups


#' Creates a blank row for the given dataset
#'
#' This function creates a blank row with column names for the given dataset. It can be used to add a new row to the dataset that will be filled later.
#'
#' @param dataset a dataframe to create a blank row for
#' @return a dataframe with a single blank row
#' @examples
#' create_blank_dataset_row(mtcars)
#' create_blank_dataset_row(iris)
create_blank_dataset_row <- function(dataset){
  dataset %>%
    dplyr::filter(dplyr::row_number()==0) %>%
    dplyr::add_row()
}

#' Adds a blank row to the given dataset
#'
#' This function adds a blank row with column names to the given dataset. It can be used to add a new row to the dataset that will be filled later.
#'
#' @param dataset a dataframe to add a blank row to
#' @return a dataframe with an additional blank row at the start
#' @examples
#' add_blank_dataset_row(mtcars)
#' add_blank_dataset_row(iris)
add_blank_dataset_row <- function(dataset){
  dplyr::bind_rows(create_blank_dataset_row(dataset), dataset)
}


#' Create a blank dataset character column
#'
#' This function creates a new character column in a given dataset with blank values.
#'
#' @param dataset A dataframe
#' @return A tibble with a single blank column
#' @examples
#' create_blank_dataset_char_col(mtcars)
#'
#' @import dplyr
#' @import tibble
#' @importFrom tibble as_tibble
create_blank_dataset_char_col <- function(dataset){
  tibble::as_tibble(list(blank=rep("", nrow(dataset))))
}

#' Add blank columns to a dataset
#'
#' This function adds one or more blank character columns to a given dataset.
#'
#' @param dataset A dataframe
#' @return A tibble with additional blank columns
#' @examples
#' add_blank_dataset_columns(mtcars)
#'
#' @import dplyr
#' @importFrom tibble as_tibble
add_blank_dataset_columns <- function(dataset){
  dataset %>%
    dplyr::bind_cols(create_blank_dataset_char_col(.))
}


#' Reverses nested list with shared sublist names
#'
#' This function reverses a nested list with shared sublist names.
#'
#' @param ls a nested list with shared sublist names
#'
#' @return a reversed nested list with shared sublist names
#'
#' @examples
#' # create a nested list with shared sublist names
#' list1 <- list(a = list(a = "hello", b = 1),
#'               b = list(a = "world", b = 2),
#'               c = list(a = "!", b = 3))
#'
#' # reverse the nested list with shared sublist names
#' everse_nested_list_with_shared_sublist_names(list1)
#'
#' @export
reverse_nested_list_with_shared_sublist_names <- function(input_list) { # @Josh O'Brien
  # https://stackoverflow.com/questions/15263146/revert-list-structure
  # get sub-elements in same order
  ordered_subelements <- lapply(input_list, `[`, names(input_list[[1]]))
  # stack and reslice
  reorder_stacked_results <- apply(do.call(rbind, ordered_subelements), 2, as.list)
  
  return(reorder_stacked_results)
}


#' Unpacks comparisons list
#'
#' This function unpacks a list containing comparison results, combines them and drop empty rows
#'
#' @param ls a list containing comparison results
#'
#' @return an unpacked list containing comparison results
#'
#' @examples
#' # create a list containing comparison results
#' list2 <- list(a = list(a = c(1,2,NA), b = c(2,3,NA)),
#'               b = list(a = c(3,4,NA), b = c(4,5,NA)))
#'
#' # unpack the list containing comparison results
#' unpack_comparisons_list(list2)
#'
#' @import dplyr
#' @export
unpack_comparisons_list <- function(ls){
  unpacked_merged_results <- reverse_nested_list_with_shared_sublist_names(ls) %>%
    purrr::map(~.x%>%dplyr::bind_rows()%>%dplyr::filter(!is.na(.[[1]])))
  
  return(unpacked_merged_results)
}

#' Create a named vector from two separate vectors
#'
#' This function takes two separate vectors, value_vector and names_vector, and creates a named vector where the elements of value_vector are assigned to the corresponding elements in names_vector.
#'
#' @param value_vector A vector of values to be assigned to names in names_vector
#' @param names_vector A vector of names to be assigned to values in value_vector
#' @return A named vector where each element in value_vector is assigned to the corresponding element in names_vector.
#' @examples
#' return_named_vector(value_vector = c(1,2,3), names_vector = c("a","b","c"))
#' # a b c
#' # 1 2 3
return_named_vector <- function(value_vector, names_vector){
  new_named_vector <- value_vector
  names(new_named_vector) <- names_vector
  return(new_named_vector)
}


#' Generate column names for a dataframe based on input vector
#'
#' This function takes a vector and generates a character vector of column names with the format "Var#Values" where # is the position of the value in the input vector.
#'
#' @param val_vector A vector for which to generate column names
#' @return A character vector of column names with the format "Var#Values" where # is the position of the value in val_vector.
#' @examples
#' generate_varValue_colnames(c(10,20,30))
#' # [1] "Var1Values" "Var2Values" "Var3Values"
generate_varValue_colnames <- function(val_vector){
  return(paste('Var', seq(val_vector), 'Values', sep=""))
}


#' Append variable names to a vector for renaming columns to generic ordered "values" columns
#'
#' @param grouping_var_vector A vector of variable names to be used for grouping
#' 
#' @return A vector with renamed column names
#'
#' @examples
#' append_var_names(c("Species", "Type", "Color"))
#'
#' @export
create_generic_varValue_colnames <- function(grouping_var_vector){
  new_column_selector <- return_named_vector(grouping_var_vector, generate_varValue_colnames(grouping_var_vector))
  return(new_column_selector)
}

#' Create a dataframe that stores the column name of the source column being used in generic values columns
#'
#' @param grouping_var_vector A vector of variable names to be used for grouping
#' 
#' @return A dataframe with renamed column names
#'
#' @examples
#' create_generic_var_to_group_mapping(c("Species", "Type", "Color"))
#'
#' @export
create_generic_var_to_group_mapping_df <- function(grouping_var_vector){
  constant_var_names <- setNames(grouping_var_vector, paste('var', 1:length(grouping_var_vector), sep="")) %>%
    as.data.frame() %>%
    t() %>%
    tibble::as_tibble()
  return(constant_var_names)
}

#' Generate a unique key for a given combination of grouping variable values.
#'
#' Given a vector of grouping variable values, this function generates a unique string key
#' that can be used to identify a group. The key is generated by concatenating the values in the
#' vector together, separated by a semicolon and a space.
#'
#' @param grouping_var_vector A vector of grouping variable values.
#' @return A string key for the given grouping variable values.
#'
#' @examples
#' generate_group_value_key(c("Male", "20-30"))  # "Male; 20-30"
#' generate_group_value_key(c("Female", "20-30"))  # "Female; 20-30"
#'
generate_group_value_key <- function(grouping_var_vector){
  group_key <- paste(grouping_var_vector, collapse='; ')
  return(group_key)
}


#' Apply a function on the specified grouping variables
#' 
#' Given a dataset, applies a function to summarize values for each group of rows
#' defined by a set of grouping variables. 
#' 
#' @param data_to_summarise The dataset containing columns to be summarized.
#' @param grouping_vars_vector A character vector of column names to group by.
#' @param group_func A function to apply to each group of rows.
#' 
#' @return A dataframe with summarized data.
#'
#' @examples
#' apply_function_on_group_vars_and_rename(iris, c("Species"), function(x) x %>% summarize(mean_sepal_length = mean(Sepal.Length)))
#' apply_function_on_group_vars_and_rename(mtcars, c("cyl", "gear"), function(x) x %>% summarize(mean_mpg = mean(mpg)))
#' 
apply_function_on_group_vars_and_rename <- function(data_to_summarise, grouping_vars_vector, group_func){
  # grouping_vars_vector <- groupings_vars_vector
  #
  new_column_selector <- create_generic_varValue_colnames(grouping_vars_vector)
  var_constants <- create_generic_var_to_group_mapping_df(grouping_vars_vector)
  group_value_key <- generate_group_value_key(grouping_vars_vector)
  
  
  summarised_data <- data_to_summarise %>%
    dplyr::mutate_at(vars(all_of(grouping_vars_vector)), as.character) %>%
    dplyr::mutate_at(vars(all_of(grouping_vars_vector)), function(x) dplyr::coalesce(x, 'Missing Value')) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(new_column_selector))) %>%
    group_func() %>%
    dplyr::bind_cols(var_constants) %>%
    dplyr::mutate(group_key=group_value_key, nvars=length(grouping_vars_vector)) %>%
    dplyr::ungroup()
  
  return(summarised_data)
}

#' Convert matrix to tibble
#' 
#' This function converts a matrix to a tibble.
#' 
#' @param data_matrix A matrix to be converted to a tibble.
#' @return A tibble version of the matrix.
#' 
#' @examples
#' matrix_to_tibble(matrix(1:9, ncol=3))
matrix_to_tibble <- function(data_matrix){
  return(data_matrix %>%
           as.data.frame() %>%
           tibble::as_tibble())
}

#' Generate an n-wise permutation matrix
#' 
#' This function generates an n-wise permutation matrix.
#' 
#' @param char_vector A character vector.
#' @param n An integer specifying the size of permutations.
#' @return A permutation matrix as a tibble.
#' 
#' @examples
#' nwise_permutation_matrix(c("A", "B", "C"), 2)
#' https://stackoverflow.com/questions/11095992/generating-all-distinct-permutations-of-a-list-in-r
nwise_permutation_matrix <- function(char_vector, n){
  cbind(combn(char_vector, n), combn(rev(char_vector), n)) %>%
    t()
}

#' Generate an n-wise combination matrix
#' 
#' This function generates an n-wise combination matrix.
#' 
#' @param char_vector A character vector.
#' @param n An integer specifying the number of combinations.
#' @return A combination matrix as a tibble.
#' 
#' @examples
#' nwise_combination_matrix(c("A", "B", "C"), 2)
nwise_combination_matrix <- function(char_vector, n){
  combn(char_vector, n) %>%
    t()
}

#' Convert vector to n-wise combination tibble
#' 
#' This function converts a character vector to an n-wise combination tibble.
#' 
#' @param char_vector A character vector.
#' @param n An integer specifying the number of combinations.
#' @return An n-wise combination tibble.
#' 
#' @examples
#' vector_to_nwise_comb_tibble(c("A", "B", "C"), 2)
vector_to_nwise_comb_tibble <- function(char_vector, n){
  nwise_tibble <- nwise_combination_matrix(char_vector, n) %>%
    matrix_to_tibble() %>%
    dplyr::distinct()
  
  return(nwise_tibble)
}

#' Convert vector to n-wise permutation tibble
#' 
#' This function converts a character vector to an n-wise permutation tibble.
#' 
#' @param char_vector A character vector.
#' @param n An integer specifying the number of permutations.
#' @return An n-wise permutation tibble.
#' 
#' @examples
#' vector_to_nwise_perm_tibble(c("A", "B", "C"), 2)
vector_to_nwise_perm_tibble <- function(char_vector, n){
  nwise_tibble <- nwise_permutation_matrix(char_vector, n) %>%
    matrix_to_tibble() %>%
    dplyr::distinct()
  
  return(nwise_tibble)
}

#' Rename columns in a dataframe based on a mapping dataframe
#'
#' This function takes a mapping dataframe that has old and new column names and returns a vector that can be used to rename columns in a dataframe.
#'
#' @param mapping_df A dataframe with two columns, one for the old column names and one for the new column names
#' @param new_name_col The name of the column in mapping_df that has the new column names
#' @param old_name_col The name of the column in mapping_df that has the old column names
#'
#' @return A named character vector that can be used to rename columns in a dataframe.
#'
#' @examples
#'
#' mapping_df <- data.frame(old_name = c("x", "y"), new_name = c("X1", "Y1"))
#' col_rename_vector <- renamer_df_to_vector(mapping_df, "new_name", "old_name")
#'
#' data_to_rename <- data.frame(x = c(1,2), y = c(3,4))
#' names(data_to_rename) <- col_rename_vector[names(data_to_rename)]
#'
#' # Output should be a dataframe with X1 and Y1 as column names
#' print(data_to_rename)
#'
renamer_df_to_vector <- function(mapping_df, new_name_col, old_name_col){
  col_renamer_vector <- mapping_df %>% dplyr::pull(dplyr::all_of(new_name_col))
  names(col_renamer_vector) <- mapping_df %>% dplyr::pull(dplyr::all_of(old_name_col))
  return(col_renamer_vector)
}

#' Generates ordered column names based on a vector of values.
#'
#' This function takes a vector of values as input and generates ordered column names based on the position of the values in the vector. The generated column names have the format "Nth variable considered", where N is the ordinal representation of the position of the value in the vector.
#'
#' @param val_vector a vector of values to generate ordered column names for.
#' @return a vector of ordered column names.
#' @examples
#' generate_ordered_col_names(c(1,2,3))
#' generate_ordered_col_names(c("low", "medium", "high"))
generate_ordered_col_names <- function(val_vector){
  paste(
    tools::toTitleCase(english::ordinal(seq(val_vector))), " variable considered",sep=""
  )
}


#' Rename successive factors in a dataset using a renaming vector
#'
#' This function takes a dataset, a vector of column names to rename, and a renaming vector. It then replaces each factor level in the target columns with the corresponding value in the renaming vector.
#'
#' @param target_var_columns A vector of the column names to rename in renaming_dataset
#' @param renaming_dataset A dataframe to which the column renaming is applied
#' @param renaming_vector A named vector where the names are the old factor levels and the values are the new factor levels to replace with
#' @return A modified version of renaming_dataset with the target_var_columns renamed according to the renaming_vector.
#' @examples
#' # Given the following dataset
#' df <- data.frame(x = c("a", "b", "c", "d"), y = c("A", "B", "C", "D"), z = c("A", "A", "A", "A"))
#' # And the following renaming vector
#' renaming_vector <- c("A"="new_a", "B"="new_b", "C"="new_c", "D"="new_d")
#'
#' # Rename x and y in df according to renaming_vector
#' rename_succesive_factors(df, c("y", "z"), renaming_vector)
#' # x y z
#' # a new_a new_a
#' # b new_b new_a
#' # c new_c new_a
#' # d new_d new_a
rename_succesive_factors <- function(renaming_dataset, target_var_columns, renaming_vector){
  renaming_dataset %>%
    dplyr::mutate(
      dplyr::across(dplyr::all_of(
        target_var_columns
      ), .fns = function(x) unname(renaming_vector[x]))
    )
}

#######################################################################
# Any additional functions relevant to the current, specific analysis
#######################################################################
