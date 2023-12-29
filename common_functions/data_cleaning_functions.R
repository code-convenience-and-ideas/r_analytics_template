#######################################################################
#
#######################################################################
# Logger setup
data_cleaning_func_logger <- lgr::get_logger_glue('root/analytics/functions/data_cleaning')

data_cleaning_func_logger$debug('Entering data cleaning functions')

#######################################################################
# Heavily reused and shared functions
#######################################################################
#' Checks if a file exists before using a function on inputs
#' 
#' This function takes any number of inputs in a list, a filepath, and a function, 
#' and checks if a file exists before using the input function on the inputs. 
#' It stores the result of the function temporarily and then saves it to disk 
#' at the filepath as a parquet format file using the arrow package.
#'
#' @param filepath A character string indicating the file path where the parquet file should be saved
#' @param fun A function that will be applied to the inputs
#' @param refresh_if_exists binary variable specifying whether to load the data from disk if the file already exists
#' @param ... Any number of input objects passed to \code{fun}
#' 
#' @return Returns the result of the function applied to the inputs
#'
#' @examples
#' # Create a test function and apply it using safe_apply()
#' my_fun <- function(x) { x^2 }
#' safe_apply("test.parquet", my_fun, 1:5)
#'
#' @import arrow
#' @importFrom fs file_exists
compute_or_load_function <- function(filepath, fun, refresh_if_exists = FALSE, ...) {
  
  # Only apply the function if the data is missing or we explicitly tell it to
  if (refresh_if_exists || !base::file.exists(filepath)) {
    
    # Otherwise, apply the function to the inputs
    result <- fun(...)
    
    # Save the result as a parquet file
    arrow::write_parquet(result, filepath)
    
    # Return the result
    return(result)
    
  } else {
    # If file already exists, read it and return it
    return(arrow::read_parquet(filepath))
  }
}

# Common modifications and formatting functions for different types of columns
#' Returns the year that the input date falls into, considering an offset for the start of the year
#'
#' The function takes in a date with a year and an offset for the start of the year, and returns the year the date belongs to, based on the offset provided. For example, if the date is 2023-04-07 and the offset is 3, the year returned will be 2022, as the offset of 3 means the year starts at 2022-04-03.
#'
#' @param date_with_year a date object, including the year
#' @param offset_for_yearstart an integer representing the offset for the start of the year. The year starts at the date obtained by subtracting this offset from the first day of the year.
#' @return year_date_belongs_to an integer representing the year the input date falls into
#' @examples
#' dates_year_with_offset(as.Date("2023-04-07"), 3)
#' # returns 2023
#' dates_year_with_offset(as.Date("2024-01-01"), 7)
#' # returns 2023
dates_year_with_offset <- function(date_with_year, offset_for_yearstart){
  year_date_belongs_to <- lubridate::year(date_with_year %m+% months(-offset_for_yearstart))
  return(year_date_belongs_to)
}

#' Returns the difference in days between two dates
#'
#' The function takes in two date objects and returns the number of days between them.
#'
#' @param end_date a date object representing the end date
#' @param start_date a date object representing the start date
#' @return actual_days_between_two_dates an integer representing the number of days between the two input dates
#' @examples
#' days_difference_in_dates(as.Date("2023-04-07"), as.Date("2023-03-31"))
#' # returns 7
#' days_difference_in_dates(as.Date("2023-04-07"), as.Date("2022-12-31"))
#' # returns 97
days_difference_in_dates <- function(end_date, start_date){
  actual_days_between_two_dates <- difftime(end_date, start_date, units="days") %>% as.numeric()
  return(actual_days_between_two_dates)
}

#' Compute years between two dates based on average days in a year.
#'
#' This function takes two dates as input and calculates the approximate number of years between them. The approximation uses 365.25 days per year to account for leap years. 
#' 
#' @param end_date The ending date to calculate the difference from the starting date.
#' @param start_date The starting date to calculate the difference to the ending date.
#' @return Returns the approximate number of years between the two input dates.
approxmate_years_difference <- function(end_date, start_date){
  approximate_years_between_dates <- days_difference_in_dates(end_date, start_date) / 365.25
  return(approximate_years_between_dates)
}

#' Find the maximum value of a vector with a default value if no non-missing values are found.
#'
#' This function finds the maximum value of a vector and returns the result. If there are no non-missing values in the vector, then it returns the specified default value. 
#' 
#' @param col_name The input vector to find the maximum value from.
#' @param default_val The default value to return if there are no non-missing values in the input vector within the group.
#' @return Returns the maximum value of the input vector if there are any non-missing values. If there are no non-missing values, then it returns the specified default value.
max_value_with_default <- function(col_name, default_val=NA){
  max_result <- max(col_name, na.rm=T)
  result_vectors_max_with_default <- ifelse(is.finite(max_result), max_result, default_val)
  return(result_vectors_max_with_default)
}


#' Return the maximum value in a vector, with a default value of 0 if the vector is empty
#' 
#' This function takes in a vector of numbers and returns the maximum value in the vector. If the vector is empty, the function returns 0 as the default value.
#'
#' @param col_name A vector of numbers
#' 
#' @return The maximum value in the input vector, with a default value of 0 if the input vector is empty
#'
#' @examples
#' max_value_default_zero(c(1, 2, 3, 4, 5)) # Output: 5
#' max_value_default_zero(c()) # Output: 0
#' 

max_value_default_zero <- function(col_name){
  result_vectors_max_with_zero <- max_value_with_default(col_name, default_val=0)
  return(result_vectors_max_with_zero)
}


#' Bound a continuous variable between an upper and lower bound
#' 
#' This function takes in a vector of continuous variables and returns a new vector where the values are bounded between an upper and lower bound. Values above the upper bound are replaced with the upper bound, and values below the lower bound are replaced with the lower bound.
#'
#' @param cont_var_vec A vector of continuous variables
#' @param upper_bound The upper bound for the variable
#' @param lower_bound The lower bound for the variable
#' 
#' @return A new vector where the values are bounded between an upper and lower bound
#'
#' @examples
#' bound_continuous_var(c(1, 2, 3, 4, 5), 3, 2) # Output: 2 2 3 3 3
#' bound_continuous_var(c(-1, 0, 1), 2, -2) # Output: -1 0 1
#'

bound_continuous_var <- function(cont_var_vec, upper_bound, lower_bound){
  bounded_continuous_vector <- dplyr::case_when(cont_var_vec>=upper_bound~upper_bound,
                                                cont_var_vec<lower_bound~lower_bound,
                                                TRUE~cont_var_vec)
  return(bounded_continuous_vector)
}


#' Replace missing values in character columns of a dataframe
#'
#' This function takes a dataframe and replaces all missing values in character columns with the string 'missing'.
#'
#' @param complex_dataset A dataframe with character columns containing missing values
#' @return A modified version of complex_dataset with missing values in character columns replaced with the string 'missing'.
#' @examples
#' # Given the following dataset
#' df <- data.frame(x = c("a", NA, "c"), y = c("d", "e", NA), z = c(1, 2, 3))
#'
#' # Replace missing values in df with "missing"
#' missing_char_replacer(df)
#' # x y z
#' # 1 a d 1
#' # 2 missing e 2
#' # 3 c missing 3
missing_char_replacer <- function(complex_dataset){
  # Takes a complicated dataset and fills all missing values in character columns with missing
  complex_dataset %>% dplyr::mutate(dplyr::across(tidyselect::where(is.character), function(x) dplyr::coalesce(x, 'missing')))
}

#######################################################################
# Any additional functions relevant to the current, specific cleaning
#######################################################################