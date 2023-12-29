#######################################################################
#
#######################################################################
# Logger setup
general_func_logger <- lgr::get_logger_glue('root/analytics/functions/general')

general_func_logger$debug('Entering general functions')

#######################################################################
# Heavily reused and shared functions
#######################################################################
#' Check if directory exists and create if it does not
#' 
#' @param target_dir Name of the directory to consider
#' 
#' @return TRUE if the directory already exists or is successfully created, otherwise an error is thrown.
check_and_make_dir <- function(target_dir){
  # Per discussion at: https://stackoverflow.com/questions/4216753/check-existence-of-directory-and-create-if-doesnt-exist
  if (!dir.exists(target_dir)){
    dir.create(target_dir)
    return(TRUE)
  } else {
    return(TRUE)
  }
}

# Create a series of shared and commonly used formatting functions to match how I like writing reports
# Alternate definition of round that round up on 0.5 per https://andrewlandgraf.com/2012/06/15/rounding-in-r/ 
#' Round a numeric value to a specified number of digits, rounding up on 0.5
#' 
#' @param x A numeric value to be rounded
#' @param digits The number of digits to round to
#' 
#' @return A numeric value rounded to the specified number of digits
round2 = function(x, digits) {
  posneg = sign(x)
  z = abs(x)*10^digits
  z = z + 0.5 + sqrt(.Machine$double.eps)
  z = trunc(z)
  z = z/10^digits
  z*posneg
}

#' Round power function
#'
#' This function takes a power figure and the number of significant figures and then will round and format that number 
#' appropriately for reporting
#'
#' @param x Numeric value to be rounded and formatted
#' @param power_unit Character string representing the unit of the power figure, e.g. "W" for watts
#' @param sigfigs Number of significant figures to be displayed (default is 1)
#' @return Character string representing the rounded and formatted number with the unit
#' @examples 
#' round_power(123456, "k", 2) # "123.46k"
#' round_power(123456, "k", 3) # "123.456k"
round_power <- function(x, power_unit, sigfigs=1, ...){
  # Taking Qliksenses approach - single string storing all pairings of power
  number_abbreviation <- "0:;3:k;6:M;9:B;12:T15:P;18:E;21:Z;24:Y;-3:m;-6:u;-9:n;-12:p;-15:f;-18:a;-21:z;-24:y;"
  
  # Find the matched power in the number_abbreviation string
  string_matcher <- paste("(-?[0-9]+):", power_unit, ";", collapse="", sep="")
  matched_power <- as.numeric(regmatches(number_abbreviation, regexec(string_matcher, number_abbreviation))[[1]][[2]])
  
  # Calculate the multiplicative factor for displaying the number
  power_multiplicative_factor <- 10 ^ (matched_power)
  
  # Round and format the number with the unit
  nice_number <- paste0(round2(x / power_multiplicative_factor, digits=sigfigs), power_unit)
  nice_number[is.na(x)] <- x[is.na(x)]
  
  return(nice_number)
}

#' Add dollar sign function
#'
#' Takes a number that is formatted as a string and then handles putting the dollar sign in the right place for nice formatting.
#' If the number is negative, it goes -$; if the number is positive, it just goes $.
#'
#' @param x Numeric value to be formatted with a dollar sign
#' @return Character string representing the formatted number with a dollar sign
#' @examples
#' add_dollar("1234.56") # "$1234.56"
#' add_dollar("-1234.56") # "-$1234.56"
add_dollar <- function(x, ...){
  # Find the matched pattern and add dollar sign in the appropriate place
  string_matcher_positive <- paste("^([0-9]+)", collapse="", sep="")
  string_matcher_negative <- paste("^-([0-9]+)", collapse="", sep="")
  dollar_number <- sub(string_matcher_positive, "$\\1", x)
  dollar_number <- sub(string_matcher_negative, "-$\\1", dollar_number)
  
  return(dollar_number)
}


# round_million function
#' Takes a number and explicitly rounds it to a million
#' @param x Numeric value to be rounded
#' @param sigfigs The number of significant digits to be included in the result
#' @return Numeric value rounded to a million
round_million <- function(x, sigfigs=1, ...){
  formatted_x <- round_power(x, "M", sigfigs=sigfigs) # round x to a million with sigfigs number of significant figures
  formatted_x[is.na(x)] <- x[is.na(x)] # handle missing values
  return(formatted_x)
}


# round_million_dollars function
#' Rounds a number to the nearest million and then ads the dollar sign
#' @param x Numeric value to be rounded and formatted with a dollar sign
#' @param sigfigs The number of significant digits to be included in the result
#' @return Numeric value rounded to a million and formatted with a dollar sign
round_million_dollars <- function(x, sigfigs=1, ...){
  formatted_x <- round_million(x, sigfigs=sigfigs) # round x to a million with sigfigs number of significant figures
  formatted_x <- add_dollar(formatted_x) # add a dollar sign to the result
  formatted_x[is.na(x)] <- x[is.na(x)] # handle missing values
  return(formatted_x)
}


# round_dollar function
#' Rounds a number to the nearest integer and adds the dollar sign
#' @param x Numeric value to be rounded and formatted with a dollar sign
#' @return Numeric value rounded to the nearest integer and formatted with a dollar sign
round_dollar <- function(x, ...){
  formatted_x <- round_dollar_power(x, power_unit="", sigfigs=0) # round x to the nearest integer and add a dollar sign
  formatted_x[is.na(x)] <- x[is.na(x)] # handle missing values
  return(formatted_x)
}


# round_dollar_power function
#' Rounds a number to a power with specific significant figures and then handles adding a dollar sign to it
#' @param x Numeric value to be rounded and formatted with a dollar sign
#' @param power_unit The power unit to round to, e.g. "M" for million
#' @param sigfigs The number of significant digits to be included in the result
#' @return Numeric value rounded to a specific power with specific significant figures and formatted with a dollar sign
round_dollar_power <- function(x, power_unit='', sigfigs=2){
  formatted_x <- add_dollar(round_power(x, power_unit = power_unit, sigfigs = sigfigs)) # round x to a specific power with specific significant figures and add a dollar sign
  formatted_x[is.na(x)] <- x[is.na(x)] # handle missing values
  return(formatted_x)
}


#' Round a number into a percentage in a common pattern
#'
#' @param x a numeric vector to be formatted
#' @param no_negative logical; if TRUE, negative numbers will be replaced with 0
#' @param proportion logical; if TRUE, the resulting string will include a "%" symbol at the end
#' @param ... additional arguments to be passed to the round2 function
#'
#' @return a character vector with the formatted values
#'
#' @examples
#' round_percent(c(0.123, -0.456, NA))
round_percent <- function(x, no_negative=TRUE, proportion=TRUE, ...){
  if (no_negative){
    formatted_x <- paste0(round2(pmax(x, 0) * 100, 1), "%")
  } else {
    formatted_x <- paste0(round2(x * 100, 1), "%")
  }
  formatted_x[is.na(x)] <- x[is.na(x)]
  return(formatted_x)
}

#' Format vector data with NA values replaced by a default string
#'
#' @param vector_data a vector of data to be formatted
#' @param format_func a formatting function to be applied to non-NA values
#' @param null_value the value to be used in place of NA values
#' @param ... additional arguments to be passed to the formatting function
#'
#' @return a character vector with the formatted values
#'
#' @examples
#' format_with_nas_replaced(c(123, 456, NA), round_power, "N/A", power_unit="W", sigfigs=3)
format_with_nas_replaced <- function(vector_data, format_func, null_value="-", ...){
  return(dplyr::if_else(!is.na(vector_data), format_func(vector_data, ...), null_value))
}

#' Format a vector of numbers as a rounded dollar value with units and with NA values replaced by a default string
#'
#' @param vector_data a numeric vector of data to be formatted
#' @param null_value the value to be used in place of NA values
#' @param power_unit the units to be used for the output, e.g. "W"
#' @param sigfigs the number of significant figures to be used in the output
#' @param ... additional arguments to be passed to the round_dollar_power function
#'
#' @return a character vector with the formatted values
#'
#' @examples
#' round_dollar_power_with_nas(c(123, 456, NA), null_value="N/A", power_unit="W", sigfigs=3)
round_dollar_power_with_nas <- function(vector_data, null_value="-", power_unit="", sigfigs=1, ...){
  return(format_with_nas_replaced(vector_data, round_dollar_power, null_value, power_unit=power_unit, sigfigs=sigfigs, ...))
}

#' Rounds numbers to a given significant figures with NA handling
#'
#' This function bundles the formatting and NA handling for rounding numbers into a single function.
#'
#' @param vector_data A vector of numerical data to be rounded
#' @param null_value The value to use in place of NAs. Default is "-"
#' @param power_unit A string to include after the rounded number, such as "mm". Default is ""
#' @param sigfigs The number of significant figures to round the data to. Default is 1.
#' @param ... Other arguments to be passed to format_with_nas_replaced function
#' 
#' @return A formatted vector of numerical data with NAs replaced by the specified null value and rounded to the specified number of significant figures with the specified power unit.
#' 
#' @examples
#' round_power_with_nas(c(1.2345, 2.3456, NA, 4.5678), null_value = "NA", power_unit = "mm", sigfigs = 2)
#'
#' @export
round_power_with_nas <- function(vector_data, null_value="-", power_unit="", sigfigs=1, ...){
  return(format_with_nas_replaced(vector_data, round_power, null_value, power_unit=power_unit, sigfigs=sigfigs, ...))
}


#' Rounds percentages to a given decimal place with NA handling
#'
#' This function bundles the formatting and NA handling for rounding percentages into a single function.
#'
#' @param vector_data A vector of percentage data to be rounded
#' @param null_value The value to use in place of NAs. Default is "-"
#' @param no_negative A logical value indicating whether negative values should be truncated to zero. Default is TRUE.
#' @param proportion A logical value indicating whether the input data should be interpreted as proportions. Default is TRUE.
#' @param ... Other arguments to be passed to format_with_nas_replaced function
#'
#' @return A formatted vector of percentage data with NAs replaced by the specified null value and rounded to the specified number of decimal places.
#'
#' @examples
#' round_percent_with_nas(c(0.12345, 0.23456, NA, -0.34567), null_value = "NA", no_negative = FALSE, proportion = FALSE)
#'
#' @export
round_percent_with_nas <- function(vector_data, null_value="-", no_negative=TRUE, proportion=TRUE, ...){
  return(format_with_nas_replaced(vector_data, round_percent, null_value, no_negative=no_negative, proportion=proportion, ...))
}


#' Calculates the lower element for a simple proportion confidence interval using central limit theorem interval
#'
#' This function calculates the lower element for a simple proportion confidence interval using central limit theorem interval. It is important to keep in mind the minimum sample size requirements before this becomes useful.
#'
#' @param p The proportion estimate
#' @param sample_size The sample size
#' @param ci_cutoff The cutoff value for the confidence interval. Default is 0.95.
#'
#' @return The lower element of the confidence interval
#'
#' @examples
#' proportion_lower_intervals(0.5, 100, 0.95)
#'
#' @export
proportion_lower_intervals <- function(p, sample_size, ci_cutoff = 0.95){
  tail_prop <- (1 - ci_cutoff) / 2
  prop_sd <- sqrt(p * (1 - p) / sample_size)
  lower_bound <- p - qnorm(1-tail_prop, 0, 1) * prop_sd
  return(lower_bound)
}

#' proportion_upper_intervals - R function to calculate the upper element for a simple proportion confidence interval
#'
#' Calculates upper element for simple proportion confidence interval using central limit theorem interval. Keep in mind minimum sample size requirements before this becomes useful.
#'
#' @param p A numerical value for the proportion for which you want to calculate a confidence interval.
#' @param sample_size A numerical value for the sample size for which you want to calculate a confidence interval.
#' @param ci_cutoff A numerical value for the percentage confidence level for which you want to calculate the interval.
#' @return Returns the upper bound for a confidence interval.
#'
#' @examples 
#' proportion_upper_intervals(0.5, 100, 0.95)
#'
proportion_upper_intervals <- function(p, sample_size, ci_cutoff){
  tail_prop <- (1 - ci_cutoff) / 2
  prop_sd <- sqrt(p * (1 - p) / sample_size)
  upper_bound <- p + qnorm(1-tail_prop, 0, 1) * prop_sd
  return(upper_bound)
}


#' conf_int_format - R function to format a range into a nice string for a confidence interval
#'
#' Formats a number range for a confidence interval into a nice string for output.
#'
#' @param best_estimate A numerical value for the best estimate or point estimate of the range.
#' @param lower_estimate A numerical value for the lower estimate or lower bound of the range.
#' @param upper_estimate A numerical value for the upper estimate or upper bound of the range.
#' @return Returns a formatted string for a confidence interval.
#'
#' @examples 
#' conf_int_format(0.5, 0.25, 0.75)
#'
conf_int_format <- function(best_estimate, lower_estimate, upper_estimate){
  merged_ci_text <- paste0(best_estimate, " (", lower_estimate, " - ", upper_estimate, ")")
  return(merged_ci_text)
}


#' proportion_conf_int_format - R function to calculate and format a confidence interval using normal assumptions
#'
#' Calculates a formatted confidence interval using normal assumptions for a proportion.
#'
#' @param p A numerical value for the proportion for which you want to calculate a confidence interval.
#' @param sample_size A numerical value for the sample size for which you want to calculate a confidence interval.
#' @param ci_cutoff A numerical value for the percentage confidence level for which you want to calculate the interval.
#' @return Returns a formatted string for a confidence interval.
#'
#' @examples 
#' proportion_conf_int_format(0.5, 100, 0.95)
#'
proportion_conf_int_format <- function(p, sample_size, ci_cutoff){
  lower_bound <- proportion_lower_intervals(p, sample_size, ci_cutoff)
  upper_bound <- proportion_upper_intervals(p, sample_size, ci_cutoff)
  ci_string <- conf_int_format(round_percent(p), round_percent(lower_bound), round_percent(upper_bound))
  return(ci_string)
}

#' Check if a key exists in the keyring
#'
#' This function tries to get a specified key from the keyring, if successful it returns a boolean value indicating if the key exists or not. If unsuccessful it returns FALSE.
#'
#' @param ... A comma-separated list of strings identifying the path of the key to look for.
#'
#' @return A boolean value indicating if the key exists or not.
#'
#' @examples
#' # Check if key 'databases' exists
#' test_key_exists("databases")
#'
#' # Check if key 'databases' exists within the 'prod' folder
#' test_key_exists("prod", "databases")
#'
#' # Check if key 'user' exists within the 'prod' folder and the username 'JohnDoe'
#' test_key_exists("prod", "user", "JohnDoe")
test_key_exists <- function(...){
  entry_exists <- FALSE
  tryCatch(
    {
      entry_results <- key_get(...)
      if (!is.null(entry_results)){
        entry_exists <- TRUE
      }
      return(entry_exists)
    },
    error=function(cond){
      # print(cond)
      # print(cond$message)
      if (grepl("Windows credential", cond$message)){
        return(entry_exists)
      } else {
        stop(cond$message)
      }

    }
  )
}


#' Try to access keyring and retrieve credentials
#' 
#' Try to bring in all the needed information from the key. If there is an error,
#' try to ask user for information to set up keyring entries for specified database
#' 
#' @param database_colloquial_name A string containing the colloquial name of the database
#' 
#' @return Nothing is returned. Keyring credentials are retrieved if successful
#' 
#' @export try_keyring_access
#' 
#' @examples
#' # Try to access keyring and retrieve credentials for "datalake"
#' try_keyring_access("datalake")
#' 
try_keyring_access <- function(database_colloquial_name){
  # expect there to be a db name, uid, and pwd for an entry
  db_id_name <- NULL
  uid <- NULL
  pwd <- NULL
  tryCatch(
    {
      # Try to bring in all the needed information from the key
      db_id_name = keyring::key_get("databases", database_colloquial_name)
      uid = get_db_username(db_id_name)
      
      # Sad artifact of how I handled username access for this, empty "" entry to find user linked to database
      pwd = if (keyring::key_get(db_id_name, uid)=="") NULL else keyring::key_get(db_id_name, uid)
    },
    error=function(cond){
      print('Entered keyring access error pathway')
      # Check if entries existed
      missing_db_name <- is.null(db_id_name)
      missing_username <- is.null(uid)
      missing_password <- is.null(pwd)
      
      # If not, send a message to user to set the values
      db_name_segment <- if (missing_db_name) "Database name; " else ""
      username_segment <- if (missing_username) "username; " else ""
      password_segment <- if (missing_password) "password; " else ""
      
      # Tell the user what is missing
      password_message_dlake <- paste("You are missing entries for that name: ", db_name_segment, username_segment, password_segment)
      message(password_message_dlake)
      print(password_message_dlake)
      
      # Tell the user what I'm going to do
      whatnext_message <- paste("The script will ask you for the missing values. Considered checking your keyring template scripts to make sure entries are up to date.")
      message(whatnext_message)
      print(whatnext_message)
      
      if (missing_db_name){
        missing_database_name_message <- paste("Please enter the name of the database (shortform i.e PRDLKDB):")
        message(missing_database_name_message)
        print(missing_database_name_message)
        keyring::key_set(service="databases", username=database_colloquial_name, prompt='Provide database name:')
      }
      
      if (missing_username){
        temp_db_name <- keyring::key_get("databases", database_colloquial_name)
        missing_username_message <- paste("Please enter the username for the database:", temp_db_name)
        message(missing_username_message)
        print(missing_username_message)
        keyring::key_set(service='user', username=temp_db_name, prompt='Provide username:')
        keyring::key_set_with_value(service=temp_db_name, keyring::key_get('user', temp_db_name), '')
      }
      if (missing_password){
        temp_db_name <- keyring::key_get("databases", database_colloquial_name)
        temp_username <- get_db_username(temp_db_name)
        
        password_message <- paste("Please enter the password for the database:", temp_db_name, ' and user login:', temp_username)
        message(password_message)
        print(password_message)
        keyring::key_set(service=temp_db_name, username=get_db_username(temp_db_name), prompt='Provide password:')
      }
      # source("S:\\Transfer\\AlexB\\scripts\\R\\establish_keyring_entries.R")
    }
  )
}


#' Test the database credentials and prompt user to reset if necessary
#' 
#' Try to make a connection to a database with current credentials. If there is an error,
#' prompt the user to reset their password
#' @param database_name The name of the database to connect to
#' @return Nothing is returned. Prompts user to update keyring if successful
#' @export test_database_credentials
#' @examples test_database_credentials("datalake")
test_database_credentials <- function(...){
  tryCatch(
    {
      # Unpack input arguments
      args_list <- list(...)
      # Try to make a connection to a database with current credentials
      test_engine = dbConnect(
        ...
      )
    },
    error=function(cond){
      print(cond)
      # If password / username did not work -> detect authentication error and if so ask for new 
      # error_is_credential <- readline(prompt="Did login fail due to invalid username / password? (Y/N)")
      if (grepl("invalid username/password; logon denied", cond$message)) {
        db_user <- args_list[['uid']]
        db_name <- args_list[['dbq']]
        # print(paste("FBT: Key set input: ", db_user, db_name))
        password_message_dlake <- paste("Please enter a new password for your ", db_name, " user name", db_user)
        message(password_message_dlake)
        print(password_message_dlake)
        keyring::key_set(db_name, db_user)
      } else {
        password_message_dlake <- paste("Other errors")
        message(password_message_dlake)
        print(password_message_dlake)
        stop(cond$message)
      }
    }
  )
}

#' Pivot group column to measure columns
#'
#' This function pivots a data frame, taking a single grouping column and spreading the
#' data wide so that each group has a column with its data
#'
#' @param summary_data the data frame to be pivoted
#' @param target_group_column the column name for the grouping column
#' @return Returns a data frame where the data is pivoted to show each group's data in its own column.
#' @examples
#' data(mtcars)
#' mtcars %>%
#'   pivot_group_column_to_measure_colums(target_group_column = "cyl")
#'
#' @import tidyr
pivot_group_column_to_measure_colums <- function(summary_data, target_group_column){
  return(
    summary_data %>%
      tidyr::pivot_longer(!target_group_column) %>%
      tidyr::pivot_wider(names_from=target_group_column) %>%
      dplyr::rename("Measure"="name")
  )
}

#' Establish a logger
#'
#' This function sets up a logger with a specific output file, and applies logging levels and options
#' for capture of errors and tracebacks
#'
#' @param logger_output_file file path and name for the logger's output file
#' @param option_logger_template options template to pass for logger creation
#' @return Returns nothing. It creates the logger "standard_logger".
#' @examples
#' establish_logger("my_logfile.log", option_logger_template="default_template")
#' 
#' @import futile.logger
establish_logger <- function(logger_output_file, option_logger_template="."){}
# Strongly considering swapping over to lgr package
#   # Create a file appender that points to the test log file
#   file_appender <- futile.logger::appender.file(logger_output_file)
# 
#   # Create a new logger and assign the file appender to it
#   # default_log_file_path <- file.path(logs_dir, logger_output_file)
#   logger_object <- futile.logger::flog.logger(name="standard_logger", appender=file_appender)
# 
#   # Set the level of logging to perform e.g INFO and above, debugging, warnings, etc
#   futile.logger::flog.threshold(futile.logger::DEBUG, name="standard_logger")
# 
#   # Capture errors and traceback in the main logger
#   options(error = function() { futile.logger::flog.error(geterrmessage(), name="standard_logger") ; traceback() ; stop() })
# 
#   # Send the session info the debug logger at the initial
#   futile.logger::flog.debug(utils::sessionInfo())
# 
# }

#' Collect information about a data frame
#'
#' This function collects information about a data frame such as the number of rows and columns and the column names and types.
#' 
#' @param dataframe_to_log the data frame to collect information about
#'
#' @return a string containing the collected information
#'
#' @examples
#' collect_dataframe_info(iris)
#' collect_dataframe_info(mtcars)
collect_dataframe_info <- function(dataframe_to_log){
  
  # Collect number of rows, number of columns
  dataframe_dims <- dataframe_to_log %>% dim()
  dataframe_dim_message <- paste(dataframe_dims[1], " rows; ",  dataframe_dims[2], ' cols;', sep="")
  message(dataframe_dim_message)
  
  # Collect column name + types
  output_column_dataset <-   dataframe_to_log %>%
    dplyr::summarise_all(class) %>%
    tidyr::pivot_longer(cols=everything()) %>%
    dplyr::mutate(combined_summary=paste0(name, ': ', value, sep=""))
  dataframe_structure_string <- paste(output_column_dataset$combined_summary, collapse="| ")
  message(dataframe_structure_string)
  
  # Combine the output string
  final_dataframe_output_string <- paste(dataframe_dim_message, dataframe_structure_string, sep="\n")
  
  return(final_dataframe_output_string)
}

#' Rename columns of a dataframe using a vector
#'
#' This function takes a dataframe and a vector of column names, and renames columns of the dataframe to match those in the vector. Only columns with matching names are renamed.
#'
#' @param dataframe_to_name The dataframe whose columns are to be renamed
#' @param naming_vector A vector containing column names that match the columns in the dataframe.
#'
#' @return The renamed dataframe
#'
#' @examples
#' data(mtcars)
#' rename_dataframe_with_vector(mtcars, c("weight"="wt", "miles per gallon"="mpg"))  # Would change wt column to weight and mpg column to miles per gallon
#'
rename_dataframe_with_vector <- function(dataframe_to_name, naming_vector){
  
  # setup renaming columns
  present_columns <- naming_vector %in% (dataframe_to_name %>% colnames())
  present_column_renamer <- naming_vector[present_columns]
  
  # rename them
  return(
    dataframe_to_name %>%
      dplyr::rename(dplyr::all_of(present_column_renamer))
  )
}

#' Reverse a Named Vector
#'
#' This function reverses the order of the values in a named vector while keeping the names as their original value.
#'
#' @param named_vec A named vector whose values are to be reversed.
#' @return The reversed named vector with the original names.
#' @examples
#' named_vec <- c("weight"="wt", "miles per gallon"="mpg")
#' reverse_named_vector(named_vec)  # Would not be vector c("wt"="weight", "mpg"="miles per gallon")
reverse_named_vector <- function(named_vec){
  rev_vec <- names(named_vec)
  names(rev_vec) <- named_vec
  return(rev_vec)
}

#######################################################################
# Any additional functions relevant to the current, specific general
#######################################################################

