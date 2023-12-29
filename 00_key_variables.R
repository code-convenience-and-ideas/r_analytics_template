# generic functions ------------
#' Works out start of year as a posixCT type
#'
#' @param year year to use in time variable
#' @param starting_month the month which starts the year
#'
#' @return a POSIXct date for the year-month starting day of year
#' @export
#'
#' @examples
#' calc_year_start(2022, 07)
calc_year_start <- function(year, starting_month) {
  return(as.POSIXct(paste0(year, "-", starting_month, "-01"), tz = "UTC"))
}

#' Pads a months number to a string of width 2 with zeroes
#'
#' @param month_num
#'
#' @return
#' @export
#'
#' @examples
#' month_number_leftpad(1)
month_number_leftpad <- function(month_num) {
  stringr::str_pad(month_num, width = 2, side = "left", pad = "0")
}

#' Splits out the relevant date terms and returns them in a list
#'
#' @param year_start_string string as an example for the start of year
#' @param reference_time the reference time period to compare to
#'
#' @return a list with variance date related summaries
#' @export
#'
#' @examples
#' calc_key_year_terms("01/07/2022", Sys.time())
calc_key_year_terms <- function(year_start_string, reference_time) {
  # Need a reference date to for comparison
  reference_date <- as.Date(reference_time)

  # Offsets for other calculations
  year_starting_month <- month_number_leftpad(
    lubridate::month(year_start_string)
  )
  year_month_offset <- as.integer(year_starting_month) - 1

  # reference year info and progress
  reference_year <- lubridate::year(
    reference_date %m+% months(-year_month_offset, abbreviate = FALSE)
  )

  # Pull out key derivative values
  reference_year_start <- calc_year_start(reference_year, year_month_offset)
  reference_year_end <- calc_year_start(reference_year + 1, year_month_offset)
  reference_year_days_since_start <- difftime(
    reference_time,
    reference_year_start,
    units = "days"
  ) |>
    as.numeric() |>
    floor()

  # Can be at most 100% completed
  reference_year_portion_completed <- pmin(
    reference_year_days_since_start / 365.25, 1
  )

  # Collate results to output list
  output_list <- list()
  output_list[["year"]] <- reference_year
  output_list[["year_start"]] <- reference_year_start
  output_list[["year_end"]] <- reference_year_end
  output_list[["year_portion"]] <- reference_year_portion_completed
  output_list[["days_in"]] <- reference_year_days_since_start
  output_list[["month_offset"]] <- year_month_offset

  return(output_list)
}

# logger setup -----------------------
key_variable_logger <- lgr::get_logger_glue("root/analytics/work/key_variables")
key_variable_logger$debug("Entering key variables script")

###############################
# DATE VARIABLES
###############################
# calendar variables ------------------

# Example start of year
example_financial_year_start <- "01/07/2022"

# Current dates and time
current_date <- Sys.Date()
current_time <- Sys.time()

# Year offsets
current_year_financial_values <- calc_key_year_terms(
  example_financial_year_start,
  current_time
)

###################################################################
# REUSED CONSTANTS AND SELECTIONS
###################################################################
# constants and selections ----------------------

key_variable_logger$debug("Leaving key variables script")
