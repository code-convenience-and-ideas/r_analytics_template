# Import needed packages and functions to use for testing
# source("../import.R")
# source("../configurationFunctions.R")
# source("../dataLoadingFunctions.R")

message(paste('My current path in DataLoading tets is: ', getwd()))

# Keyring entry comes in format expected
testthat::test_that("Data loading testing", {
  # Set some key ring values
  expect_true(TRUE)
})

testthat::test_that("compressed_file_name", {
  # Set some key ring values
  expect_identical(compressed_file_name("temp_file.xlsx", TRUE), "temp_file.xlsx.gz")
  expect_identical(compressed_file_name("temp_file.xlsx", FALSE), "temp_file.xlsx")
})

testthat::test_that("lowercase_columns", {
  temp_tribble <- dplyr::tribble(
    ~TEST, ~test2, ~aAbB, ~Colname
  )
  
  # Set some key ring values
  expect_identical(temp_tribble %>% lowercase_columns() %>% colnames(), c("test", "test2", 'aabb', 'colname'))
})

testthat::test_that("lowercase_columns", {
  temp_tribble <- dplyr::tribble(
    ~TEST, ~test2, ~aAbB, ~Colname
  )
  
  # Set some key ring values
  expect_identical(temp_tribble %>% uppercase_columns() %>% colnames(), c("TEST", "TEST2", 'AABB', 'COLNAME'))
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




testthat::test_that("load_proc_wrt_data", {
  p_load('RSQLite')  # Use just for temporary database for a memory test
  table1_results <- produce_test_loading_table1_data()
  table2_results <- produce_test_loading_table2_data()
  
  
  check_and_make_dir('./temp')
  
  mydb <- odbc::dbConnect(RSQLite::SQLite(), "./temp/my-db.sqlite")
  DBI::dbWriteTable(mydb, "table1", table1_results)
  DBI::dbWriteTable(mydb, "table2", table2_results)


  testthat::expect_identical(  load_proc_wrt_data(mydb, "select * from table1", "./temp/table1.parquet", FALSE) %>%
                       dplyr::mutate(measure_data=.Date(measure_data)) %>% 
                       as.data.table(), table1_results %>% 
                       as.data.table())
  
  testthat::expect_identical(  load_proc_wrt_data(mydb, "select * from table2", "./temp/table2.parquet", FALSE) %>%
                       dplyr::mutate(measure_data=.Date(measure_data)) %>% 
                       as.data.table(), table2_results %>% 
                       as.data.table())
  
  odbc::dbDisconnect(mydb)
  unlink("./temp/my-db.sqlite", recursive=TRUE)
  unlink("./temp", recursive=TRUE)
})

testthat::test_that("load_from_database", {
  p_load('RSQLite')  # Use just for temporary database for a memory test
  # Set up table example results
  table1_results <- produce_test_loading_table1_data()
  table2_results <- produce_test_loading_table2_data()

  # Set up temp dir
  temp_dir <- './temp'
  check_and_make_dir(temp_dir)
    
  # Set up temporary sqlite database
  mydb <- odbc::dbConnect(RSQLite::SQLite(), file.path(temp_dir, "my-db.sqlite"))
  DBI::dbWriteTable(mydb, "table1", table1_results)
  DBI::dbWriteTable(mydb, "table2", table2_results)

  
  # Create scripts path and etc for this work
  disk_file_path <- list(
    "t1"="./temp/table1.parquet",
    "t2"="./temp/table2.parquet"
  )
  
  testing_db_link <- list(
    "t1"=mydb,
    "t2"=mydb
  )
  
  testing_script_paths <- list(
    "t1"="./temp/table1_query.txt",
    "t2"="./temp/table2_query.txt"
  )
  
  readr::write_file("select * from table1", "./temp/table1_query.txt")
  readr::write_file("select * from table2", "./temp/table2_query.txt")
  
  testthat::expect_identical(  load_from_database("t1", testing_script_paths, disk_file_path, testing_db_link, FALSE) %>%
                       dplyr::mutate(measure_data=.Date(measure_data)) %>% 
                       as.data.table(), table1_results %>% 
                       as.data.table())
  
  testthat::expect_identical(  load_from_database("t2", testing_script_paths, disk_file_path, testing_db_link, FALSE) %>%
                       dplyr::mutate(measure_data=.Date(measure_data)) %>% 
                       as.data.table(), table2_results %>% 
                       as.data.table())
  
  # Clean up the by product files used in testing
  odbc::dbDisconnect(mydb)
  unlink("./temp/my-db.sqlite", recursive=TRUE)
  unlink("./temp", recursive=TRUE)
})

testthat::test_that("load_from_disk", {
  table1_results <- produce_test_loading_table1_data()
  table3_results <- produce_test_loading_table3_data_simpler()
  
  check_and_make_dir('./temp')
  arrow::write_parquet(table1_results, "./temp/test.parquet")
  utils::write.table(table3_results, "./temp/test.tsv", sep="\t", row.names=FALSE)
  writexl::write_xlsx(table1_results, "./temp/test.xlsx")
  
  file_link_list <- list(
    "parquet"="./temp/test.parquet",
    "tsv"="./temp/test.tsv",
    'xlsx'="./temp/test.xlsx"
  )
  
  testthat::expect_identical(load_from_disk("parquet", file_link_list, FALSE, data_type="parquet"), table1_results)
  # Much looser test for delimited files as type preservation with csv is extremely difficult
  testthat::expect_true(all(load_from_disk("tsv", file_link_list, FALSE, data_type="delimited") == table3_results))
  testthat::expect_identical(load_from_disk("xlsx", file_link_list, FALSE, data_type="excel") %>%
                               dplyr::mutate(measure_data=as.Date(measure_data)),
                             table1_results)
  unlink("./temp", recursive=TRUE)
})

testthat::test_that("load_sql_data", {
  p_load('RSQLite')  # Use just for temporary database for a memory test
  
  # Set up table example results
  table1_results <- produce_test_loading_table1_data()
  table2_results <- produce_test_loading_table2_data()
  
  
  # make directory for temporary files
  temp_dir <- './temp'
  check_and_make_dir(temp_dir)
  
  # Set up temporary sqlite database
  mydb <- odbc::dbConnect(RSQLite::SQLite(), file.path(temp_dir, "my-db.sqlite"))
  DBI::dbWriteTable(mydb, "table1", table1_results)
  DBI::dbWriteTable(mydb, "table2", table2_results)

  # Start logger needed for function
  default_log_file_path <- file.path("./temp", 'standard.log')
  futile.logger::flog.logger("standard_logger", futile.logger::DEBUG, appender=flog.appender(appender.tee(default_log_file_path)))
  
  # Set the level of logging to perform e.g INFO and above, debugging, warnings, etc
  futile.logger::flog.threshold(futile.logger::INFO, name="standard_logger")
  
  # Capture errors and traceback in the main logger
  options(error = function() { flog.error(geterrmessage(), name="standard_logger") ; traceback() ; stop() })
  
  futile.logger::flog.debug(sessionInfo())
  
  futile.logger::flog.info("Running data log test")
  
  # Create scripts path and etc for this work
  disk_file_path <- list(
    "t1"="./temp/table1.parquet",
    "t2"="./temp/table2.parquet"
  )
  
  testing_db_link <- list(
    "t1"=mydb,
    "t2"=mydb
  )
  
  testing_script_paths <- list(
    "t1"="./temp/table1_query.txt",
    "t2"="./temp/table2_query.txt"
  )
  
  readr::write_file("select * from table1", "./temp/table1_query.txt")
  readr::write_file("select * from table2", "./temp/table2_query.txt")
  
  expected_results_list <- list("t1"=table1_results, "t2"=table2_results)
  
  # Test initial database load for matching data
  testthat::expect_identical(    load_sql_data(testing_script_paths, disk_file_path, testing_db_link, FALSE, TRUE, FALSE) %>%
                                   purrr::map(~(.x %>% dplyr::mutate(measure_data=.Date(measure_data)) %>% 
                         as.data.table())), expected_results_list %>% purrr::map(~.x %>% as.data.table()))
  
  # Test disk load from data only when data does not already exist
  testthat::expect_identical(    load_sql_data(testing_script_paths, disk_file_path, testing_db_link, FALSE, TRUE, TRUE) %>%
                                   purrr::map(~(.x %>% dplyr::mutate(measure_data=.Date(measure_data)) %>% 
                                 as.data.table())), expected_results_list %>% purrr::map(~.x %>% as.data.table()))

  # Test disk load from data as full defult
  testthat::expect_identical(    load_sql_data(testing_script_paths, disk_file_path, testing_db_link, FALSE, FALSE, TRUE) %>%
                                   purrr::map(~(.x %>% dplyr::mutate(measure_data=.Date(measure_data)) %>% 
                                 as.data.table())), expected_results_list %>% purrr::map(~.x %>% as.data.table()))
  
  # Clean up the by product files used in testing
  odbc::dbDisconnect(mydb)
  unlink("./temp/my-db.sqlite", recursive=TRUE)
  unlink("./temp", recursive=TRUE)
})
