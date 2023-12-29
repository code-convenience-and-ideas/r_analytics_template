# Run all of the set up code form the analysis
# source("./main.R")
source(here::here("scripts", "r", "01_configuration.R"))

report_document <- file.path(scripts_dir, "report.rmd")

output_file_paths <- file.path(reports_dir, "default_report")
report_output_file_paths <- c(paste(output_file_paths,
                                    "service_delivery_report",
                                    ".html",
                                    sep = ""),
                              paste(output_file_paths,
                                    "service_delivery_report",
                                    ".docx",
                                    sep = ""))

output_flag_default_report <- rmarkdown::render(
    report_document,
    output_file = report_output_file_paths,
    output_format = c("html_document2", "word_document2"))
