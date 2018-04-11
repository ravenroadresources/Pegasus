
require(dplyr)
# require(xlsx)
# require(rJava)
Sys.setenv("R_ZIPCMD" = "C:/R-3.4.2/Rtools/bin/zip.exe")

package_path <- system.file(package = "Pegasus")
# dp_user_path <- file.path(package_path, "shiny", "www", "default", "distro_params_user.txt")
# dp_def_path <- file.path(package_path, "shiny", "www", "default", "distro_params_default.txt")
# # dp_def1_path <- file.path(package_path, "shiny", "www", "default", "distro_params_default_1.txt")
# vbs_path <- file.path(package_path, "extdata", "Flamingo.vbs")
# prosper_path <- file.path(package_path, "extdata", "Flamingo_0.0.4.Out")
# xls_path <- file.path(package_path, "extdata", "Flamingo.xlsm")
# rmd_path <- file.path(package_path, "shiny", "www", "rmd", "report.Rmd")
# report_path <- file.path(package_path, "shiny", "www", "rmd", "report.html")
# analog_default_path <- file.path(package_path, "extdata", "Analogs_example.xlsx")

rsource_db_path <- file.path(package_path, "shiny", "www", "db.R")
rsource_prod_path <- file.path(package_path, "shiny", "www", "prod.R")

db_file <- file.path(package_path, "extdata", "Abanico_MONTHLYPROD.xlsx")


# SERVER
shiny::shinyServer(function(input, output) {

  observe({
    if (input$close > 0) shiny::stopApp()  # stop shiny
  }) # stop shinyapp

  # DATABASE
  source(rsource_db_path, local = TRUE)
  # PROD ANALYSIS
  source(rsource_prod_path, local = TRUE)


})


