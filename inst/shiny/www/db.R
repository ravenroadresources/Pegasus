

rawdata_r <- shiny::reactive({
  temp <- readxl::read_excel(db_file, sheet = "MONTHLYPROD")
  return(temp)
})


output$rawdata_preview <- shiny::renderDataTable({
  data <- rawdata_r()
  return(head(data))
  })

output$rawdata_summary <- shiny::renderTable({
  data <- rawdata_r()
  return(summary(data))
  })


#/////////////////////////////////////////

# con_r <- shiny::reactive({
#   db <- file.path(input$db_file)
#   RODBC::odbcDriverConnect(db)
#   # RODBC::odbcConnectAccess2007(db)
# })
#
# tables_r <- shiny::reactive({
#   RODBC::sqlTables(con_r())
# })
#
# variables_r <- shiny::reactive({
#   RODBC::sqlColumns(con_r(), "DAILYPROD") #input$table_selected)
# })
#
# # rawdata_r <- shiny::reactive({
# #   vars <- input$vars_selected
# #   table <- input$table_selected
# #   sqlQuery(con_r(), paste("SELECT", vars, "FROM", table))
# #
# # })
#
#
# output$tables_out <- shiny::renderDataTable({
#   tables_r()
# })
