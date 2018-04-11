library(shiny)
library(shinydashboard)
library(rhandsontable)
library(shinyjs)

# UI
shiny::shinyUI(
  shinydashboard::dashboardPage(skin = "blue",
  ######################################################################################
  dashboardHeader(title = "Pegasus"
                  # dropdownMenu(type = "tasks",
                  #   taskItem("Variables validated"),
                  #   menuItem("AA", tabName = "results", icon = icon("info-circle")))
                  # dropdownMenu(type = "notification",
                  #   messageItem(from = "x", message = "aaa"),
                  #   menuItem("AA", tabName = "results", icon = icon("dashboard")),
                  #   menuItem("BB", tabName = "Settings", icon = icon("dashboard")))
  ),
  ######################################################################################
  dashboardSidebar(
    shinyjs::useShinyjs(),
    h4("   ////////////////////////////////// "),
    hr(),
    sidebarMenu(
      menuItem(HTML("<b>DataBase</b>"), tabName = "_database", icon = icon("database")
        # menuSubItem("Tables Analysis", tabName = "_satfam", icon = icon("bars")),
        # menuSubItem("Import", tabName = "_satfam", icon = icon("bars"))
       ),
      menuItem(HTML("<b>Production Analysis</b>"), icon = icon("certificate"),
        menuSubItem("Production Stats", tabName = "_prodstats", icon = icon("bar-chart")),
        menuSubItem("Production Plots", tabName = "_prodplot", icon = icon("line-chart")),
        menuSubItem("Chan Analysis", tabName = "_chan", icon = icon("angle-double-up"))),
      menuItem(HTML("<b>Injection Analysis</b>"), tabName = "_inject", icon = icon("arrow-circle-down")),
      menuItem(HTML("<b>Interference Analysis</b>"), tabName = "_interf", icon = icon("arrows-alt")),
      menuItem(HTML("<b>Well Typing</b>"), tabName = "_welltyping", icon = icon("th-large")),
      menuItem(shiny::hr()),   #  HTML("hr {border-top: 1px solid;}")),
      menuItem(HTML("<b>Reporting</b>"), tabName = "_report", icon = icon("file-text")),
      menuItem(HTML("<b>Help</b>"), tabName = "_support", icon = icon("life-buoy"), selected = TRUE),
      menuItem(HTML("<b><font color=#367FA9>I/O</font></b>"),
                shiny::tags$div(id = 'close', "Close",
                               class = "btn action-button",
                               stile = "text-align: left;",
                               onclick = "setTimeout(function(){window.close();},500);"),
                shiny::tags$div(id = 'new_session',
                                class = "btn action-button",
                                HTML("<a id='new_session' href='./'>Reload</a>")),
                               icon = icon("power-off") )
    )
  ),
  ######################################################################################
  dashboardBody(
    tabItems(
      tabItem(tabName = "_database",
               tabsetPanel(
                  tabPanel("Import DB"),
                  tabPanel("Preview",
                           shiny::dataTableOutput("rawdata_preview")
                           ),
                  tabPanel("Summary",
                           shiny::tableOutput("rawdata_summary")
                           )
                )
      ),
      #-------------------------------------------------------------------------------
      tabItem(tabName = "_prodstats",
                tabsetPanel(
                  tabPanel("Dashboard"),
                  tabPanel("Bar plot"

                           ),
                  tabPanel("Tree maps")
                )
              ),
      #-------------------------------------------------------------------------------
      tabItem(tabName = "_prodplot",
          sidebarLayout(
            sidebarPanel(
              shiny::selectInput("welllist", "Select well to plot:", c("a", "b", "c"), selected = "a", multiple = TRUE),
              shiny::sliderInput("datefilter", "Dates:", 1950, 2020, c(2000, 2018))
            ),
            mainPanel(
                    tabsetPanel(
                  tabPanel("Rates",
                           shiny::tableOutput("data_summary"),
                           shiny::plotOutput("plot_rates", width = 1200, height = 800)
                           ),
                  tabPanel("WCUT vs CumOil",
                           shiny::selectInput("plot_wcut1_color", "Color by:", c("fm", "well"), selected = "fm"),
                           shiny::plotOutput("plot_wcut1", width = 1200, height = 800),
                           shiny::br(),
                           shiny::plotOutput("plot_wcut2", width = 1200, height = 800)
                           ),
                  tabPanel("WOR and GOR",
                           shiny::plotOutput("plot_worgor", width = 1200, height = 800), shiny::br(),
                           shiny::numericInput("gor1", "Gor 1 (scf/stb):", 100),
                           shiny::numericInput("gor2", "Gor 2 (scf/stb):", 250),
                           shiny::numericInput("gor3", "Gor 3 (scf/stb):", 500),
                           shiny::numericInput("gor4", "Gor 4 (scf/stb):", 1000),
                           shiny::plotOutput("plot_cumgor", width = 1200, height = 800), shiny::br(),
                           shiny::plotOutput("plot_gortime", width = 1200, height = 800), shiny::br()
                           ),
                  tabPanel("Inflow Performance"),
                  tabPanel("Other variables")
                )
            )
          )
              ),
      #-------------------------------------------------------------------------------
      tabItem(tabName = "_chan",
              shiny::h4("Chan plots"),
              shiny::selectInput("chan_var", "Select Variable:", c("WOR", "GOR"), selected = "WOR"),
              shiny::selectInput("chan_xaxis", "Select X axis variable:", c("Time", "CumOil", "CumFluid"), selected = "Time"),
              shiny::plotOutput("plot_chan", width = 1200, height = 800)
              ),
      #-------------------------------------------------------------------------------
      tabItem(tabName = "_inject",
                shiny::h4("Injection analysis")
              ),
      #-------------------------------------------------------------------------------
      tabItem(tabName = "_interf",
              tabsetPanel(id = "navbar",
                tabPanel("Voronoi Grid"),
                tabPanel("Distance Matrix",
                         shiny::br()),
                tabPanel("Drained area")
              )
      ),
      #-------------------------------------------------------------------------------
      tabItem(tabName = "_welltyping",
              shiny::br(),
              shiny::p("WELLTYPING"),
              shiny::br()
              # shiny::downloadButton("aaa", label = "Download Results")
    ),
      #-------------------------------------------------------------------------------
      tabItem(tabName = "_report",
              shiny::br(),
              shiny::p("REPORT"),
              shiny::br()
              # shiny::downloadButton("aaa", label = "Download Results")
    ),
      #-------------------------------------------------------------------------------
      tabItem(tabName = "_support",
              tabsetPanel(
                tabPanel("About",
                         shiny::br(),
                         shiny::br(),
                         shiny::img(src = "img/Pegasus_Logo.png", align = "center", width = '50%'),
                         shiny::br(),
                         shiny::h5("Developed by Francesco Giorgetti - April 2018"),
                         shiny::br(),
                         shiny::h6("Pegasus makes your production analysis fly!")
                         ),
                tabPanel("Help",
                         shiny::h4("Help page:")),
                tabPanel("Credits",
                         shiny::h4("Credits:"),
                         shiny::h5("Packages dependecy:"),
                         shiny::h6("Imports: shiny, shinydashboard, rhandsontable, shinyjs, dplyr, ggplot2, gridExtra, truncdist, mc2d,
                              petroreadr, DT, pse, GGally, readxl, openxlsx"),
                         shiny::h6("Suggests: knitr, rmarkdown, roxygen2, testthat")
                         )
              )
      )
   )
  )
)
)




