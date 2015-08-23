library(shiny)

ui <- fluidPage(
  titlePanel("Stock Metrics"),
  sidebarLayout(
    #sidebar panel  
    sidebarPanel(
      wellPanel(
        checkboxInput("check", "45 Day Moving Average"),
        checkboxInput("check2", "Mean Return"),
        #dateRangeInput("DateRange", "date", start = "2015-01-01", end = "2015-02-01"),
        submitButton("Apply Changes"))),
      #main panel
      mainPanel(
        tabsetPanel(
          #TAB 1 - Returns
          tabPanel("Stock Returns", 
                   verticalLayout(
                              splitLayout(plotOutput("tab1A"),
                                          plotOutput("tab1B")),
                              plotOutput("tab1C"))),
          #TAB 2 - Return Distribution
          tabPanel("Return Distribution", 
                   verticalLayout(
                              splitLayout(plotOutput("plot2A"),
                                          plotOutput("plot2B")),
                              splitLayout(plotOutput("plot2C"),
                                          plotOutput("plot2D")))),
          
          tabPanel("About",
                   h3("About"),
                   p("this application is created in R by Adam")))
      )
  )
)