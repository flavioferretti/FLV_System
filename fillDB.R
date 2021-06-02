#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

options(shiny.port = 8080)
options(shiny.host = "0.0.0.0")

library(RMariaDB)
library(quantmod)
library(dplyr)
library(shiny)

## load lib_flv
source("http://192.168.4.1/~flavio/lib/lib_flv.R")

# MariaDB parameters
db='PriceDB'
host='192.168.4.1'
user='flavio'
pw='27182818'

## Read list of symbols from stock_automate DB
my_query="SELECT `symbol`, `rec_id` FROM `nasdaq`" 
list_of_symbols <- ReadParamOnMyDB(my_query, user, pw, 'stock_automate', host, "")


# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
  titlePanel("symbol2DB"),
  
  
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      # in ui
      helpText("Select a symbol to examine. "),
      selectizeInput("yahoo_symbol", "Symblos:", list_of_symbols[,1],selected = "AAPL", multiple = FALSE), 
      
      
      #selectInput("yahoo_symbol", "Symblos:", list_of_symbols[,1],selected = "AAPL", multiple = FALSE), 
      #textInput("yahoo_symbol", "Symbols:", value = "AAPL"),
      dateRangeInput(
        "dates",
        "Date range",
        start = "1980-01-01",
        end = as.character(Sys.Date())
      ),
      selectInput(
        "chart_type_yahoo",
        "Chart type:",
        c("candlesticks", "line"),
        selected = "line",
        multiple = FALSE
      ),
      selectInput(
        "subset_yahoo",
        "Subsets:",
        c(
          "last 1 days",
          "last 3 days",
          "last 1 weeks",
          "last 2 weeks",
          "last 3 weeks",
          "last 1 months",
          "last 3 months",
          "last 6 months",
          "last 1 years",
          "last 3 years",
          "last 5 years",
          "last 10 years",
          "last 15 years",
          "last 20 years",
          "last 30 years"
        ),
        selected = "last 30 years",
        multiple = FALSE
      ),
      selectInput(
        "my_period_yahoo",
        "Chart period:",
        c("none", "daily", "weekly"),
        selected = "none",
        multiple = FALSE
      ),
      selectInput(
        "chart_teme",
        "Chart theme:",
        c("white", "black"),
        selected = "white",
        multiple = FALSE
      ),
      actionButton("goButton", "Store on DB", class = "btn-primary")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(plotOutput("plot"), hr(),
              verbatimTextOutput("verb"))
  ))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  
  updateSelectizeInput(session, 'foo', choices = list_of_symbols, server = TRUE)
  
  # Yahoo world ----------------------------------------------------------
  
  # dataInput_yahoo <- reactive({
  #   if (input$my_period_yahoo == "none") {
  #     na.omit(
  #       getSymbols(
  #         input$yahoo_symbol,
  #         src = "yahoo",
  #         from = input$dates[1],
  #         to = input$dates[2],
  #         auto.assign = FALSE
  #       )
  #     )
  #   }#none
  #   else if (input$my_period_yahoo == "daily") {
  #     na.omit(to.daily(
  #       getSymbols(
  #         input$yahoo_symbol,
  #         src = "yahoo",
  #         from = input$dates[1],
  #         to = input$dates[2],
  #         auto.assign = FALSE
  #       )
  #     ))
  #   }#daily
  #   else if (input$my_period_yahoo == "weekly") {
  #     na.omit(to.weekly(
  #       getSymbols(
  #         input$yahoo_symbol,
  #         src = "yahoo",
  #         from = input$dates[1],
  #         to = input$dates[2],
  #         auto.assign = FALSE
  #       )
  #     ))
  #   }#weekly
  #   
  # })
  
  
  
  
  # store stuff
  observeEvent(input$goButton, {
    # session$sendCustomMessage(type = 'testmessage', message = 'Now store dataset on MySQL Server')
    
    progress <- Progress$new(session, min=1, max=15)
    on.exit(progress$close())
    
    progress$set(message = 'MySQL storage in progress',
                 detail = 'This may take a while...')
    
    xa <- getSymbols(
      input$yahoo_symbol,
      src = "yahoo",
      from = input$dates[1],
      to = input$dates[2],
      auto.assign = FALSE ,
      return.class = 'xts'
    )
    
    # plot stuff
    output$plot <- renderPlot({
      #y <- dataInput_yahoo()
      chartSeries(
        xa,
        name = input$yahoo_symbol,
        subset = input$subset_yahoo,
        theme = chartTheme(input$chart_teme),
        type =  input$chart_type_yahoo ,
        TA = c(addVo())
      )
    })
    
    
    ###
    #MyTickerName <- .Last.value # keep them
    ###
    x <- data.frame(xa)
    x <- na.omit(x)
    
    #debug
    #tail(x)
    #names(x)[0] <- "date"
    x$date <- rownames(x)
    
    names(x)[1] <- "o"
    names(x)[2] <- "h"
    names(x)[3] <- "l"
    names(x)[4] <- "c"
    names(x)[5] <- "v"
    names(x)[6] <- "a"
    
    write.csv(x, "data.csv", row.names = FALSE)
    
    
    ### mariaDB ###
    db <-
      dbConnect(
        RMariaDB::MariaDB(),
        user = user,
        password = pw,
        dbname = db,
        host = host
      )
    #dbWriteTable(storiesDb, value = sampleGardenData, row.names = FALSE, name = "tbl_newspaper_search_results", append = TRUE )
    dbWriteTable(
      db,
      value = x,
      row.names = FALSE,
      name = input$yahoo_symbol,
      overwrite = TRUE
    )
    dbDisconnect(db)
    ###
    rm(x)
    ###
    
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
