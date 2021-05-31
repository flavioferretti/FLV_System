options(shiny.port = 8080)
options(shiny.host = "0.0.0.0")

library(RMariaDB)
library(quantmod)
library(dplyr)
library(shiny)

## load lib_flv
source("http://192.168.4.1/~flavio/lib/lib_flv.R")

# MariaDB parameters
db = 'PriceDB'
host = '192.168.4.1'
user = 'flavio'
pw = '27182818'

## Read list of symbols from stock_automate DB
my_query = "show tables"
list_of_symbols <- ReadParamOnMyDB(my_query, user, pw, db, host, "")
#

library(shiny)

# Define UI for application that draws a histogram



ui <- navbarPage(
    title = 'flvSystem 0.0.1',
    tabPanel('DB Explorer',
             fluidPage(
                 sidebarLayout(
                     sidebarPanel(
                         # Symbol select
                         helpText("Select a symbol to examine. "),
                         selectizeInput(
                             "symbol",
                             "Symblos:",
                             list_of_symbols[, 1],
                             selected = "AAPL",
                             multiple = FALSE
                         ),
                         # from - to
                         helpText("Total retrieved data lenght: "),
                         verbatimTextOutput("sample_lenght"),
                         numericInput(
                             "bars",
                             "Number of bar in chart:",
                             100,
                             min = 20,
                             max = 100000
                         ),
                         selectInput(
                             "chart_type",
                             "Chart type:",
                             c("candlesticks", "line"),
                             selected = "candlesticks",
                             multiple = FALSE
                         ),
                         selectInput(
                             "subset",
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
                             "my_period",
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
                         selectInput(
                             "studies",
                             "Studies:",
                             c("OFF", "ON"),
                             selected = "OFF",
                             multiple = FALSE
                         ),
                         conditionalPanel(
                             condition = "input.studies == 'ON'",
                             selectInput(
                                 "studies_hist",
                                 "Display Histogram",
                                 c("OFF", "ON"),
                                 selected = "OFF",
                                 multiple = FALSE
                             ),
                             checkboxInput("studies_SMA", "SMA", value = FALSE),
                             sliderInput(
                                 "sma_period",
                                 "SMA Period:",
                                 min = 3,
                                 max = 200,
                                 value = 14,
                                 step = 1
                             ),
                             checkboxInput("studies_SMA2", "SMA #2", value = FALSE),
                             sliderInput(
                                 "sma_period2",
                                 "SMA Period #2:",
                                 min = 3,
                                 max = 200,
                                 value = 50,
                                 step = 1
                             ),
                             checkboxInput("studies_MACD", "MACD", value = FALSE),
                             checkboxInput("studies_BBands", "BBands", value = FALSE),
                             sliderInput(
                                 "vol_sma_period",
                                 "Volumes SMA Period:",
                                 min = 0,
                                 max = 100,
                                 value = 0,
                                 step = 1
                             ),
                             numericInput(
                                 "vol_sma_factor",
                                 "Volumes SMA chart factor:",
                                 10,
                                 min = 100,
                                 max = 1000000,
                                 value = 1000
                             )
                         ),
                         
                     ),
                     #sidebarPanel
                     
                     
                     mainPanel(verbatimTextOutput("HeaderText"),
                               plotOutput("plot1"),
                               conditionalPanel(
                                   condition = "input.studies_hist == 'ON'",
                                   plotOutput("hist")
                               ))
                 )
             )),
    tabPanel('Length menu',        DT::dataTableOutput('ex2')),
    tabPanel('No pagination',      DT::dataTableOutput('ex3')),
    tabPanel('No filtering',       DT::dataTableOutput('ex4')),
    tabPanel('Function callback',  DT::dataTableOutput('ex5'))
)




################################################################################
# Define server logic ##########################################################
server <- function(input, output, session) {
    # MySQL world ----------------------------------------------------------
    dataInput <- reactive({
        if (input$my_period == "none") {
            getSymbols.MySQL(
                input$symbol,
                env = globalenv(),
                user = user,
                password = pw,
                dbname = db,
                port = 3306,
                host = host,
                auto.assign = FALSE
            )
        }##none
        else if (input$my_period == "daily") {
            to.daily(
                getSymbols.MySQL(
                    input$symbol,
                    env = globalenv(),
                    return.class = 'xts',
                    user = user,
                    password = pw,
                    dbname = db,
                    port = 3306,
                    host = host,
                    auto.assign = FALSE
                )
            )
        }#daily
        else if (input$my_period == "weekly") {
            to.weekly(
                getSymbols.MySQL(
                    input$symbol,
                    env = globalenv(),
                    return.class = 'xts',
                    user = user,
                    password = pw,
                    dbname = db,
                    port = 3306,
                    host = host,
                    auto.assign = FALSE
                )
            )
        }#weekly
        
        
        
        
    })
    
    
    #tail symbol
    my_tailored_symbol <- reactive({
        na.omit(tail(dataInput(), input$bars))
        
    })
    
    #display sample_lenght
    output$sample_lenght <- reactive({
        nrow(dataInput())
    })
    
    
    
    #updateSelectizeInput(session, 'yahoo_symbol', choices = list_of_symbols, server = TRUE)
    
    output$plot1 <- renderPlot({
        chartSeries(
            my_tailored_symbol(),
            name = input$symbol,
            subset = input$subset,
            theme = chartTheme(input$chart_teme),
            type =  input$chart_type ,
            TA = c(addVo())
        )
        if (input$studies == "ON" & input$studies_SMA == TRUE) {
            print(addSMA(
                n = input$sma_period,
                on = 1,
                col = "red"
            ))
        }
        if (input$studies == "ON" & input$studies_SMA2 == TRUE) {
            print(addSMA(
                n = input$sma_period2,
                on = 1,
                col = "blue"
            ))
        }
        if (input$studies == "ON" & input$studies_MACD == TRUE) {
            print(addMACD())
        }
        if (input$studies == "ON" &
            input$studies_BBands == TRUE) {
            print(addBBands())
        }
        if (input$studies == "ON" & input$vol_sma_period >= 2) {
            Volume_SMA_chart  	<-
                SMA(Vo(my_tailored_symbol()), n = input$vol_sma_period) / input$vol_sma_factor
            print(addTA(Volume_SMA_chart$SMA,
                        on = 2,
                        col = "blue"))
        }
    })
    
    output$HeaderText <- renderText({
        paste("Symbol:",
              input$symbol,
              "db:",
              db,
              "host:",
              host,
              "user:",
              user)
    })
    
    output$hist <- renderPlot({
        if (input$studies == "ON" & input$studies_hist == "ON") {
            hist(
                Cl(my_tailored_symbol()),
                main = input$symbol,
                col = topo.colors(16),
                xlab = "Close price"
            )
        }
        
    })
}


# Run the application
shinyApp(ui = ui, server = server)
