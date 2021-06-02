options(shiny.port = 8080)
options(shiny.host = "0.0.0.0")

library(RMariaDB)
library(quantstrat)
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
################################################################################
# Could put these in global.R, these global variables are "hard coded"  ----------------
min_date_barrier <- "1980-01-01"
max_date_barrier <- as.character(Sys.Date())
stock_universe <- c("AAPL", "CAT", "BB")
#stock_universe <- list_of_symbols

# These variables won't change when the app launches, so hard code them too:

Sys.setenv(TZ = "UTC")
currency('USD')
stock(stock_universe, currency = "USD", multiplier = 1)

portfolio.st <- account.st <- strategy.st <- "my.first"

# In here, store the original market data which contains your full range of possible values for the market data:
# Don't keep requesting data frequently otherwise you won't be able to download the data temporarily.
if (!exists("rawdata")) {
    rawdata <- new.env()
    assign("rawdata", rawdata, envir = .GlobalEnv)
    
    lapply(stock_universe, function(sym) {
        # if (exists(sym, envir = rawdata)) {
        #     message("Have already downloaded data for ", sym)
        #     return()
        # } else {
        getSymbols(stock_universe,
                  env = rawdata,  # important to specify environment
                  from = min_date_barrier,
                  to = max_date_barrier,
                  adjust = T, auto.assign = TRUE)
        # getSymbols.MySQL(
        #     stock_universe,
        #     env = globalenv(),
        #     user = user,
        #     password = pw,
        #     dbname = db,
        #     port = 3306,
        #     host = host,
        #     auto.assign = FALSE
        # )
        #}
        return()
    })
    
}
################################################################################

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
             )),###############################################################
    ####################### BT ################################################
    tabPanel('Backtest',        
             fluidPage(
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             "stock", label = "Choose stock", choices = stock_universe
                         ),
                         
                         dateInput("start_date", "Choose start date",
                                   value = "2018-02-03"),
                         dateInput("end_date", "Choose end date",
                                   value = as.character(Sys.Date())),
                         selectInput("init_equity", "starting
    equity", choices = c(10000, 50000))
                     ),
                     
                     
                     mainPanel(
                         plotOutput("plot_backtest"),
                         verbatimTextOutput("results")
                     )
                 ))),
    tabPanel('No pagination',      DT::dataTableOutput('ex3')),
    tabPanel('No filtering',       DT::dataTableOutput('ex4')),
    tabPanel('Function callback',  DT::dataTableOutput('ex5'))
)




################################################################################
# Define server logic ##########################################################
server <- function(input, output, session) {
    # MySQL world ----------------------------------------------------------
    dataInput <- reactive({
        if (input$my_period == "none" ) {
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
    
    backtest_setup <- reactive({
        
        # need these input variables in this reactive to avoid bugs in the app when you change the time range:
        
        input$start_date
        input$end_date
        rm.strat(portfolio.st, silent = FALSE)
        initPortf(name = portfolio.st,
                  symbols = input$stock,        #------------------------ correct way to apply the "stock" input
                  initDate = "2000-01-01")
        initAcct(name = account.st,
                 portfolios = portfolio.st,
                 initDate = "2000-01-01",
                 initEq = as.numeric(input$init_equity)) # convert equity to numeric from string
        initOrders(portfolio = portfolio.st,
                   symbols = input$stock,  # ----------------------------------
                   initDate = "2000-01-01"
        )
        strategy(strategy.st, store = T)
        
        
        add.indicator(strategy = strategy.st,
                      name = "SMA",
                      arguments = list(x =
                                           quote(Cl(mktdata)),
                                       n = 10),
                      label = "nFast")
        
        add.indicator(strategy = strategy.st,
                      name = "SMA",
                      arguments = list(x =
                                           quote(Cl(mktdata)),
                                       n = 30),
                      label = "nSlow")
        
        add.signal(strategy = strategy.st,
                   name="sigCrossover",
                   arguments = list(columns = c("nFast", "nSlow"),
                                    relationship = "gte"),
                   label = "long")
        add.signal(strategy = strategy.st,
                   name="sigCrossover",
                   arguments = list(columns = c("nFast", "nSlow"),
                                    relationship = "lt"),
                   label = "short")
        add.rule(strategy = strategy.st,
                 name = "ruleSignal",
                 arguments = list(sigcol = "long",
                                  sigval = TRUE,
                                  orderqty = 100,
                                  ordertype = "stoplimit",
                                  orderside = "long",
                                  threshold = 0.0005,
                                  prefer = "High",
                                  TxnFees = -10,
                                  replace = FALSE),
                 type = "enter",
                 label = "EnterLONG")
        add.rule(strategy.st,
                 name = "ruleSignal",
                 arguments = list(sigcol = "short",
                                  sigval = TRUE,
                                  orderqty = -100,
                                  ordertype = "stoplimit",
                                  threshold = -0.005,
                                  orderside = "short",
                                  replace = FALSE,
                                  TxnFees = -10,
                                  prefer = "Low"),
                 type = "enter",
                 label = "EnterSHORT")
        add.rule(strategy.st,
                 name = "ruleSignal",
                 arguments = list(sigcol = "short",
                                  sigval = TRUE,
                                  orderside = "long",
                                  ordertype = "market",
                                  orderqty = "all",
                                  TxnFees = -10,
                                  replace = TRUE),
                 type = "exit",
                 label = "Exit2SHORT")
        add.rule(strategy.st,
                 name = "ruleSignal",
                 arguments = list(sigcol = "long",
                                  sigval = TRUE,
                                  orderside = "short",
                                  ordertype = "market",
                                  orderqty = "all",
                                  TxnFees = -10,
                                  replace = TRUE),
                 type = "exit",
                 label = "Exit2LONG")
        
    })
    
    V <- reactive({
        
        validate(need(input$start_date >= as.Date(min_date_barrier), "start date cannot be less than hard coded min_date_barrier"))
        validate(need(input$end_date <= as.Date(max_date_barrier), "end date cannot be greater than  hard coded max_date_barrier"))
        validate(need(as.Date(input$start_date) < as.Date(input$end_date), "start date must be less than end date."))
        # assign symbol market data to the global environment for the range of dates you want:
        time_rng <- paste0(input$start_date, "/", input$end_date)
        mdata <- get(input$stock, envir = rawdata)
        mdata <- mdata[time_rng]
        
        validate(need(NROW(mdata) > 0, "no data available, choose an appropriate time range"))
        
        mdata
    })
    
    backtest_results <- reactive({
        
        backtest_setup()
        mdata <- V()
        assign(input$stock, mdata, envir = .GlobalEnv)
        # not supplying mktdata as a parameter, so look in global environment for objects with the symbol names (which will exist because V assigns to .GlobalEnv):
        applyStrategy(strategy.st, portfolios = portfolio.st)
        # alternatively you could pass in the data directly to apply strategy if you're just using one symbol of data in the applyStrategy call, instead of having applyStrategy directly search in the .GlobalEnv for the symbol name
        #applyStrategy(strategy.st, portfolios = portfolio.st, mktdata = mdata)
        updatePortf(portfolio.st)
        updateAcct(account.st)
        updateEndEq(account.st)
        
    })
    
    output$plot_backtest = renderPlot({
        backtest_results()
        chart.Posn(portfolio.st, Symbol = input$stock)
    })
    
    output$results = renderPrint({
        backtest_setup()
        tmpdata <- V() # need this here so that any changes to the inputs will reprint the trade stats table
        print(tradeStats(portfolio.st))
    })

    
    }


# Run the application
shinyApp(ui = ui, server = server)
