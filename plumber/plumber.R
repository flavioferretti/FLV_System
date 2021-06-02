# plumber.R

#* Plot stocks chart of Symbol subsetted to Observation_Days
#* @param Symbol
#* @param Observation_Days
#* @get /stocks
#* @serializer png
function(Symbol="AAPL",Observation_Days="30"){
  
  library(quantmod)
  to_date <- as.Date(as.POSIXlt(Sys.time()))
  from_date   <- to_date-as.integer(Observation_Days)
  
  my_stocks <- na.omit(getSymbols(Symbol, src = "yahoo",
				        from = from_date,
				        to = to_date,
				        auto.assign = FALSE)	)

print( candleChart( my_stocks ) )
    				        
    		        
  
  #list(Symbol = paste0("Symbol is: '", Symbol, "'"))
  #list(Observation_Days = paste0("Observation_Days is: '", Observation_Days, "'"))
  #list(from_date = paste0("Symbol is: '", Symbol, "'","from_date is: '", from_date, "'","Observation_Days is: '", Observation_Days, "'"))
  #list(to_date = paste0("to_date is: '", to_date, "'"))
  
  #list(
	#	Symbol = paste0("Symbol is: '", Symbol, "'"), 
	#	Observation_Days = paste0("Observation_Days is: '", Observation_Days, "'"),
	#	to_date   = paste0("to_date is: '", to_date, "'"),
	#	from_date = paste0("from_date is: '", from_date, "'")
	#)
}


#* Echo the parameter that was sent in
#* @param msg The message to echo back.
#* @get /echo
function(msg=""){
  list(msg = paste0("The message is: '", msg, "'"))
}

#* Plot out data from the iris dataset
#* @param spec If provided, filter the data to only this species (e.g. 'setosa')
#* @get /plot
#* @serializer png
function(spec){
  myData <- iris
  title <- "All Species"

  # Filter if the species was specified
  if (!missing(spec)){
    title <- paste0("Only the '", spec, "' Species")
    myData <- subset(iris, Species == spec)
  }

  plot(myData$Sepal.Length, myData$Petal.Length,
       main=title, xlab="Sepal Length", ylab="Petal Length")
}

#* @get /hello
#* @serializer html
function(){
  "<html><h1>hello world</h1></html>"
}



# http://debian:8787/p/4bb09833/report?Symbol=NFLX&Observation_Days=300&Buy_Date=2021-02-18&Buy_price=548.22&chart_w=1200&chart_h=500
# http://debian:8787/p/4bb09833/report?Symbol=GOOGL&Observation_Days=300&Buy_Date=2021-03-03&Buy_price=2097.07&chart_w=1200&chart_h=500
# http://debian:8787/p/4bb09833/report?Symbol=AMZN&Observation_Days=300&Buy_Date=2021-02-22&Buy_price=3092.93&chart_w=1200&chart_h=500
# http://debian:8787/p/4bb09833/report?Symbol=AAPL&Observation_Days=300&Buy_Date=2021-02-15&Buy_price=129.87&chart_w=1200&chart_h=500
# http://debian:8787/p/4bb09833/report?Symbol=TSLA&Observation_Days=300&Buy_Date=2021-02-15&Buy_price=781.30&chart_w=1200&chart_h=500


#* My Stocks report
#* @param Symbol
#* @param Observation_Days
#* @param Buy_Date
#* @param Buy_price
#* @param chart_w 
#* @param chart_h
#* @get /report
#* @serializer html
function(Symbol="ISP.MI",Observation_Days="300",Buy_Date="2020-10-09",Buy_price="1.6731",chart_w="1200",chart_h="500"){
  
  library(quantmod)

  library(PerformanceAnalytics) # performance and risk management
  library(TTR)
  library(knitr) # formatting tables
  library(gridExtra)
  
  to_date <- as.Date(as.POSIXlt(Sys.time()))
  from_date   <- to_date-as.integer(Observation_Days)
  
  my_stocks <- na.omit(getSymbols(Symbol, src = "yahoo",
                                  from = from_date,
                                  to = to_date,
                                  auto.assign = FALSE)	)
  # subsetting into my_prices
  date_range <- paste0( Buy_Date, "::" , as.Date(as.POSIXlt(Sys.time())) )
  my_prices <- my_stocks[date_range]
  
  last_price <- as.double(tail(my_stocks[,6], n=1L))
  #my_stock_price <- as.double(head(my_prices[,1], n=1L))
  
  if(last_price <= Buy_price)my_line__color <- "red"
  if(last_price >= Buy_price)my_line__color <- "blue"
  my_buy_line__color <- "palegreen"
  
  sma.20 = SMA(my_stocks[,6], 20)   
  sma.50 = SMA(my_stocks[,6], 50) 
  #mid_stock_price <- (last_price + my_stock_price)/2
  mid_stock_price <- as.double(tail(sma.20$SMA, n=1L))
  
  png(file = "myplot.png"  , width=as.numeric(chart_w), height=as.numeric(chart_h))
  my_title = paste(colnames(my_stocks[,6]) , "last:", last_price, "buy_price:", as.numeric(Buy_price), "mid_price:", mid_stock_price)
  candleChart( my_stocks , name=my_title, TA=c(	addVo() ))
  print(addTA(my_prices[,6], on = 1, col = my_line__color ))
  print(addTA(sma.20$SMA, on = 1, col = "purple" ))
  print(addTA(sma.50$SMA, on = 1, col = "orange" ))
  print(addLines(v=nrow(my_stocks)-nrow(my_prices)+1,  col = my_buy_line__color ))
  print(addLines(h=Buy_price  , col = my_buy_line__color ))
  #print(addLines(h=mid_stock_price , col = "beige" ))
  dev.off()
  system("base64 myplot.png > myplot.txt")
  
  #### Buy & Hold SIGNAL ###
  signal_buyhold <- Lag( ifelse(  my_prices[,6] != 0  , 1,  1))
  signal_buyhold[1,1] <- as.numeric(0)
  returns_buyhold <- ROC( my_prices[,6] )*signal_buyhold
  returns_buyhold <- na.omit(returns_buyhold)
  portfolio_buyhold <- exp(cumsum(returns_buyhold))
  win_equity <- round((as.numeric(tail(portfolio_buyhold, n=1L))-1)*100 , digits = 2)
  
  ## DD table Buy&Hold
  png(file = "dd_buyhold.png",width = 600, height = 400)
  grid.table(table.Drawdowns(returns_buyhold))
  dev.off()
  system("base64 dd_buyhold.png > dd_buyhold.txt")
  
  ## Buy&Hold Risk TABLE
  png(file = "downside_buyhold.png",width = 400, height = 300)
  grid.table(table.DownsideRisk(returns_buyhold))
  dev.off()
  system("base64 downside_buyhold.png > downside_buyhold.txt")
  
  ## Performance Summary Chart Buy & Hold
  png(file = "performance_summary_buyhold.png",width=as.numeric(chart_w), height=as.numeric(chart_h))
  charts.PerformanceSummary(returns_buyhold, main = paste("Performance Summary ", "plus/minus:", win_equity,"%"), col=my_line__color)
  dev.off()
  system("base64 performance_summary_buyhold.png > performance_summary_buyhold.txt")
  #print('<img src="data:image/png;base64, ')
  
  #self_img <- readLines("myplot.txt")
  
  #cat(self_img)
  html_head <- '<html><head> <link href="http://192.168.4.1/~flavio/bootstrap/css/bootstrap.css" rel="stylesheet"></head><body>'
  html_body_img_init <- '<img src="data:image/png;base64, '
  html_body_img01 <- paste(readLines("myplot.txt"), collapse="")
  html_body_img02 <- paste(readLines("dd_buyhold.txt"), collapse="")
  html_body_img03 <- paste(readLines("downside_buyhold.txt"), collapse="")
  html_body_img04 <- paste(readLines("performance_summary_buyhold.txt"), collapse="")
  html_body_img_close <- '">  '
  html_foot <- '<script src="http://192.168.4.1/~flavio/bootstrap/js/bootstrap.min.js"></script></body></html>'
  
  
  
  
  html_nav <- paste('<script src="http://localhost/R/PlumberAPI/plumbermain.js"> </script>',
                    '<div  class="container form-control col-12 bg-white"><p class="h4"> FLV API 0.1.0</p></div>',
                 
                    '<div  class="container form-control col-12 bg-light">',
                    '  <div class="row"> ',
                    '    <div class="col-12">',
                    '      <button class="btn btn-secondary" onclick=ispmi();>ISP.MI</button>',
                    '      <button class="btn btn-secondary" onclick=apple();>AAPL</button>  ' ,
                    '      <button class="btn btn-secondary" onclick=amzn();>AMZN</button>  ' ,
                    '      <button class="btn btn-secondary" onclick=googl();>GOOGL</button>  ', 
                    '      <button class="btn btn-secondary" onclick=nflx();>NFLX</button> ' ,
                    '      <button class="btn btn-secondary" onclick=gold();>GOLD</button> ' ,
                    '      <button class="btn btn-secondary" onclick=tsla();>TSLA</button>',
                    '    </div>',
                    '  </div>',
                    '</div>',
                    
                    '<div  class="container form-control col-12 bg-light">',
                    '  <div class="row"> ',
                    '    <div class="col-3">',
                    '      <label for="mysymbol">Symbol </label> <input class="form-control" type=text id=mysymbol > ',
                    '    </div>',
                    '    <div class="col-3">',
                    '      <label for="mydays">Observation days </label> <input class="form-control" type="number" id="mydays" name="mydays" min="5" max="50000" > ',
                    '    </div>',
                    '    <div class="col-3">',
                    '      <label for="mydate">Buy date </label><input class="form-control" type="text" id="mydate" name="mydate" >',
                    '    </div>',
                    '    <div class="col-3">',
                    '      <label for="myprice">Buy price </label> <input class="form-control" type=myprice id=myprice > ',
                    '    </div>',
                    '  </div>',
                    '  <div class="row"> ',
                    '    <div class="col-3">',
                    '      <label for="mywidth">Chart width </label> <input class="form-control" type="number" id="mywidth" name="mywidth" min="200" max="5000" value="1200"> ',
                    '    </div>',
                    '    <div class="col-3">',
                    '      <label for="myheight">Chart height </label> <input class="form-control" type="number" id="myheight" name="myheight" min="200" max="5000" value="500"> ',
                    '    </div>',
                    '    <div class="col-3">',
                    '      <label for="QueryButton">Run custom query </label><br><button id="QueryButton" class="btn btn-warning " onclick=MyCustom();>Query</button> ' ,
                    '    </div>',
                    '  </div>',
                    '</div>' )
  
  paste(html_head, 
        html_nav,
        '<div  class="container form-control col-12 bg-light">',
        
        '<div class="accordion" id="accordionExample">
                      <div class="accordion-item">
                      <h2 class="accordion-header" id="headingOne">
                      <button class="accordion-button" type="button" data-bs-toggle="collapse" data-bs-target="#collapseOne" aria-expanded="true" aria-controls="collapseOne">
                      FLV Presets
                    </button>
                      </h2>
                      <div id="collapseOne" class="accordion-collapse collapse show" aria-labelledby="headingOne" data-bs-parent="#accordionExample">
                      <div class="accordion-body">
                      <div class="row">  
                        <div class="col-12"> 
                          <button class="btn btn-secondary" onclick=ispmi();>ISP.MI</button> 
                          <button class="btn btn-secondary" onclick=apple();>AAPL</button> 
                          <button class="btn btn-secondary" onclick=amzn();>AMZN</button> 
                          <button class="btn btn-secondary" onclick=googl();>GOOGL</button> 
                          <button class="btn btn-secondary" onclick=nflx();>NFLX</button> 
                          <button class="btn btn-secondary" onclick=gold();>GOLD</button> 
                          <button class="btn btn-secondary" onclick=tsla();>TSLA</button> 
                        </div>
                      </div>
                    
                      </div>
                      </div>
                      <div class="accordion-item">
                      <h2 class="accordion-header" id="headingTwo">
                      <button class="accordion-button collapsed" type="button" data-bs-toggle="collapse" data-bs-target="#collapseTwo" aria-expanded="false" aria-controls="collapseTwo">
                      Custom query
                    </button>
                      </h2>
                      <div id="collapseTwo" class="accordion-collapse collapse" aria-labelledby="headingTwo" data-bs-parent="#accordionExample">
                      <div class="accordion-body">
                      <div class="row"> 
                        <div class="col-3">
                          <label for="mysymbol">Symbol </label> <input class="form-control" type=text id=mysymbol > 
                        </div>
                        <div class="col-3">
                          <label for="mydays">Observation days </label> <input class="form-control" type="number" id="mydays" name="mydays" min="5" max="50000" > 
                        </div>
                        <div class="col-3">
                          <label for="mydate">Buy date </label><input class="form-control" type="text" id="mydate" name="mydate" >
                        </div>
                        <div class="col-3">
                          <label for="myprice">Buy price </label> <input class="form-control" type=myprice id=myprice > 
                        </div>
                      </div>
                      <div class="row"> 
                        <div class="col-3">
                          <label for="mywidth">Chart width </label> <input class="form-control" type="number" id="mywidth" name="mywidth" min="200" max="5000" value="1200"> 
                        </div>
                        <div class="col-3">
                          <label for="myheight">Chart height </label> <input class="form-control" type="number" id="myheight" name="myheight" min="200" max="5000" value="500"> 
                        </div>
                        <div class="col-3">
                          <label for="QueryButton">Run custom query </label><br><button id="QueryButton" class="btn btn-warning " onclick=MyCustom();>Query</button> 
                        </div>
                      </div>
                    </div>
                      </div>
                      </div>
                      <div class="accordion-item">
                      <h2 class="accordion-header" id="headingThree">
                      <button class="accordion-button collapsed" type="button" data-bs-toggle="collapse" data-bs-target="#collapseThree" aria-expanded="false" aria-controls="collapseThree">
                      ', "<h4>" , Symbol , " Overview [ " , win_equity ," % ]</h4><br>",'<script>document.getElementById("mysymbol").value ="',Symbol,'"</script>',
        
                    '</button>
                      </h2>
                      <div id="collapseThree" class="accordion-collapse collapse" aria-labelledby="headingThree" data-bs-parent="#accordionExample">
                      <div class="accordion-body">',
                        "<h4>" , Symbol , " Overview [ " , win_equity ," % ]</h4><br>",'<script>document.getElementById("mysymbol").value ="',Symbol,'"</script>',
                        '<script>document.getElementById("mydate").value ="',Buy_Date,'"; </script>',
                        '<script>document.getElementById("myprice").value ="',Buy_price,'"</script>',
                        '<script>document.getElementById("mydays").value =',Observation_Days,'</script>',
                        html_body_img_init, 
                        html_body_img01, 
                        html_body_img_close, 
                        "<h4>Performance Summary</h4><br>",
                        html_body_img_init, 
                        html_body_img04, 
                        html_body_img_close,
                        "<h4>Drawdowns</h4><br>",
                        html_body_img_init, 
                        html_body_img02, 
                        html_body_img_close, 
                        "<h4>Downside</h4><br>",
                        html_body_img_init, 
                        html_body_img03, 
                        html_body_img_close,
                    '</div>
                      </div>
                      </div>
                      </div>',
          
        '</div>',
        
        html_foot)
  
  
  #print(paste('<img src="data:image/png;base64, ',  paste(readLines("myplot.txt"), collapse="") ,  '">  '))
  
}


 