function myUrl(symbol, days, date, price, c_w, c_h){ 
      var getUrl = window.location; 
      var baseUrl = getUrl .protocol + "//" + getUrl.host + "/" + getUrl.pathname.split('/')[1];  
      window.location.href=baseUrl+"?Symbol="
                            +symbol+"&Observation_Days="
                            +days+"&Buy_Date="
                            +date+"&Buy_price="
                            +price+"&chart_w="
                            +c_w+"&chart_h="
                            +c_h;
}


function ispmi(){
  myUrl("ISP.MI", 300, "2020-10-09", 1.6731, 1200, 500);
}

function gold(){
  myUrl("GOLD", 300, "2021-01-08", 24, 1200, 500);
}

function nflx(){
  myUrl("NFLX", 300, "2021-02-18", 548.22, 1200, 500);
}

function googl(){
  myUrl("GOOGL", 300, "2021-03-03", 2097.07, 1200, 500);
}

function amzn(){
  myUrl("AMZN", 300, "2021-02-22", 3092.93, 1200, 500);
}

function apple(){
  myUrl("AAPL", 300, "2021-02-15", 129.87, 1200, 500);
}

function tsla(){
  myUrl("TSLA", 300, "2021-02-15", 781.30, 1200, 500);
}


function MyCustom(){
  var s  =  document.getElementById("mysymbol").value;
  var ds = document.getElementById("mydays").value;
  var d  = document.getElementById("mydate").value;
  var p  = document.getElementById("myprice").value;
  var w  = document.getElementById("mywidth").value;
  var h  = document.getElementById("myheight").value;
  myUrl(s.trim(), ds.trim(), d.trim(), p.trim(), w.trim(), h.trim());
  
}

