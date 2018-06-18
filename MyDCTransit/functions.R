#### You must enter your WMATA API key here.
mykey <- "putkeytexthereoritwontdoanything"

mywmata <- function(url) {
 
  myjson <- GET(url,add_headers(api_key= mykey))
  mydata <- fromJSON(content(myjson,type="text",encoding="UTF-8"),flatten=TRUE,simplifyDataFrame = TRUE)[[1]]
   
}

mywmata2 <- function(url) {
  
  myjson <- GET(url,add_headers(api_key= mykey))
  mydata <- fromJSON(content(myjson,type="text",encoding="UTF-8"),flatten=TRUE,simplifyDataFrame = TRUE)[[2]]
  
}

rawmata <- function(url) {
  
  myjson <- GET(url,add_headers(api_key= mykey))
  mydata <- fromJSON(content(myjson,type="text",encoding="UTF-8"),flatten=TRUE,simplifyDataFrame = TRUE)
  
}

tempplot <- function(theforecast) {
  require(ggplot2)
  hourly <- data.frame(theforecast["hourly"])
  ggplot(hourly[1:18,],aes(x=hourly.time,y=hourly.temperature)) + geom_line(color="blue",size=1) + geom_point(color="blue",size=2)  + 
    theme_light() + theme(axis.text.y = element_text(face="bold", color="#993333",size=9),axis.text.x = element_text(color="blue3",size=9)) + 
    scale_x_datetime(labels = date_format("%I%p",tz =  Sys.timezone()),expand=c(0,0)) + labs(y=NULL,x=NULL)
  
}


rainchance.hourly <- function(theforecast) {
  
  hourly <- data.frame(theforecast["hourly"])
  hourly$hourly.precipProbability <- hourly$hourly.precipProbability * 100
  ggplot(hourly[1:18,],aes(x=hourly.time,y=hourly.precipProbability)) + geom_line(color="blue",size=1) + 
    theme_light() + theme(axis.text.y = element_text(face="bold", color="#993333",size=9),axis.text.x = element_text(color="blue3",size=9)) + 
    scale_x_datetime(labels = date_format("%I%p",tz =  Sys.timezone()),expand=c(0,0)) + labs(y=NULL,x=NULL) + scale_y_continuous(limits=c(0,100)) +
    geom_area(fill="blue3",alpha=.3) 
  
}

nexthour_rain <- function(theforecast) {
  
  minutely <- data.frame(theforecast["minutely"])

  ggplot(minutely,aes(x=minutely.time,y=minutely.precipIntensity)) + geom_line(color="blue",size=1) + 
    theme_light() + theme(axis.text.y = element_text(face="bold", color="#993333",size=9),axis.text.x = element_text(color="blue3",size=9)) + 
    scale_x_datetime(labels = date_format("%I%M%p",tz =  Sys.timezone()),expand=c(0,0)) + labs(y=NULL,x=NULL) +
    geom_area(fill="blue3",alpha=.3) + scale_y_continuous(limits=c(0,.35))
  
}

makeCurrentBox <- function(now) {
  require(shiny)
  current <- as.data.frame(now["currently"])
  
  myicon <- icon("clock")
  if (current$currently.cloudCover > 0.6) {
    myicon <- icon("cloud")
  
  } else if (current$currently.cloudCover < .25) {
    myicon <- icon("sun-o")} 
  
  mytext <- withTags({
    div(style="text-align: left; padding:0px",
    p(current$currently.summary),
    p(paste("Temp:",current$currently.temperature,", Dewpoint:",current$currently.dewPoint),sep="")
    )
  })
  
  infoBox(icon=myicon,width=NULL,title="Currently",value=mytext)
}

dewPointplot <- function(theforecast) {
  require(ggplot2)
  daily <- data.frame(theforecast["daily"])
  ggplot(daily,aes(x=daily.time,y=daily.dewPoint)) + geom_line(color="blue",size=1) + geom_point(color="blue",size=2)  + 
    theme_light() + theme(axis.text.y = element_text(face="bold", color="#993333",size=9),axis.text.x = element_text(color="blue3",size=9)) + 
    scale_x_datetime(labels = date_format("%a %b %d",tz =  Sys.timezone()),expand=c(0,0)) + labs(y=NULL,x=NULL) +  scale_y_continuous(limits=c(30,75))
  
}


