#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(httr)
library(jsonlite)
library(magrittr)
library(dplyr)
library(shinydashboard)
library(ggplot2)
library(scales)
library(darksky)
library(stringr)
source("functions.R")
source("global.R")
ui <- dashboardPage(title="WMATA",
  
 
  dashboardHeader(disable=FALSE
                  ),
  dashboardSidebar(collapsed = TRUE,
    sidebarMenu(
      menuItem("Weather & Transit", tabName = "dashboard", icon = icon("subway"),selected=TRUE),
      menuItem("Bus Map", tabName = "busmap", icon = icon("map")),
      menuItem("Weather Map", tabName = "weathermap", icon = icon("bolt"))
    ),
    sliderInput("maxbus","Max Buses Listed",min=1,max=6,value=3)
    
  ),
  dashboardBody(
    tags$head(
      tags$meta(name="apple-mobile-web-app-capable", content= "yes"),
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
      #tags$style(HTML('.info-box {transform: scaley(0.7);} .info-box-icon {;} .info-box-content {transform: scaley(1.3)}'))
    ),
    tabItems(
      tabItem(tabName="dashboard",
              
    fluidRow(
      
      column(width=6,
             tabBox(width=NULL,
                    tabPanel("Southbound",
                             box(width=NULL,div(tableOutput("trainssouth"),style=mytablestyle)),
                             box(width=NULL,div(tableOutput("nextbussouth"),style=mytablestyle))
                             ),
                    tabPanel("Northbound",
                             box(width=NULL,div(tableOutput("trainsnorth"),style=mytablestyle)),
                             box(width=NULL,div(tableOutput("nextbusnorth"),style=mytablestyle))
                             
                             
                             
                             )
                    
                    ),
             #box(width=NULL,div(tableOutput("trainssouth"),style=mytablestyle)),
             #box(width=NULL,div(tableOutput("nextbussouth"),style=mytablestyle)),
             infoBoxOutput(width=NULL,"alertinfo2"),
             uiOutput(width=NULL,"alertsbox2")
        
      ),
      column(width=6,
            # makeCurrentBox(now),
            uiOutput("currentbox"),
            box(title="Rain Intensity Next Hour",width=NULL,height=150,plotOutput("rainforthehour",height=130)),
            box(title="Today's Rain Chance",width=NULL,height=150,plotOutput("todaysrain",height=130)),
            box(title="Today's Temps",width=NULL,height=150,plotOutput("temps",height=130))
           
            
      )
      
      
     )
                   
         
  
    
    ),
    # tabItem(tabName="north",
    #         fluidRow(
    #           box(width=12,div(tableOutput("nextbusnorth"),style=mytablestyle))
    #         ),
    #         fluidRow(box(width=12,div(tableOutput("trainsnorth"),style=mytablestyle))),
    #         fluidRow(uiOutput("alertsbox1"),infoBoxOutput("alertinfo"))
    # 
    # ),
    
    tabItem(tabName="busmap",
            leafletOutput("busLocations",height=600)
            
    ),
    
    tabItem(tabName="weathermap",
            includeHTML("darkskyembedmap.html")
            
    )
    
    )
)
)


#################### SERVER BELOW #####################

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  myspacing <- "xs"
  
  mydelay <- 40000
  
  busicons <- awesomeIcons(
    icon = 'bus',
    iconColor = 'black',
    library = 'fa'

  )
  
  #Example Route and Bus Stop based on Dupont and the Washington Hilton as Location
  myBusRoute <- "L2"
  myLat <- 38.9131175
  myLon <- -77.0453528
  stopID1 <- "1001694"
  stopID2 <- "1001686"
  myTrainStationID <- "A03"
  
  
  now_forecast <- reactive({
    
    invalidateLater(millis = 1000 * 60 * 5)
    if (file.exists("weather.Rd")) {
      thetime <- (file.mtime("weather.Rd"))
    } else {thetime <- Sys.time() - 3600}
    
    if(thetime + .5 * 60 * 60 < Sys.time()) {
      
      now <- darksky::get_current_forecast(myLat,myLon)
      save(now,file="weather.Rd")
    } else {
      load("weather.Rd")
    }
    return(now)
    
  })
  
  output$busLocations <- renderLeaflet({
    
    invalidateLater(millis = mydelay)
    
    # get the data
  myurl <- paste("https://api.wmata.com/Bus.svc/json/jBusPositions?RouteID=",myBusRoute,"&Lat=",myLat,"&Lon=",myLon,"&Radius=5000",sep="")
    
    thebuses <- mywmata(myurl)


   # validate(need(thebuses$Lon,message="No data"))
    #draw the map
    leaflet() %>%  addTiles(options=tileOptions(detectRetina = TRUE)) %>%  setView(lng=myLon,lat=myLat,zoom=13.5) %>% 
      addAwesomeMarkers(lng=thebuses$Lon, lat=thebuses$Lat, label=paste(thebuses$DirectionText,"VehicleID",thebuses$VehicleID,sep="-"),labelOptions = list(permanent=TRUE),icon=busicons) 
   
      #
  })

  
 
  
  
  output$nextbussouth <- renderTable({
    invalidateLater(millis = mydelay)
    
    southbusurl <- paste("https://api.wmata.com/NextBusService.svc/json/jPredictions?StopID=",stopID1,sep="")
    south <- mywmata2(southbusurl)
    
    req(length(south) != 0)
    thebuss <- select(south,RouteID,VehicleID,DirectionText,Minutes)
    if (nrow(thebuss) > input$maxbus) {thebuss <- thebuss[1:input$maxbus,]}
    colnames(thebuss) <- c("Route","VehicleID","Direction","Min")
    thebuss
    },width="100%",align="lcrr",spacing=myspacing)


  
  output$nextbusnorth <- renderTable({
    invalidateLater(millis = mydelay)
    
   # north <- mywmata2("https://api.wmata.com/NextBusService.svc/json/jPredictions?StopID=1002502")
    northbusurl <- paste("https://api.wmata.com/NextBusService.svc/json/jPredictions?StopID=",stopID2,sep="")
    north <- mywmata2(northbusurl)
    req(length(north) != 0)
    #validate(need(length(north)!= 0,message="No Buses"))
    thebusn <- select(north,RouteID,VehicleID,DirectionText,Minutes)
    colnames(thebusn) <- c("Route","VehicleID","Direction","Min")
    
 #keep the bus table from getting too long.   
    if (nrow(thebusn) > input$maxbus) {thebusn <- thebusn[1:input$maxbus,]}
    
    
    thebusn
    
  },width="100%",align="lcrr",spacing=myspacing)  

  
 
  
  output$trainssouth <- renderTable({
    
    #somehow making trains and railalerts once here allows output$trainsnorth to see them
    # I really don't want to call to the API twice just to split wmata up into both directions
    invalidateLater(millis = mydelay)

    trainPredictionUrl <- paste("https://api.wmata.com/StationPrediction.svc/json/GetPrediction/",myTrainStationID,sep="")    
    trains <<- mywmata(trainPredictionUrl)
  
    railalerts <<- mywmata("https://api.wmata.com/Incidents.svc/json/Incidents")
    req(length(trains) != 0)
  
    southbound <- filter(trains,Group=="1")
    theTrainTableS <- select(southbound,Car,DestinationName,Min)
    colnames(theTrainTableS) <- c("Car","Destination","Min")
    theTrainTableS
    
  },width="100%",align="lrr",spacing=myspacing)
  
  output$trainsnorth <- renderTable({
    invalidateLater(millis = mydelay)
    
    req(length(trains) != 0)
    
    northbound <- filter(trains,Group=="2")
    theTrainTableN <- select(northbound,Car,DestinationName,Min)
    colnames(theTrainTableN) <- c("Car","Destination","Min")
    
    theTrainTableN
    
    
    
  },width="100%",align="lrr",spacing=myspacing)
  
  
  
  output$alertinfo2 <- renderInfoBox({
    invalidateLater(millis = mydelay)
    if (length(railalerts) == 0) {
      infoBox(width=NULL,"Normal",value="No Train Alerts",color="green",icon=icon("check-square"))
    } else {
    
      temp <- strsplit(railalerts$LinesAffected,";")
     # temp <- paste(unique(unlist(temp)),collapse = ",")
      temp <- strsplit(railalerts$LinesAffected,";") %>% unlist() %>% str_trim() %>% unique() %>% paste(collapse = ", ")
      
     mytext <- unique(railalerts$IncidentType) %>% paste(collapse=", ") %>% paste("Lines:",temp)
      if ("Alert" %in% railalerts$IncidentType) {
        infoBox(width=NULL,"INCIDENT",value=mytext,color="red",icon=icon("exclamation-triangle"))
      } else {
        infoBox(width=NULL,"INCIDENT",value=mytext,color="yellow",icon=icon("exclamation-triangle"))
      }
      
        }
  })
  
alertText <- reactive({
  
    invalidateLater(millis = mydelay)

    sometext <- paste("Last Updated",date())
    if (length(railalerts) == 0) {

    paste("<p>",sometext,"</p>","<p>No Alerts","</p>",sep="")
      
  } else {

    thealerts <- paste("<p>",railalerts$Description,"</p>",sep="", collapse = " ")
    paste("<p>",sometext,"</p>",thealerts,sep="")
  }

  })

  # output$alerts2 <- renderText({
  #   
  #   invalidateLater(millis = mydelay)
  #   
  #   sometext <- paste("Last Updated",date())
  #   if (length(railalerts) == 0) {
  #     
  #     paste("<p>",sometext,"</p>","<p>No Alerts","</p>",sep="")
  #   } else {
  #     
  #     paste("<p>",sometext,"</p>",railalerts$Description,"</p>",sep="")
  #     
  #   }
  #   
  # })
  
 output$alertsbox2 <- renderUI({
    
    if (length(railalerts) == 0) {
      box(width=NULL,status="success",div(HTML(alertText()),style="font-size:110%"),collapsible = TRUE, collapsed = FALSE)
    } else {
      
      if ("Alert" %in% railalerts$IncidentType) {
        box(width=NULL,title="Alert Details",height="auto",status="danger",div(HTML(alertText()),style="font-size:110%"),collapsible = TRUE, collapsed = TRUE)
      } else {
        box(width=NULL,title="Delay Details",height="auto",status="warning",div(HTML(alertText()),style="font-size:110%"),collapsible = TRUE, collapsed = TRUE)
      }
    }
  })
  
  output$todaysrain <- renderPlot({
    
    rainchance.hourly(now_forecast())
  },res=120,height=130)

  output$rainforthehour <- renderPlot({
    
    nexthour_rain(now_forecast())
  },res=120,height=130)
  
  output$temps <- renderPlot({
    
    tempplot(now_forecast())
  },res=120,height=130)  
    
  output$currentbox <- renderUI({
    
    makeCurrentBox(now_forecast())
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

