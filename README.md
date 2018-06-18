# DC Weather and Metro Dashboard

This is an R Shiny app using shinydashboard that displays location-specific bus, metro, and weather information. I created it to run on a Raspberry Pi or other private server and then use a tablet or other web browser to run this constantly for example prior to leaving for wok.

## Setup:

Latitude, longitude, bus and metro stops need to be set up in the app.R server section that defines those variables. By default it uses metro stops and the red line near the Washington Hilton in Dupont Circle.

You'll also need [a Wmata API key](https://developer.wmata.com) in functions.R and to set up the proper environmental variable for the Dark Sky API as described [in its documentation](https://github.com/hrbrmstr/darksky). If deploying you'll need to set up this variable on your shiny server too.

The weather map simply embeds from the darksky web site. You can edit `darkskyembedmap.html` with a [map embed from Darsky as you like](https://darksky.net/widgets).

## Requirements / Dependencies

The shiny app requires a variety of dependencies including leaflet, shinydashboard, and the aforementioned darksky interface.

