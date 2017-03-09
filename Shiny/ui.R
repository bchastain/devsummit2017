library(shiny)
library(leaflet)
library(RColorBrewer)
library(rgdal)
# Read in artificial attainment data (FAKE DATA, DO NOT CITE!)
res <- readRDS("pcttoattainment.rds")

# Define UI for application that draws a histogram
shinyUI(fluidPage(
   titlePanel("Chesapeake Bay Segments"),
   div(class="outer",
       
       tags$head(
         # Include our custom CSS
         includeCSS("styles.css")
       ),
       
       # Full-page leaflet map
       leafletOutput("map", width="100%", height="100%"),
       
       # Moveable modal for input boxes and chart
       absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                     draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                     width = 500, height = "auto",
                     
                     h3("% to Attainment by DU"),
                     
                     selectInput('e0', 'Designated Use', sort(as.character(unique(res$DU))), multiple=TRUE, selectize=TRUE),
                     selectInput('e1', 'Segment', sort(as.character(unique(res$SEGMENT))), multiple=TRUE, selectize=TRUE),
                     
                     #       verbatimTextOutput('ex_out'),
                     #       dataTableOutput('table'),
                     plotOutput("distPlot")
       )

   )   

))