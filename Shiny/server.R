library(shiny)
library(htmltools)

# Read in artificial attainment data (FAKE DATA, DO NOT CITE!)
res <- readRDS("pcttoattainment.rds")
# Read in CBP Segments feature class
myFeatureClass<-readOGR('ADAV.gdb',layer='cbpseg2003_st_ss_92', verbose=FALSE)
# Transform to lag/long
cbp_latlng <- spTransform(myFeatureClass, CRS("+init=epsg:4326"))
# Define server logic
shinyServer(function(input, output, session) {
  # Make a reactive variable for storing currently selected segment.
  makeReactiveBinding('selectedSegment')
  
  # Use the colorbrewer "Paired" nominal color ramp
  pal <- colorFactor('Paired', res$SEGMENT)
  # Create the map using mapbox tiles and the transformed CBP feature class
  map <- leaflet(cbp_latlng) %>%
    addTiles(
      urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
      attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
    ) %>%
    setView(lng = -76, lat = 38, zoom = 8)  %>% addPolygons(
      stroke = TRUE, weight=0.5, fillOpacity = 0.7, smoothFactor = 0.5,
      color = "black", fillColor = ~pal(cbp_latlng$CBPSEG_MATCH), layerId=cbp_latlng$CBPSEG_MATCH, popup=~htmlEscape(CBPSEG_MATCH)
    )
  # Render the map.
  output$map <- renderLeaflet(map)
  
  observe({
    # Clear selected segment if user clicks off of segments.
    if (is.null(input$map_click))
      return()
    selectedSegment <<- NULL
  })
  
  observe({
    # Capture user segment click event
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      # Set the selected segment
      selectedSegment <<- event$id
      # Change segment input box to the clicked segment
      updateSelectInput(session, "e1", selected=selectedSegment)
    })
  })
  
  # Create a reactive dataset to perform input box-based filtering
  datasetInput <- reactive({
    x <- res
    # Filter based on Designated Use
    if(!is.null(input$e0)) {
      x <- subset(x, DU %in% input$e0)
    }
    # Filter based on Segment name
    if(!is.null(input$e1)) {
      x <- subset(x, SEGMENT %in% input$e1)
    }
    # Aggregate based on filter and year, and calculate the average % to attainment value
    aggregate(x, list(DU=x$DU, X3.yr.Period=x$X3.yr.Period), FUN = function(x) mean(suppressWarnings(as.numeric(as.character(x)))))
  })
  
  # Temporary outputs for testing
  #output$ex_out <- renderPrint({ input$map_shape_click$id })
  #output$table <- renderDataTable(datasetInput())
  
  output$distPlot <- renderPlot({
    # Create a new line plot with filtered data, 0-1 y-axis, and 3-yr period labels
    plot(as.numeric(datasetInput()$X3.yr.Period), datasetInput()$Value, ylim=c(0,1), type="n", xaxt = "n", xlab="3-yr Period", ylab="% to Attainment" ) 
    axis(1, labels = as.character(datasetInput()$X3.yr.Period), at=as.numeric(datasetInput()$X3.yr.Period))
    # Create a list of 4 colors for the 4 Designated Uses
    colors <- as.list(rainbow(4))
    # Assign each color to a specific DU (by name)
    names(colors) <- sort(as.character(unique(res$DU)))
    # Emtpy vector for storing just the active colors (in case all 4 are not used)
    activecolors <- vector()
    q<-0
    for (i in datasetInput()$DU) {
      # Subset the data for each Designated Use
      du <- subset(datasetInput(), DU==i)
      # Create a line for this Designated Use with the aforementioned color
      lines(du$X3.yr.Period, du$Value, type="l", lwd=1.5, col=colors[[i]])
      # Keep track of used colors
      activecolors <- c(activecolors, colors[[i]])
      q <- q + 1 
    }
    # Print legend with only active DUs/colors
    legend("bottomright", legend=unique(datasetInput()$DU), lty=rep(1,q), lwd=rep(2.5,q), col=activecolors) 
  })
})