##install packages
library(maptools)
library(shinydashboard)
library(leaflet)
library(magrittr)
library(dplyr)
library(rgeos)
library(sf)

# Load dataset  ----
path = "~/graphics2/NYPD_Map/"

arrestData <- read.csv(paste0(path, "ArrestDat.csv"), header=TRUE)
precincts1 <- sf::st_read(paste0(path, "/precincts1/nypp.shp"))
map_data = arrestData[arrestData$Year == 2017, ]

exp_minus_one <- function(x) { round( exp(x)-1 ) }
print("Finished initializing")

#--------------------------------------------------------------------------------------------------------------------#
#                                             DEFINE USER INTERFACE                                                  #
#--------------------------------------------------------------------------------------------------------------------#
# Define body of dashboard ----
body <- dashboardBody(
  tags$head(tags$style(HTML('
                         /* Sidebar font size */
                              .sidebar-menu>li>a {
                                   font-size:16px;
                              }
                         /* Box title font size */
                              .box-header .box-title, .box-header>.fa, .box-header>.glyphicon, .box-header>.ion {
                                     font-size: 20px;
                              }
                         /* Overall font size */
                              body {
                              font-size: 16px;
                              }
                              small {
                              font-size: 14px;
                              }
                         /* Table properties */
                              td {
                                  padding-left: 15px;
                                  padding-right: 15px;
                                   vertical-align: middle;
                              }
                         /* Expand and center title */
                              .main-header .logo {
                                   float:inherit;
                                   width:inherit;
                              }
                              .main-header .navbar {
                                   display: none;
                              }
                             '))),
  
  title = "NYPD Precincts", skin="blue",
  
  fluidPage(fluidRow(
    column(12,
           box(title = "Map of Police Precincts", solidHeader=T, status="primary", width = '100%',
               div(leafletOutput("nycMap", height = 450)),
               div(htmlOutput("footer"), align = "right")
           ),
           
           box(title="Map Options", status = "primary", solidHeader=T, collapsible=T, width = '100%',
               selectInput('year', 'Year', seq(2005, 2018), selectize=TRUE),
               
               div(style = 'display: flex',
                   div(style = 'flex: 2',
                       selectInput("colorby","Color Precincts by:",choices=c(
                         "Total number of arrests, weighted by precinct population" = "arrests_weighted",
                         "Total number of arrests" = "arrests_raw",
                         "Total number of arrests for each race" = "race_arr",
                         "Number of arrests by race, weighted by precinct population" = "race_weighted",
                         "Racial distribution for each precinct" = "race_dist"))),
                   
                   div(style = 'flex: 1',
                       conditionalPanel(condition = "input.colorby == 'race_dist' ||
                                                                         input.colorby == 'race_arr' || input.colorby == 'race_weighted'",
                                        selectInput("race", "Race", choices = c(
                                          "White" = "W",
                                          "Black" = "B",
                                          "Hispanic" = "H"
                                          #"Asian/Pacific islander" = "A",
                                          #"Native American" = "N"
                                        ))))
               ),
               
               div(style = 'display: flex',
                   div(style = 'flex: 1',
                       selectizeInput('removePrecincts', "Remove precincts", multiple = TRUE,
                                      choices=arrestData$Precinct, selected = c(22, 50))),
                   div(style = 'flex: 1',
                       selectizeInput('filterPrecincts', "Filter precincts", multiple = TRUE,
                                      choices = c("Show all", arrestData$Precinct),
                                      selected = "Show all",
                                      options = list(maxItems = 5)))),
               
               radioButtons('scale', "Scale", choices = c('Linear', 'Logarithmic'), inline = TRUE,
                            selected = "Logarithmic")
           ))
  )))

# UI with Header, Sidebar, and body ----
ui <- dashboardPage(dashboardHeader(title = "Stop-and-Frisk in New York City"),
                    dashboardSidebar(disable = TRUE),
                    body)
#--------------------------------------------------------------------------------------------------------------------#
#                                             DEFINE SERVER LOGIC                                                    #
#--------------------------------------------------------------------------------------------------------------------#
server <- function(input, output, session) {
  # Selects data for applicable years
  arrestDat <- reactive({
    map_data = arrestData[arrestData$Year == input$year[1], ]
    return(map_data)
  })
  
  # Determines the visible precincts based on both selectize inputs
  allowedPrecincts <- reactive({
    if(input$filterPrecincts[1] == "Show all")
      show <- precincts1$Precinct
    else
      show <- input$filterPrecincts
    hide <- input$removePrecincts
    
    return(setdiff(show, hide))
  })
  
  # Filter shapes to only show visible precincts
  filteredPrecincts <- reactive({
    precincts1[(precincts1$Precinct %in% allowedPrecincts()), ]
  })
  
  # Same as above, but for data
  filteredData <- reactive({
    arrestDat <- arrestDat()
    arrestDat[(arrestDat$Precinct %in% allowedPrecincts()), ]
  })
  
  # Prevents users from picking "Show all" with other precincts ----
  observe({
    if ( "Show all" %in% input$filterPrecincts &
         input$filterPrecincts %>% length %>% is_greater_than( 1 )) {
      if( input$filterPrecincts[1] == "Show all" ) {
        updateSelectizeInput(session, inputId = "filterPrecincts",
                             selected = input$filterPrecincts %>% setdiff( "Show all" ))
      } else {
        updateSelectizeInput(session, inputId = "filterPrecincts",
                             selected = "Show all")
      }
    }})
  
  # Define color vector, according to user input
  countryVar <- reactive({
    linearValues <- {
      if(input$colorby == "race_dist"){
        switch(as.character(input$race),
               "W" = filteredData()$White,
               "B" = filteredData()$Black,
               "H" = filteredData()$Hisp,
               "A" = filteredData()$AsPac,
               "N" = filteredData()$Native)
      } else if(input$colorby == "race_arr"){
        switch(as.character(input$race),
               "W" = filteredData()$WhiteA,
               "B" = filteredData()$BlackA,
               "H" = filteredData()$HispA,
               "A" = filteredData()$AsPacA,
               "N" = filteredData()$NativeA)
      } else if ( input$colorby == "race_weighted") {
        switch(as.character(input$race),
               "W" = filteredData()$WhiteA/filteredData()$White*1000,
               "B" = filteredData()$BlackA/filteredData()$Black*1000,
               "H" = filteredData()$HispA/filteredData()$Hisp*1000,
               "A" = filteredData()$AsPacA/filteredData()$AsPac*1000,
               "N" = filteredData()$NativeA/filteredData()$Native*1000,
        )
      } else {
        switch(as.character(input$colorby),
               "arrests_weighted" = filteredData()$TotalA/filteredData()$Population*1000,
               "arrests_raw" = filteredData()$TotalA)}
    }
    
    if( input$scale == 'Logarithmic') {
      log(linearValues+1)
    } else { linearValues }
    
  })
  
  updateColor <- reactive({
    values <- countryVar()
    lower <- min(values)
    upper <- max(values)
    return (leaflet::colorNumeric(rev(heat.colors(10)), c(lower, upper), 10))
  })
  
  # Render base map ----
  output$nycMap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(-74.004, 40.705, zoom = 10) %>%
      setMaxBounds(-73.000, 40.200, -75.000, 41.100)
  })
  
  # Create instance of base map where polygons can be added
  MapProxy <- leafletProxy('nycMap')
  
  # Re-render the map whenever an input changes ----
  observeEvent({input$year; input$colorby; input$race; input$removePrecincts; input$scale; input$filterPrecincts}, {
    mapPalette = updateColor()
    MapProxy %>% clearControls() %>% clearShapes()  %>% clearPopups()
    
    MapProxy %>% addPolygons(data = filteredPrecincts(),
                             layerId = ~Precinct,
                             color = mapPalette(countryVar()),
                             weight = 2, fillOpacity = .6
                             #label = filteredPrecincts()$Precinct,
                             #labelOptions = labelOptions(
                             #  style = list("font-weight" = "normal", padding = "3px 8px",
                             #                textsize = "15px", direction = "auto"))
    )
    MapProxy %>% addLegend('bottomleft',
                           title = ifelse(input$colorby %in% c("arrests_weighted", "race_weighted"),
                                          "Arrests per 1000 people",
                                          ifelse( input$colorby %in% c("race_dist") ,
                                                  "Population", "Arrests")),
                           pal = mapPalette, values = countryVar(), opacity = 0.7,
                           labFormat = labelFormat(transform = ifelse(input$scale == 'Logarithmic',
                                                                      exp_minus_one ,
                                                                      identity)))
    if ( input$filterPrecincts[1]  != "Show all" ) {
      MapProxy %>% addPolygons(data = filteredPrecincts(),
                               color = 'red', weight = 2,
                               fill = FALSE, opacity = 1)
    }
  })
  
  #Show pop-up on click event 
  observeEvent(input$nycMap_shape_click, {
    click <- input$nycMap_shape_click
    # Precinct information:
    precinctOver <- precincts1[precincts1$Precinct==click$id,]
    precinctOverData <- arrestDat()[arrestDat()$Precinct==click$id,]
    
    text <- sprintf(
      "<b><center><h2>Precinct %s</h2></center></b></br>
       <b>Area:</b> %s square miles</br>
       <table style='width:100%%'><tr>
        <td><b>Population:</b> %s (2010 census)</td>
        <td><b>Total number of arrests:</b> %s </td>
        <tr>
        <td><b>White people:</b> %s ( %s %%) </td>
        <td><b>White people:</b> %s ( %s %%) </td>
        <tr>
        <td><b>Black people:</b> %s ( %s %%) </td>
        <td><b>Black people:</b> %s ( %s %%) </td>
        <tr>
        <td><b>Hispanic people:</b> %s ( %s %%) </td>
        <td><b>Hispanic people:</b> %s ( %s %%) </td>",
      precinctOverData$Precinct, round(precinctOverData$Area, 2), 
      precinctOverData$Population, precinctOverData$TotalA,
      precinctOverData$White, round(precinctOverData$White/precinctOverData$Population*100,2),
      precinctOverData$WhiteA, round(precinctOverData$WhiteA/precinctOverData$TotalA*100,2),
      precinctOverData$Black, round(precinctOverData$Black/precinctOverData$Population*100,2),
      precinctOverData$BlackA, round(precinctOverData$BlackA/precinctOverData$TotalA*100,2),
      precinctOverData$Hisp, round(precinctOverData$Hisp/precinctOverData$Population*100,2),
      precinctOverData$HispA, round(precinctOverData$HispA/precinctOverData$TotalA*100,2)
    )
    
    # Highlights precinct:
    MapProxy %>%
      addPolygons(data=precinctOver, layerId='highlighted', color="white", fill = FALSE)
    MapProxy %>% clearPopups() %>%
      addPopups(click$lng, click$lat, text)
  })
}


# Run the app ----
shinyApp(ui = ui, server = server)