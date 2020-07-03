##install packages
library(maptools)
library(shinydashboard)
library(leaflet)
library(magrittr)
library(dplyr)
library(rgeos)

if(!exists("arrestData")) arrestData <- read.csv("ArrestDat.csv")
if(!exists("precincts1")) precincts1 <- readShapeSpatial("precincts1/nypp")

exp_minus_one <- function(x) { round( exp(x)-1 ) }
createTextRow <- function(label, count, total){
    sprintf("&emsp; <small><b>%s:</b> %s (%s%%)</small>",
            label, count, round(count/total *100, 2))
}

#####ui###
ui <- dashboardPage(
    dashboardHeader(title = "Stop-and-Frisk in New York City"),
    dashboardSidebar(disable = TRUE),
    dashboardBody(
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
            column(7,
                   box(title = "Map of Police Precincts", solidHeader=T, status="primary", width = '100%',
                       div(leafletOutput("nycMap", height = 450)),
                       div(htmlOutput("footer"), align = "right")
                   ),
                   
                   box(title="Map Options", status = "primary", solidHeader=T, collapsible=T, width = '100%',
                       
                       div(style = 'display: flex',
                           div(style = 'flex: 2',
                               selectInput("colorby","Color Precincts by:",choices=c(
                                   "Total number of arrests, weighted by population" = "arrests_weighted",
                                   "Total number of arrests" = "arrests_raw",
                                   "Number of arrests by race" = "race_arr",
                                   "Number of arrests by race, weighted by population" = "race_weighted",
                                   "Racial distribution of city" = "race_dist"))),
                           
                           div(style = 'flex: 1',
                               conditionalPanel(condition = "input.colorby == 'race_dist' ||
                                                                         input.colorby == 'race_arr' || input.colorby == 'race_weighted'",
                                                selectInput("race", "Race", choices = c(
                                                    "White" = "W",
                                                    "African-American" = "B",
                                                    "Hispanic" = "H",
                                                    "Asian/Pacific islander" = "A",
                                                    "Native American" = "N"))))
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
                                    selected = "Logarithmic"),
                       
                       sliderInput('year', "Year Range", min = 2005, max = 2018, value = c(2005, 2018), sep = "", animate = TRUE)
                       
                   )),
            column(5,
                   
                   box(title="Precinct Information", status = "primary", solidHeader=T, width = '100%',
                       htmlOutput("precinctOverInfo"),
                       div(plotOutput("graph_perc", width = "80%", height = 300), align = "center")
                   )
                   
            ))
        )
    )
)

server<- function(input, output, session) {
    
    
    # Selects data for applicable years
    arrestDat <- reactive({
    dat <- arrestData[arrestData$Year >= input$year[1] &
                               arrestData$Year <= input$year[2], ]
    dat2 <- select(dat, "AsPacA", "BlackA", "HispA", "NativeA", "WhiteA", "TotalA")
    dat2 <- aggregate(dat2, by = list(Precinct = dat$Precinct), FUN = sum)
    dat <- arrestData[arrestData$Year == 2010,]
    dat <- select(dat, "Precinct", "Population", "Area", "AsPac", "Black", "Hisp", "Native", "White")
    dat <- left_join(dat, dat2, by = "Precinct")
    return(dat)
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
    
    #    Define color vector, according to user input
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
        
        if( input$scale == 'Logarithmic') { log(linearValues+1)
        } else { linearValues }
        
    })
    getColor <- function(values){
        lower <- min(values)
        upper <- max(values)
        mapPalette <<- colorNumeric(rev(heat.colors(10)), c(lower, upper), 10)
        mapPalette(values)
    }
    
    #    Render base map ----
    output$nycMap <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            setView(-74.004, 40.705, zoom = 10) %>%
            setMaxBounds(-73.000, 40.200, -75.000, 41.100)
    })
    
    #    Create instance of base map where polygons can be added
    MapProxy <- leafletProxy('nycMap')
    
    #    Initialize information box to display default text
    output$precinctOverInfo <- renderText({"<center><h4>Hover over a precinct for more information.</h4></center>"})
    
    
    
    #      #    Updates "Filter Precincts" so that removed precincts do not show up in the
    #      #    select menu and crash the app - NOT WORKING PROPERLY
    #      observeEvent(input$removePrecincts, {
    #           print(allowedPrecincts())
    #
    #           updateSelectizeInput(session, 'filterPrecincts',
    #                                choices = c("Show all", arrestData$Precinct) %>%
    #                                     setdiff(input$removePrecincts))
    #      })
    #      observeEvent(input$filterPrecincts, {
    #           updateSelectizeInput(session, 'removePrecincts',
    #                                choices = c(arrestData$Precinct) %>%
    #                                     setdiff(input$filterPrecincts))
    #      })
    
    #    Prevents users from picking "Show all" with other precincts
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
    
    #    Mouseover events: highlights precinct and prints information
    observe({
        arrestDat <- arrestDat()
        
        eventOver <-input$nycMap_shape_click

        if(!is.numeric(eventOver$id)) {
            return()
        }
        
        #         Precinct information:
        precinctOver <- precincts1[precincts1$Precinct==eventOver$id,]
        precinctOverData <- arrestDat[arrestDat$Precinct==eventOver$id,]

        
        #         Highlights precinct:
        MapProxy %>%
            #                addPolygons(data=precinctOver, layerId='highlighted', color="white", fill = FALSE)
            addPolygons(data=precinctOver, layerId='highlighted', color="white", fill = FALSE)
        
        #         Prints precinct information ----
        output$precinctOverInfo <- renderText({
            
            paste(
                sprintf(
                    "<b><center><h2>Precinct %s</h2></center></b></br>
                        <b>Area:</b> %s square miles</br>
                        <table style='width:100%%'><tr><td><b>Population:</b> %s (2010 census)",
                    precinctOverData$Precinct, round(precinctOverData$Area, 2), precinctOverData$Population),
                createTextRow("White", precinctOverData$White, precinctOverData$Population),
                createTextRow("Afr.-American", precinctOverData$Black, precinctOverData$Population),
                createTextRow("Hispanic", precinctOverData$Hisp, precinctOverData$Population),
                createTextRow("Asian/Pac. Islndr", precinctOverData$AsPac, precinctOverData$Population),
                createTextRow("Native American", precinctOverData$Native, precinctOverData$Population),
                
                sprintf("<td><b>Total number of arrests:</b> %s", precinctOverData$TotalA),
                createTextRow("White", precinctOverData$WhiteA, precinctOverData$TotalA),
                createTextRow("Afr.-American", precinctOverData$BlackA, precinctOverData$TotalA),
                createTextRow("Hispanic", precinctOverData$HispA, precinctOverData$TotalA),
                createTextRow("Asian/Pac. Islndr", precinctOverData$AsPacA, precinctOverData$TotalA),
                createTextRow("Native American", precinctOverData$NativeA, precinctOverData$TotalA),
                sep="<br/>")
            
            
        })
        
        output$graph_perc <- renderPlot({
            
            precinct_pop <- as.numeric(precinctOverData[4:8])
            precinct_arr <- as.numeric(precinctOverData[9:13])
            graph_max <- max(precinct_arr/precinct_pop, na.rm=TRUE)
            
            barplot(
                    precinct_arr/precinct_pop, # Arrested in precinct/Living in precinct
                main = "Proportion of Population Arrested, by Race",
                names.arg = c("White", "Afrn Am.", "Natv Am.", "Asian", "Hispanic"),
                xlab = "Race", ylab = "Percent in Precinct")
            mtext("(Number of arrests divided by number living in precinct)", padj = -0.6)
        })
        
    })
    
    #    Mouseout events: stop displaying information
    # observeEvent(input$nycMap_shape_mouseout$id, {
    #     if( input$nycMap_shape_mouseout$id  %>% is.null %>% not) {
    #         
    #         MapProxy %>% removeShape( 'highlighted' )
    #         output$precinctOverInfo <- renderText("<center><h4>Hover over a precinct for more information.</h4></center>")
    #         output$graph_perc <- renderPlot(NULL)
    #         
    #     }
    # })
    
    #    Rerender the map whenever an input changes
    observeEvent({input$colorby; input$race; input$removePrecincts; input$scale; input$filterPrecincts}, {
        
        
        MapProxy %>%
            clearControls() %>%
            clearShapes()
        
        
        MapProxy %>% addPolygons(data = filteredPrecincts(),
                                 layerId = ~Precinct,
                                 color = getColor(countryVar()),
                                 weight = 2, fillOpacity = .6) %>%
            addLegend('bottomleft',
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
}

shinyApp(ui = ui, server = server)