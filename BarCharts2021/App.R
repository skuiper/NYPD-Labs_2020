library(shiny)
library(dplyr)
library(readr)
library(plyr)
library(data.table)
library(ggplot2)
library(reshape2)
library(scales)


####### Read in the nypd dataset #######
nypd_arr <- read_csv("~/graphics2/NYPD_Bar_Charts_2021/nypd_arrestBar_Yusen.csv")
nypd <- nypd_arr

####### Custom Colors #######
customColors <- c("#a6cee3", "#1f78b4", "#b2df84", "#33a02c",
                  "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00")

####### Force Variables #######
ForceVariables <- c("Instruction" = "Instruction",
                    "HandCuff" = "HandCuff",
                    "Firearm" = "Firearm",
                    "CEW" = "CEW",
                    "PepperSpray" = "PepperSpray",
                    "Other" = "Other",
                    "Verbal" = "Verbal",
                    "Summons" = "Summons"
                    )

####### Race Variables #######
RaceVariables <- c("Asian" = "Asian",
                   "Black" = "Black",
                   "Hispanic" = "Hispanic",
                   "Native" = "Native",
                   "White" = "White",
                   "Other" = "Other")

####### Gender Variables #######
GenderVariables <- c("Female" = "Female",
                     "Male" = "Male",
                     "Unknown" = "Unknown")

####### Crime Type Variables #######
CrimeVariables <- c("Assault" = "Assault",
                    "Tresspass" = "Tresspass",
                    "Weapon" = "Weapon",
                    "Theft" = "Theft",
                    "Substance" = "Substance",
                    "Other" = "Other")

####### Categorical Variables #######
CatOptions = c("Race" = "Race",
               "Gender" = "Gender",
               "Crime Type" = "CrimeType",
               "Year" = "Year")

####### Quantitative Variables #######
QuanOptions = c("Stopped" = "Stopped",
                "Frisked" = "Frisked",
                "Searched" = "Searched",
                "Arrested" = "Arrested",
                ForceVariables)

####### User Interface #######
ui <- fluidPage(
    titlePanel("NYPD Bar Charts"),
    fluidRow(
        column(4, tabsetPanel(
            tabPanel("Axes", wellPanel(
                selectInput("Yaxis", "Y-axis Variable", choices = QuanOptions),
                conditionalPanel(condition = "input.Yaxis == 'Force'",
                                 selectizeInput("ModifyForce", 
                                                "Select the Type of Force",
                                                choices = ForceVariables,
                                                selected = ForceVariables,
                                                multiple = TRUE)),
                selectInput("Xaxis", "X-axis Variable", choices = CatOptions),
                radioButtons("YType", label = "Y-axis Measurement", inline = TRUE,
                             choices = c("Counts" = "Counts", 
                                         "Percentage Of Stops" = "Percentage",
                                         "Percentage of Arrests" = "ArrestPercent"),
                             selected = "Counts"),
                sliderInput("Year", "Choose the Years", 2005, 2019, 
                            value = c(2005, 2019), sep = "", animate = TRUE),
                
                selectInput("Facet", "Facet By", choices = c("None", CatOptions)),
                conditionalPanel(condition = "input.Yaxis != 'Force'",
                                 selectInput("Color", "Color By", 
                                             choices = c("None", CatOptions))),
                downloadButton("DownloadingData", "Download")
            )),
            
            tabPanel("Filters", wellPanel(
                selectizeInput("ModifyRace",
                               "Filter by Race",
                               choices = RaceVariables,
                               selected = RaceVariables,
                               multiple = TRUE),
                selectizeInput("ModifyGender",
                               "Filter by Gender",
                               choices = GenderVariables,
                               selected = GenderVariables,
                               multiple = TRUE),
                selectizeInput("ModifyCrime",
                               "Filter by Crime Type",
                               choices = CrimeVariables,
                               selected = CrimeVariables,
                               multiple = TRUE)
            ))
        )),
        
        column(8, plotOutput("BarChart", height = "500px"))
        
    ))

#######Server#######
server <- function(input, output){
    
    FilterNYPD <- function(dataset){
        dataset <- dataset[dataset$Year >= input$Year[1] &
                               dataset$Year <= input$Year[2], ]
        
        dataset <- dataset[dataset$Race %in% input$ModifyRace &
                               dataset$Gender %in% input$ModifyGender &
                               dataset$CrimeType %in% input$ModifyCrime, ]
        
        dataset
    }
    
    ModifyForce <- reactive({
        if(input$YType == "Counts"){
            nypd <- FilterNYPD(nypd)
            
            meltNYPD <- melt(nypd, id.vars = c("pct", CatOptions),
                             measure.vars = input$ModifyForce)
            
            currentData <- data.frame("Year" = meltNYPD$Year,
                                      "Xvar" = meltNYPD[[input$Xaxis]],
                                      "variable" = meltNYPD$variable,
                                      "value" = meltNYPD$value)
            
            if(input$Facet != "None"){
                currentData$Facet <- meltNYPD[[input$Facet]]
            }
        } else{
            if(input$YType == "ArrestPercent"){
                nypd <- nypd_arr
            }
            nypd <- FilterNYPD(nypd)
            
            currentData <- data.frame("Year" = nypd$Year,
                                      "Xvar" = nypd[[input$Xaxis]],
                                      "Instruction" = nypd$Wall,
                                      "Weapon" = nypd$Weapon,
                                      "HandCuff" = nypd$HandCuff,
                                      "Firearm" = nypd$Firearm,
                                      "CEW" = nypd$CEW,
                                      "PepperSpray" = nypd$PepperSpray,
                                      "Other" = nypd$Other)
            
            if(input$YType == "ArrestPercent"){
                currentData <- mutate(currentData, DivideBy = nypd$Arrested)
            } else if(input$YType == "Percentage"){
                currentData <- mutate(currentData, DivideBy = nypd$Stopped)
            }
            
            ddplyVariables <- c("Xvar")
            
            if(input$Facet != "None"){
                currentData$Facet <- nypd[[input$Facet]]
                ddplyVariables <- c(ddplyVariables, "Facet")
            }
            
            currentData <- ddply(currentData, ddplyVariables, summarise,
                                 DivideBy = sum(DivideBy),
                                 Weapon = sum(Weapon) / sum(DivideBy),
                                 CEW = sum(CEW) / sum(DivideBy),
                                 Instruction = sum(Instruction) / sum(DivideBy),
                                 HandCuff = sum(HandCuff) / sum(DivideBy),
                                 Firearm = sum(Firearm) / sum(DivideBy),
                                 PepperSpray = sum(PepperSpray) / sum(DivideBy),
                                 Other = sum(Other) / sum(DivideBy))
            
            currentData <- melt(currentData, id.vars = ddplyVariables,
                                measure.vars = input$ModifyForce)
        }
        
        currentData
    })
    
    prepareCountsData <- reactive({
        nypd <- FilterNYPD(nypd)
        if(input$Color != "None"){
            nypd <- arrange(nypd, nypd[[input$Color]])
        }
        
        if(!is.null(input$ModifyForce) & input$Yaxis == "Force"){
            currentData <- ModifyForce()
        } else{
            currentData <- data.frame("Year" = nypd$Year,
                                      "Xvar" = nypd[[input$Xaxis]],
                                      "Yvar" = nypd[[input$Yaxis]],
                                      "Stopped" = nypd$Stopped,
                                      "Arrested" = nypd$Arrested
                                      )
            
            if(input$Facet != "None"){
                currentData$Facet <- nypd[[input$Facet]]
            }
            
            if(input$Color != "None"){
                currentData$Color <- nypd[[input$Color]]
            }
        }
        
        currentData
    })
    
    preparePercentData <- reactive({
        if(input$YType == "ArrestPercent"){
            nypd <- nypd_arr
        }
        nypd <- FilterNYPD(nypd)
        if(input$Color != "None"){
            nypd <- arrange(nypd, nypd[[input$Color]])
        }
        
        if(!is.null(input$ModifyForce) & input$Yaxis == "Force"){
            currentData <- ModifyForce()
        } else{
            currentData <- data.frame("Year" = nypd$Year,
                                      "Xvar" = nypd[[input$Xaxis]],
                                      "Yvar" = nypd[[input$Yaxis]],
                                      "Stopped" = nypd$Stopped,
                                      "Arrested" = nypd$Arrested                                    
                                      )
            
            if(input$YType == "ArrestPercent"){
                currentData <- mutate(currentData, DivideBy = nypd$Arrested)
            } else if(input$YType == "Percentage"){
                currentData <- mutate(currentData, DivideBy = nypd$Stopped)
            }
            
            ddplyVariables <- c("Xvar")
            
            if(input$Facet != "None"){
              currentData$Facet <- nypd[[input$Facet]]
              ddplyVariables <- c(ddplyVariables, "Facet")
            }
            
            
            
            if(input$Color != "None"){
                currentData$Color <- nypd[[input$Color]]
                ddplyVariables <- c(ddplyVariables, "Color")
            }
            
            
            #currentData <- ddply(currentData, ddplyVariables, summarise,
            #                    totalYvar = sum(Yvar), DivideBy = sum(DivideBy))
            
            #Yusen's revision
            currentData <- ddply(currentData, ddplyVariables, summarise,
                                 totalYvar = sum(Yvar), DivideBy = sum(DivideBy), Stopped = sum(Stopped), Arrested = sum(Arrested))
            
            currentData$Percentage <- currentData$totalYvar / currentData$DivideBy
            
            
            #Yusen's revision
            if(input$Color != "None"){
              
              setDT(currentData)[ , sum.value := sum(Stopped), by = Xvar]
              #If Facet is non
              if(input$Facet == "None"){
                currentData <- mutate(currentData, true_perc = totalYvar/sum.value)

              }
              else{
                setDT(currentData)[ , sum.value := sum(DivideBy), by = .(Xvar,Facet)]
                currentData <- mutate(currentData, true_perc = totalYvar/sum.value)
              }
            
              currentData <- select(currentData, -c(Percentage))
              currentData <- mutate(currentData, Percentage = true_perc)
              currentData <- select(currentData, -c(true_perc))
              currentData <- mutate(currentData, DivideBy = sum.value)
              currentData <- select(currentData, -c(sum.value))
            
            
            }
            
        }
        
        currentData
    })
    
    output$BarChart <- renderPlot({
        validate(
            if(is.null(input$ModifyForce)){
                "Please select a type of force"
            },
            if(is.null(input$ModifyRace)){
                "Please select at least one race"
            },
            if(is.null(input$ModifyGender)){
                "Please select at least one gender"
            },
            if(is.null(input$ModifyCrime)){
                "Please select at least one crime type"
            }
        )
        
        if(input$YType == "Counts"){
            currentData <- prepareCountsData()
        } else{
            currentData <- preparePercentData()
        }
        
        if(!is.null(input$ModifyForce) & input$Yaxis == "Force"){
            currentPlot <- ggplot(
                data = currentData,
                aes(x = Xvar, y = value, fill = variable)) +
                geom_bar(position="stack", stat="identity") +
                scale_colour_manual(values=customColors) +
                xlab("Force")
        } else{
            if(input$YType == "Counts"){
                currentPlot <- ggplot(
                    data=currentData,
                    aes(x = Xvar, y = Yvar))
            } else if (input$YType == "Percentage")
            {
                #setDT(currentData)[ , sum.value := sum(DivideBy), by = Xvar]
                #currentData <- mutate(currentData, true_perc = totalYvar/sum.value)
                #currentData <- select(currentData, -c(Percentage))
                #currentData <- mutate(currentData, Percentage = true_perc)
                currentPlot <- ggplot(
                    data=currentData,
                    aes(x = Xvar, y = Percentage))
            }
            else if (input$YType == "ArrestPercent"){
                #setDT(currentData)[ , sum.value := sum(DivideBy), by = Xvar]
                #currentData <- mutate(currentData, true_perc = totalYvar/sum.value)
                #currentData <- select(currentData, -c(Percentage))
                #currentData <- mutate(currentData, Percentage = true_perc*0.01)
                currentPlot <- ggplot(
                    data=currentData,
                    aes(x = Xvar, y = Percentage))
            }
            
            currentPlot <- currentPlot + 
                geom_bar(stat = "identity", position="stack") +
                xlab(input$Xaxis) + scale_y_continuous(labels = comma)
            
            ##Color:
            if(input$Color != "None"){
                currentPlot <- currentPlot + aes(fill=Color) +
                    theme(legend.position="right", axis.title=element_text(size=18)) +
                    scale_colour_manual(values=customColors)
            }
        }
        
        currentPlot <- currentPlot + 
            theme(axis.title=element_text(size=18))
        
        if(input$Xaxis == "Year"){
            currentPlot <- currentPlot + scale_x_continuous(breaks = 2005:2018)
        }
        
        if(input$YType == "Counts"){
            currentPlot <- currentPlot + ylab("Counts")
        } else if(input$YType == "Percentage"){
            currentPlot <- currentPlot + ylab("Percentage of Stops")
        } else if(input$YType == "ArrestPercent"){
            currentPlot <- currentPlot + ylab("Percentage of Arrests")
        }
        
        if(input$YType == "Percentage" | input$YType == "ArrestPercent"){
            currentPlot <- currentPlot +
                scale_y_continuous(labels = percent_format())
        }
        
        # Facets:
        if(input$Facet != "None"){
            currentPlot <- currentPlot + facet_wrap(~Facet, ncol=3) +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
        }
        
        currentPlot
    })
    
    #Reactivate Dataset
    downloadFile <- reactive({
      if(input$YType == "Counts"){
        currentData <- prepareCountsData()
      } else{
        currentData <- preparePercentData()
      }
    })
    
    # Downloadable csv of selected dataset ----
    output$DownloadingData <- downloadHandler(
      
      filename = function() {
        paste("currentData", ".csv", sep = "")
      },
      content = function(file) {
        write.csv(downloadFile(), file, row.names = FALSE)
      })
}

shinyApp(ui = ui, server = server)
