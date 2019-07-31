library(tidyverse)
library(stringr)
library(data.table)
library(lubridate)
library(rvest)
library(plotly)
library(leaflet)
library(readxl)
library(geojsonio)
library(rgdal)
library(ggrepel)
library(ggthemes)
library(here)
library(rsconnect)
library(shiny)
options(shiny.maxRequestSize = 30*1024^2) #allows max upload of files to be 30 MBsrunExample("09_upload")
#STAGE DATA

#set what your files will be
# SB=read_excel("YearlyData.xlsx",sheet="SB")
# KB=read_excel("YearlyData.xlsx",sheet="KB")
# JS1=read_excel("YearlyData.xlsx",sheet="JS")
# CBT=read_excel("YearlyData.xlsx",sheet="CBT")
# CBS=read_excel("YearlyData.xlsx",sheet="CBS")
# CBC=read_excel("YearlyData.xlsx",sheet="CBC")
# BB=read_excel("YearlyData.xlsx",sheet="BB")

SB <- read_csv("SB.csv")
KB <- read_csv("KB.csv")
JS1 <- read_csv("JS1.csv")
CBT <- read_csv("CBT.csv")
CBS <- read_csv("CBS.csv")
CBC <- read_csv("CBC.csv")
BB <- read_csv("BB.csv")
DB <- read_csv("DB.csv")


#map data
states.url <- "http://eric.clst.org/assets/wiki/uploads/Stuff/gz_2010_us_040_00_5m.json"
states <- geojson_read(states.url, what = "sp")
hobogps <- read_csv("hobogps.csv") #sites 

#set_here() #makes a here()directory so that it can be called to access the relative pathway for shape files
#getting watershed shape files
directory <- here::here() #first get the folder path where everything is stored. Helps to open the app directly from a file, not r studio
watersheds <- readOGR(directory,layer = "Watersheds") #then read it out
watersheds <- spTransform(watersheds, CRS("+proj=longlat +datum=WGS84 +no_defs")) #put it into right form

#use only every five minutes (~every 5th row)

SB1 <- SB[seq(1, nrow(SB), 5), ]
KB1 <- KB[seq(1, nrow(KB), 5), ]
JS11 <- JS1[seq(1, nrow(JS1), 5), ]
CBT1 <- CBT[seq(1, nrow(CBT), 5), ]
CBS1 <- CBS[seq(1, nrow(CBS), 5), ]
CBC1 <- CBC[seq(1, nrow(CBC), 5), ]
BB1 <- BB[seq(1, nrow(BB), 5), ]
DB1 <- DB[seq(1, nrow(DB), 5), ]



#one way to get columns on dataframes is to put them in a list and perform operations on each one
ldf <- list(SB1,KB1,JS11,CBT1,CBS1,CBC1,BB1,DB1)
names1 <- c("SB","KB", "JS1","CBT","CBS","CBC","BB","DB")
names(ldf) <- names1
ldf <- lapply(ldf, function(x) {
  mutate(x,
         year = as.numeric(str_sub(Date, 1, 4)), #these need to be as.numeric in order for the graphs to display
         month = as.numeric(str_sub(Date, 6,7)),
         day = as.integer(str_sub(Date, 9,10)),
         Stage = ifelse(Stage < -8,
                        NA,
                        Stage))
})

ldf <- lapply(ldf, function(x) {
  drop_na(x,
          Stage)
})

stage <- do.call(rbind.data.frame, ldf) #makes every element in list into one large datasheet

#HOURLY STAGE DATA
SB2 <- SB[seq(1, nrow(SB), 60), ] #instrad of 5 minute intervals, do hourly, to get the plot running quicker and better to interact with
KB2 <- KB[seq(1, nrow(KB), 60), ]
JS12 <- JS1[seq(1, nrow(JS1), 60), ]
CBT2 <- CBT[seq(1, nrow(CBT), 60), ]
CBS2 <- CBS[seq(1, nrow(CBS), 60), ]
CBC2 <- CBC[seq(1, nrow(CBC), 60), ]
BB2 <- BB[seq(1, nrow(BB), 60), ]
DB2 <- DB[seq(1, nrow(DB), 60), ]


ldf1 <- list(SB2,KB2,JS12,CBT2,CBS2,CBC2,BB2,DB2)
names(ldf1) <- names1
ldf1 <- lapply(ldf1, function(x) {
  mutate(x,
         year = as.numeric(str_sub(Date, 1, 4)), #these need to be as.numeric in order for the graphs to display
         month = as.numeric(str_sub(Date, 6,7)),
         day = as.integer(str_sub(Date, 9,10)),
         Stage = ifelse(Stage < -100,
                        NA,
                        Stage))
})

ldf1 <- lapply(ldf1, function(x) {
  drop_na(x,
          Stage)
})
#stage2 <- do.call(rbind.data.frame, ldf1) #makes every element in list into one large datasheet




# Define UI for application that draws a histogram
ui <- fluidPage(
  
   # Application title
   titlePanel("Acadia 2018 Stage Map"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        checkboxInput(inputId = "check1", label = strong("View Stage Data?"), value = FALSE),
        conditionalPanel(condition = "input.check1 == true",
            dateRangeInput(inputId = "date1", strong("Select Date Range"), start = "2019-01-01", end = "2019-12-31",
                           min = "2018-01-01", max = "2019-12-31"),
            actionButton(inputId = "button1", 
                         label = "Render Stage Graph"),
            fileInput(inputId = "file1",
                      label = "Upload HOBO file as csv"),
            selectInput(inputId = "dropdown2",
                        label = "Which site did you upload?",
                        choices = names1),
            checkboxInput(inputId = "check2", label = strong("Update current data sheet?"), value = FALSE),
                        conditionalPanel(condition = "input.check2 == true",
                                         actionButton(inputId = "button2",
                                                      label = "Update site"),
                                         checkboxInput(inputId = "check3", label = strong("Did you accidently update the wrong sheet?"), value = FALSE),
                                         conditionalPanel(condition = "input.check3 == true",
                                                          selectInput(inputId = "dropdown3",
                                                                      label = "Which site did you mess up?",
                                                                      choices = names1),    
                                            actionButton(inputId = "button3",
                                                          label = "clear 2019 data from that site"))))
         
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        leafletOutput(outputId = "map1"),
        plotlyOutput(outputId = "plot5"),
        plotlyOutput(outputId = "plot6"),
        plotOutput(outputId = "plot7"))
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
  output$map1 <- renderLeaflet({
    states %>%
      leaflet() %>%
      addTiles() %>%
      setView(-68.3, 44.35, 11) %>%
      addPolygons(data = watersheds,
                  popup = paste("Watershed:", watersheds@data$Name)) %>%
      addMarkers(clusterOptions = markerClusterOptions(),
                 layerId = ~hobogps$site,
                 lat = ~hobogps$Latitude,
                 lng = ~hobogps$Longitude,
                 popup = paste("Site: ", hobogps$site, "<br>"))
  })
  
  stream <- reactive(
    # req(input$date1),
    
    stage %>%
          filter(name == req(input$map1_marker_click$id), ##NEED to have req()in order to get graph to show up, otherwise there is an initial error
          Date > as.POSIXct(input$date1[1]) & Date < as.POSIXct(input$date1[2]))
    )
  
  output$plot5 <- renderPlotly({
    input$button1
    
    #isolating the stream makes it so you can click a marker but until you click "Render graph", you aren't going to change the graph
    #ggplot is a lot faster than plotly which is why it was chosen here
    isolate(stream()) %>%
      ggplot(aes(x = Date,
                 y = Stage)) +
      geom_point() +
      labs(title = paste(as.character(isolate(input$date1[1])),"to", paste(as.character(isolate(input$date1[2]))),"Stage Data - ", paste(as.character(isolate(input$map1_marker_click$id)))), #isolating the inputs so that the graph only continues to render on click of action button
           subtitle = "(if data available)",
           y = "Stage (feet)",
           x = "Date") +
      theme_economist()
    
  })
  
  allstage <- reactive(
    lapply(ldf1, function(x) {
      filter(x, Date > as.POSIXct(input$date1[1]) & Date < as.POSIXct(input$date1[2])
      )
    })
  )
  output$plot6 <- renderPlotly({
    req(input$button1)
    p <- plot_ly() 
    #   layout()
    # ?layout
    #for each dataframe in the list of data, 
    for(site in allstage()) {  p <- add_trace(p, y=site[["Stage"]], x=site[["Date"]],
                                        name = site[["name"]],
                                        type = "scatter",
                                        mode = "lines",
                                        connectgaps = FALSE)
      }
    #p <- layout(title = "All Sites - Hourly Stage", paste(as.character(isolate(input$date1[1])),"to", paste(as.character(isolate(input$date1[2]))))) #error: no applicable method for 'layout' applied to an object of class "character"
    p
  })
  
  # df <- reactive (
  #   read_csv(input$file1$datapath) #"BB7919.csv"
  #   # ifelse(names(df()())[1] == "Line#",
  #   #        df() <- df()[,-c(1)],
  #   #        df() <- df())
  # )
  
  df <- reactive({
    df <- read_csv(req(input$file1$datapath))
    #df2 <- read_csv("CBC.csv")
    #new.data <- read_csv(file.choose())
    #df <- new.data
    
    ifelse(names(df)[1] == "Line#",
           df <- df[,-c(1)],
           df <- df)
    
    
    for(i in 1:ncol(df)){
      ifelse(str_detect(names(df[i]), "Water") == TRUE,
             names(df)[i] <- "Stage",
             ifelse(str_detect(names(df[i]), "Temp") == TRUE,
                    names(df)[i] <- "Wtemp",
                    ifelse(str_detect(names(df[i]), "Baro") == TRUE,
                           names(df)[i] <- "BaroPress",
                           ifelse(str_detect(names(df[i]), "Abs") == TRUE,
                                  names(df)[i] <- "AbsPress",
                                  ifelse(str_detect(names(df[i]), "Date") == TRUE,
                                         names(df)[i] <- "Date",
                                         df <- df)))))
      
      
    }
     #taking away the seconds if a file is uploaded that has seconds
    if (str_length(df$Date) > 15) {
      df$Date <- str_sub(df$Date, 1,-4)
    } 
    
    df <- df %>%
      mutate(Date = as.POSIXct(mdy_hm(Date))) #changing to same date format as other sheets
    
    
    df <- df %>%
      mutate(name = input$dropdown2) %>% 
      filter(Stage > -8)
    df <- df[,c("Date","Stage","Wtemp","BaroPress", "AbsPress","name")]#columns in right order for binding later when updating csv file, also eliminates unnecesary columns
  })
  output$plot7 <- renderPlot({
    
    req(input$file1) #only displays if a file has been input
    
    df() %>%
      ggplot(aes(x = Date,
                 y = Stage)) +
      geom_point() +
      labs(title = req(input$dropdown2))
  }
  )
  
  #if the 2nd button is pressed, the csv file will update
  observeEvent(input$button2, {
    
    
    
    ifelse(input$dropdown2 == "SB",
           sitedata <- SB,
           ifelse(input$dropdown2 == "KB",
                  sitedata <- KB,
                  ifelse(input$dropdown2 == "JS1",
                         sitedata <- JS1,
                         ifelse(input$dropdown2 == "CBT",
                                sitedata <- CBT,
                                ifelse(input$dropdown2 == "CBS",
                                       sitedata <- CBS,
                                       ifelse(input$dropdown2 == "CBC",
                                              sitedata <- CBC,
                                              ifelse(input$dropdown2 == "BB",
                                                     sitedata <- BB,
                                                     ifelse(input$dropdown2 == "DB",
                                                            sitedata <- DB,
                                                            print("No")))))))))
    
    # sitedata <- as.data.frame(input$dropdown2)
    print(sitedata)
    # print(df())
    print(as.name(input$dropdown2))
    # print(nrow(df()))
    # print(nrow(sitedata))
    #print(nrow(CBT))
    #print(colnames(CBT))
    # print(colnames(df()))
    #Warning: Error in as.POSIXlt.character: character string is not in a standard unambiguous format. Solved by making sitedata a dataframe, not a string from the dropdown input
    
    x <- rbind(sitedata, df()) #puts the data together vertically
    
    #print(nrow(x))
    x <- x[!duplicated(x$Date), ] #deletes rows of the same date
    x <- x %>%
      arrange(Date) #arranges data by date
    # print(nrow(x))
    # print(paste0(as.character(input$dropdown2), ".csv"))
    #x <- x[,c("Date","Stage","Wtemp","BaroPress", "AbsPress","name")]
    write_csv(x, paste0(as.character(input$dropdown2),".csv"))
    
    #dialog popup saying the sheet has been updated
    showModal(modalDialog(
      title = "Sheet Updated",
      paste0("Site ", as.character(input$dropdown2), " was updated and written to ", as.character(input$dropdown2), ".csv. Please restart app to see most current data.")
      ))
  })
  
  #if button 3 is presed, all data from 2019 is erased
  observeEvent(input$button3, {
    ifelse(input$dropdown3 == "SB",
           sitedata <- SB,
           ifelse(input$dropdown3 == "KB",
                  sitedata <- KB,
                  ifelse(input$dropdown3 == "JS1",
                         sitedata <- JS1,
                         ifelse(input$dropdown3 == "CBT",
                                sitedata <- CBT,
                                ifelse(input$dropdown3 == "CBS",
                                       sitedata <- CBS,
                                       ifelse(input$dropdown3 == "CBC",
                                              sitedata <- CBC,
                                              ifelse(input$dropdown3 == "BB",
                                                     sitedata <- BB,
                                                     ifelse(input$dropdown3 == "DB",
                                                            sitedata <- DB,
                                                            print("No")))))))))
    
    
    sitedata <- sitedata[!(str_detect(sitedata$Date, "2019") == TRUE),] #delete rows with 2019 in them
    write_csv(sitedata, paste0(as.character(input$dropdown3),".csv"))
    
    
    showModal(modalDialog(
      title = "Sheet cleared",
      paste0("Site ", as.character(input$dropdown3), " was cleared of 2019 data. Please restard the app to see most current data.")
    ))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
