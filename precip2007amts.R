library(readxl)
library(tidyverse)
library(stringr)
library(data.table)
library(lubridate)
library(rvest)

##data cleaning and graphs for precip data from 1982 - 2007
#monthly precip
data <- read_excel(file.choose())
clean <- data[4:nrow(data),c(1,33,34)]
colnames(clean)[2] <- "Precip (inches)"
colnames(clean)[1] <- "month.year"
#if there's a digit in the months column, it's the year, so bring it to this new column as a digit
clean <- clean %>%
  mutate(year = ifelse(str_detect(clean$month.year,
                                  "[:digit:]"),
                       as.numeric(str_sub(month.year, -3,-2)),
                       NA)) 
#make the dates actual dates
clean <- clean %>%
  mutate(year1 = ifelse(year > 8,
                        year + 1900,
                        year + 2000)) %>%
  fill(year1)
View(clean)

clean <- clean %>%
  drop_na("month.year") #drops rows with NA values in month.year column
clean <- clean %>%
  mutate(precip.inches = ifelse(str_detect(clean$`Precip (inches)`,
                                           "[:alpha:]"),
                                NA,
                                clean$`Precip (inches)`)) %>%
  drop_na("precip.inches") 



clean$precip.inches <- as.numeric(clean$precip.inches)
clean <- clean[,c(1,6,5)] #get only relevant columns
y <- clean %>%
  group_by(clean$year1) %>%
  summarize(total.rainfall = sum(precip.inches))

colnames(y)[1] <- "year"
y %>%
  ggplot(aes(x = year,
             y = total.rainfall)) +
  geom_point()
lapply(clean, class)



## daily precip
##from 1982 - 2007

#data wrangling
data <- read_excel(file.choose())
daily.precip <- data[4:nrow(data),-c(33,34,35,36,37)]

colnames(daily.precip)[1] <- "month.year"
daily.precip <- daily.precip %>%
  drop_na("month.year")

colnames(daily.precip)[2,] <- c(1:31)


#if there's a digit in the months column, it's the year, so bring it to this new column as a digit
daily.precip <- daily.precip %>%
  mutate(year = ifelse(str_detect(daily.precip$month.year,
                                  "[:digit:]"),
                       as.numeric(str_sub(month.year, -3,-2)),
                       NA)) %>%
  mutate(year1 = ifelse(year > 8,
                        year + 1900,
                        year + 2000)) %>%
  fill(year1) %>% #fills in empty cells, like the excel dropdown function
  mutate(new.date = paste(month.year, year1)) %>%   #joins together the year and months
  mutate(olddate = ifelse(str_detect(month.year,
                                           "[:digit:]"),
                                NA,
                                month.year)) %>%
  drop_na("olddate") #will take out rows that say month and year in month.year column

daily.precip1 <- daily.precip[,-c(1,33,34,36)]

#changing 
dailygathered <- daily.precip1 %>%
  gather(key ="day",
         value = "precipitation",
         -new.date)  

#getting dates right
dailygathered <- dailygathered %>% 
  mutate(wrong.day = as.numeric(str_sub(day, 4,))) %>% #extracts just day number
  mutate(real.day1 = wrong.day -1) %>% #turns it into the real day because it started at 2
  mutate(new.date1 = paste(new.date, real.day1)) %>%  #joins together the day and date
  mutate(new.date2 = myd(new.date1)) #lubridate turns the format into somethign readable

dailygathered1 <- dailygathered[,c("new.date2","precipitation")] #only relevant columns
dailygathered2 <- dailygathered1 %>%
  mutate(precip1 = ifelse(is.na(precipitation),
                          0,
                          precipitation))#changing NA's to 0
dailygathered2 <- dailygathered2[,c("new.date2","precip1")] #only relevant columns
dailygathered2 %>%
  write.csv("82_07dailyPrecipdata.csv")


##graphs
library(lubridate)
library(plotly)
precip <- read.csv(file.choose()) #daily precip for every day since 2007

#precip$Date <- as.Date(precip$Date)
?year
daily.precip1 <- precip %>%
  mutate(newdate2 = mdy(Date))

daily.precip1 <- daily.precip1 %>%
  mutate(year = as.numeric(str_sub(newdate2, 1, 4)),
         month = as.numeric(str_sub(newdate2, 6,7)),
         day = as.numeric(str_sub(newdate2, -2,-1))) %>%
  mutate(cmonth = month.name[month]) #column with month names
         
         
x <- daily.precip1 %>%
  filter(year == 2009) %>%
  filter(cmonth == "June")
  
#monthly data  
x %>%
  ggplot(aes(x = day,
             y = precip)) +
  geom_point(color = "darkorchid4") +
  labs(title = paste("Jan","1999", "Precipitation - Acadia NP"),
       subtitle = "From Macfarland Station",
       y = "Daily precipitation (inches)",
       x = "Day") +
  theme_bw(base_size = 15)
#lapply(precip, class)
#making table

daily %>%
  group_by(cmonth) %>%
  summarize(monthl.precip = sum(precip))


x<- precip.data %>%
  filter(year == 2007) %>%
  group_by(cmonth) %>%
  summarize(yearly.precip = sum(precip))
colnames(x) <- c("Month", "Precipitation (In)")

#hourly table
daily.table <- precip.data %>%
  filter(year == 2010,
         cmonth == "May") %>% #
  group_by(day) %>%
  summarize(daily.precip = sum(precip))
colnames(daily.table) <- c("Day", "Precipitation (inches)")


#yearly data
years <- daily.precip1 %>%
  group_by(year) %>%
  summarize(total.precip = sum(precip))
years %>%
  ggplot(aes(x = year,
             y = total.precip)) +
  geom_bar(aes(fill = total.precip),
           stat = "identity") + 
  labs(title = "Total Yearly Precipitation - Acadia NP",
       subtitle = "From Macfarland Station",
       y = "Total Precipitation (inches)",
       x = "Year") 
  
scale_color_manual(values = c("red",
                                "green",
                                "blue"))
?geom_bar
p <- precip %>%
  ggplot(aes(x = Date,
         y = precip)) +
  geom_point(color = "darkorchid4") +
  labs(title = "Precipitation - Acadia NP",
       subtitle = "From Macfarland Station",
       y = "Daily precipitation (inches)",
       x = "Date") +
       theme_bw(base_size = 15)

?rbind


# Create a shareable link to your chart
# Set up API credentials: https://plot.ly/r/getting-started


#map
library(leaflet)
library(rvest)
library(geojsonio)
library(ggrepel)
#counties.url <- "http://eric.clst.org/assets/wiki/uploads/Stuff/gz_2010_us_050_00_500k.json"
#counties <- geojson_read(counties.url, what = "sp")
states.url <- "http://eric.clst.org/assets/wiki/uploads/Stuff/gz_2010_us_040_00_5m.json"

states <- geojson_read(states.url, what = "sp")
hobogps <- read_csv("hobogps.csv")


#bins <- c(0, 7, 14, 21, 28, Inf)
colors <- colorBin(palette = "YlOrRd", domain = c(0, 100))




library(rgdal)
?readOGR
#add<-shapefile("Watersheds.shp")
watersheds <- readOGR("C:/Users/sfigr/Documents/Friends of Acadia/Map",layer = "Watersheds")
watersheds <- spTransform(shapeData, CRS("+proj=longlat +datum=WGS84 +no_defs"))

states %>%
  leaflet() %>%
  addTiles() %>%
  setView(-68.3, 44.35, 11) %>%
  addPolygons(data = watersheds,
              fillOpacity = 0.3,
              popup = paste("Watershed:", watersheds@data$Name)) %>%
  addMarkers(clusterOptions = markerClusterOptions(),
             lat = ~hobogps$Latitude,
             lng = ~hobogps$Longitude,
             popup = paste("Site: ", hobogps$site, "<br>"))

#next: take the yearly data and when it's clicked on the map, have a graph pop up of stage data
#year is based off the slider

clean$precip.inches <- as.numeric(clean$precip.inches)
clean <- clean[,c(1,6,5)] #get only relevant columns


#getting up tp date information
#webscraping
library(rvest)
#reading in the data

precip.url <- "http://nadp.slh.wisc.edu/siteOps/ppt/Data.aspx?id=ME98&stdate=01/01/2019T00:00&endate=01/01/2020T00:00&plot_sel=1111110&data_sel1=H&data_sel2=110&sel2_count=2&offset_txt=-5"
precip2019.data <- precip.url %>%
  read_html() %>%
  html_nodes("table") %>%   #looking for tables here
  .[[2]] %>% #get the third table down since that's the one we want
  html_table()

precip2019.data1 <- precip2019.data[,c(1,2,3)] #get only relevant columns
#changint to datetime format
precip2019.data1 <- precip2019.data1 %>%
  mutate(hour1 = str_sub(Hour, -2)) %>%
  mutate(new.date = paste(Date, hour1)) %>%
  mutate(DateTime = ymd_h(new.date))

#takes off new date
precip2019.data1 <- precip2019.data1 %>%
  select(-new.date)
#sums up data to daily precip values
dailyprecip2019 <- precip2019.data1 %>%
  group_by(Date) %>%
  summarize(precip = sum(`Precip (in)`))

dailyprecip2019 <- dailyprecip2019[c(-nrow(dailyprecip2019)),] #deletes alst row of the data frame, the totals row, which is redundant

#gets precip date to a date format
x <- precip %>%
  mutate(new.date2 = mdy(Date))
x <- x[,-c(1)]
x<- x[,c(2,1)]
colnames(dailyprecip2019)[1] <- "new.date2"
#putting together up to date data with previous csv
total <- rbind(x, dailyprecip2019) #puts the data together vertically
total <- total[!duplicated(total$new.date2), ] #deletes rows of the same date


?rbind
colnames(x)[2] <- "Hour Interval"


precipdata1 <- precip.data %>%
  filter(year == 2018,
         cmonth == "January")

  

x <- precipdata1 %>%
  group_by(Date) %>%
  summarize(precip = sum(precip)) %>%
  ggplot(aes(x = Date, 
               y = precip)) +
  geom_bar(stat = "identity") +
  labs(x = "Date")

ggplotly(x)

##stage data
library(readxl)

#SB16=read_excel("SB.xlsx",sheet="SB16")
#SB17=read_excel("SB.xlsx",sheet="SB17")
#JS16=read_excel("JS.xlsx",sheet="JS16")
#JS17=read_excel("JS.xlsx",sheet="JS17")
#SBPrecip16=read_excel("SB.xlsx",sheet="Precip16")
#SBPrecip17=read_excel("SB.xlsx",sheet="Precip17")




SB=read_excel("YearlyData.xlsx",sheet="SB")
KB=read_excel("YearlyData.xlsx",sheet="KB")
JS1=read_excel("YearlyData.xlsx",sheet="JS")
CBT=read_excel("YearlyData.xlsx",sheet="CBT")
CBS=read_excel("YearlyData.xlsx",sheet="CBS")
CBC=read_excel("YearlyData.xlsx",sheet="CBC")
BB=read_excel("YearlyData.xlsx",sheet="BB")
#ldf <- list(SB,KB,JS1,CBT,CBS,CBC,BB)
#one way to break the data set smaller int o 5 mintue breaks
x <- SB %>%
  group_by(Date=cut(Date,breaks="5 min"))%>%
  summarize(Stage=mean(Stage),
            Wtemp = mean(Wtemp),
            MinATemp = min(MinATemp),
            MaxATemp = max(MaxATemp),
            BaroPress = mean(BaroPress),
            AbsPress = mean(AbsPress))


#another way (~every 5th row), might not get good readings on other data though
SB <- SB[seq(1, nrow(SB), 5), ]
KB <- KB[seq(1, nrow(SB), 5), ]
JS1 <- KB[seq(1, nrow(SB), 5), ]
CBT <- CBT[seq(1, nrow(SB), 5), ]

CBS <- CBS[seq(1, nrow(SB), 5), ]
CBC <- CBC[seq(1, nrow(SB), 5), ]
BB <- BB[seq(1, nrow(SB), 5), ]

#one way to get the right columns on the dataframes is to do it maually with each
SB <- SB %>%
  mutate(Stage = ifelse(Stage < 0,
                        NA,
                        Stage)) %>%
  drop_na(Stage) %>%
  mutate(name = "SB",
         year = as.numeric(str_sub(Date, 1, 4)), #these need to be as.numeric in order for the graphs to display
         month = as.numeric(str_sub(Date, 6,7)),
         day = as.integer(str_sub(Date, 9,10)))

KB <- KB %>%
  mutate(Stage = ifelse(Stage < 0,
                        NA,
                        Stage)) %>%
  drop_na(Stage) %>%
  mutate(name = "KB",
         year = as.numeric(str_sub(Date, 1, 4)), #these need to be as.numeric in order for the graphs to display
         month = as.numeric(str_sub(Date, 6,7)),
         day = as.integer(str_sub(Date, 9,10)))

JS1 <- JS1 %>%
  mutate(Stage = ifelse(Stage < 0,
                        NA,
                        Stage)) %>%
  drop_na(Stage) %>%
  mutate(name = "JS1",
         year = as.numeric(str_sub(Date, 1, 4)), #these need to be as.numeric in order for the graphs to display
         month = as.numeric(str_sub(Date, 6,7)),
         day = as.integer(str_sub(Date, 9,10)))
CBT <- CBT %>%
  mutate(Stage = ifelse(Stage < 0,
                        NA,
                        Stage)) %>%
  drop_na(Stage) %>%
  mutate(name = "CBT",
         year = as.numeric(str_sub(Date, 1, 4)), #these need to be as.numeric in order for the graphs to display
         month = as.numeric(str_sub(Date, 6,7)),
         day = as.integer(str_sub(Date, 9,10)))

CBS <- CBS %>%
  mutate(Stage = ifelse(Stage < 0,
                        NA,
                        Stage)) %>%
  drop_na(Stage) %>%
  mutate(name = "CBS",
         year = as.numeric(str_sub(Date, 1, 4)), #these need to be as.numeric in order for the graphs to display
         month = as.numeric(str_sub(Date, 6,7)),
         day = as.integer(str_sub(Date, 9,10)))

CBC <- CBC %>%
  mutate(Stage = ifelse(Stage < 0,
                        NA,
                        Stage)) %>%
  drop_na(Stage) %>%
  mutate(name = "CBC",
         year = as.numeric(str_sub(Date, 1, 4)), #these need to be as.numeric in order for the graphs to display
         month = as.numeric(str_sub(Date, 6,7)),
         day = as.integer(str_sub(Date, 9,10)))

BB <- BB %>%
  mutate(Stage = ifelse(Stage < 0,
                        NA,
                        Stage)) %>%
  drop_na(Stage) %>%
  mutate(name = "BB",
         year = as.numeric(str_sub(Date, 1, 4)), #these need to be as.numeric in order for the graphs to display
         month = as.numeric(str_sub(Date, 6,7)),
         day = as.integer(str_sub(Date, 9,10)))

#another way is to make a list and put it into a function
#then you can subset this with ldf$SB or ldf$"SB"
#ldf[["SB"]]

ldf <- list(SB,KB,JS1,CBT,CBS,CBC,BB)
ldf <- lapply(ldf, function(x) {
  mutate(x,
         year = as.numeric(str_sub(Date, 1, 4)), #these need to be as.numeric in order for the graphs to display
         month = as.numeric(str_sub(Date, 6,7)),
         day = as.integer(str_sub(Date, 9,10)),
         Stage = ifelse(Stage < 0,
                        NA,
                        Stage))
})

names1 <- c("SB","KB", "JS1","CBT","CBS","CBC","BB")
names(ldf) <- names1

#gets rid of weird negative values
ldf <- lapply(ldf, function(x) {
  drop_na(x,
          Stage)
})



ldf1$SB

##HOURly intervals not 5 mintutes

SB <- read_csv("SB.csv")
KB <- read_csv("KB.csv")
JS1 <- read_csv("JS1.csv")
CBT <- read_csv("CBT.csv")
CBS <- read_csv("CBS.csv")
CBC <- read_csv("CBC.csv")
BB <- read_csv("BB.csv")
DB <- read_csv("DB.csv")
SB1 <- SB[seq(1, nrow(SB), 60), ]


KB1 <- KB[seq(1, nrow(KB), 60), ]


JS11 <- JS1[seq(1, nrow(JS1), 60), ]


CBT1 <- CBT[seq(1, nrow(CBT), 60), ]


CBS1 <- CBS[seq(1, nrow(CBS), 60), ]


CBC1 <- CBC[seq(1, nrow(CBC), 60), ]


BB1 <- BB[seq(1, nrow(BB), 60), ]
DB1 <- DB[seq(1, nrow(DB), 60), ]



ldf1 <- list(SB1,KB1,JS11,CBT1,CBS1,CBC1,BB1,DB1)
names(ldf1) <- names1

ldf1 <- lapply(ldf1, function(x) {
  mutate(x,
         year = as.numeric(str_sub(Date, 1, 4)), #these need to be as.numeric in order for the graphs to display
         month = as.numeric(str_sub(Date, 6,7)),
         day = as.integer(str_sub(Date, 9,10)),
         Stage = ifelse(Stage < 0,
                        NA,
                        Stage))
})

ldf1 <- lapply(ldf1, function(x) {
  drop_na(x,
          Stage)
})
lapply(ldf1, function(x) {
  filter(x, Date > as.POSIXct("2019-04-30 05:00:00 UTC") & Date < as.POSIXct("2019-09-20 08:52:00 UTC")
  )
})
###one graph with overlays of each stream
# color_map=c(SB="darkturquoise",KB="black",JS="yellow3",
#             BB="red",CBC="purple",CBS="green3",CBT="orange")
p <- plot_ly()
for(site in ldf1) {  p <- add_trace(p, y=site[["Stage"]], x=site[["Date"]],
                                    name = site[["name"]],
                                    type = "scatter",
                                    mode = "lines",
                                    connectgaps = FALSE
                                    )
}
p



?add_trace
###making all csv files have name and right columns
SB <- read_csv("SB.csv")
KB <- read_csv("KB.csv")
JS1 <- read_csv("JS1.csv")
CBT <- read_csv("CBT.csv")
CBS <- read_csv("CBS.csv")
CBC <- read_csv("CBC.csv")
BB <- read_csv("BB.csv")
DB <- read_csv("DB.csv")
DB <- read.csv(file.choose())

#getting correct number of columns
SB <- SB[,-c(3,5,6)]
KB <- KB[,-c(3,5,6)]
JS1 <- JS1[,-c(3,5,6)]
CBT <- CBT[,-c(3,5,6)]
CBS <- CBS[,-c(3,5,6)]
CBC <- CBC[,-c(3,5,6)]
BB <- BB[,-c(3,5,6)]

#mutating name
SB <- SB %>%
  mutate(name = "SB")
KB <- KB %>%
  mutate(name = "KB")
JS1 <- JS1 %>%
  mutate(name = "JS1")
CBT <- CBT %>%
  mutate(name = "CBT")
CBS <- CBS %>%
  mutate(name = "CBS")
CBC <- CBC %>%
  mutate(name = "CBC")
BB <- BB %>%
  mutate(name = "BB")

SB <- write_csv(SB, "SB.csv")
KB <- write_csv(KB, "KB.csv")
JS1 <- write_csv(JS1, "JS1.csv")
CBT <- write_csv(CBT, "CBT.csv")
CBS <- write_csv(CBS, "CBS.csv")
CBC <- write_csv(CBC, "CBC.csv")
BB <- write_csv(BB, "BB.csv")



###UPDATING CSV FILES



SB <- read_csv("SB.csv")
KB <- read_csv("KB.csv")
JS1 <- read_csv("JS1.csv")
CBT <- read_csv("CBT.csv")
CBS <- read_csv("CBS.csv")
CBC <- read_csv("CBC.csv")
BB <- read_csv("BB.csv")


#this part waas for when I accidently uploaded the wrong file for stage and values got assigned to the wrong sites. 
#still very useful if you accidently set the wrong data. 
SB <- SB[!(str_detect(SB$Date, "2019") == TRUE),] #delete rows with 2019 in them
BB <- BB[!(str_detect(BB$Date, "2019") == TRUE),] #delete rows with 2019 in them
JS1 <- JS1[!(str_detect(JS1$Date, "2019") == TRUE),] #delete rows with 2019 in them
CBT <- CBT[!(str_detect(CBT$Date, "2019") == TRUE),] #delete rows with 2019 in them
write_csv(SB, "SB.csv")
write_csv(BB, "BB.csv")
write_csv(JS1, "JS1.csv")
write_csv(CBT, "CBT.csv")

DB <- read_csv(file.choose())
DB1 <- DB
#names(BB)[1] == "Line#"
#ifelse statement if first column is line number
# ifelse(names(DB1)[1] == "Line#",
#        DB1 <- DB1[,-c(1)],
#        DB1 <- DB1)


# names(x)[2] <- "Stage"
# str_detect(names(DB1[7]), "Water level")
#all(is.na(DB1[2]))
#sapply(DB1, function(x)all(is.na(x)))
#this for loop is needed because not all the hobo sheets uplaoded have the same columns in the same order
# remcols <- NULL
# for(i in 1:ncol(DB1)) {
#   ifelse(all(is.na(DB1[i])) == TRUE,
#          remcols[i] <- i,
#          ifelse(str_detect(DB1[i], "TRUE") == TRUE,
#                 remcols[i] <- i,
#                 DB1 <- DB1))
# }
# 
# remcols
# remcols <- remcols[!is.na(remcols)]
# DB1 <- DB1[,-c(remcols)] #takes off all the columns 
#this for loop is needed because not all the hobo sheets uplaoded have the same columns in the same order
# ifelse(str_detect(names(DB1[2]),"Diff") == TRUE,
#        DB1 <- DB1[,-c(2)],
#        DB1 <- DB1)

for(i in 1:ncol(DB1)){
  ifelse(str_detect(names(DB1[i]), "Water Level") == TRUE,
         names(DB1)[i] <- "Stage",
         ifelse(str_detect(names(DB1[i]), "Temperature") == TRUE,
                names(DB1)[i] <- "Wtemp",
                ifelse(str_detect(names(DB1[i]), "Barometric Pressure") == TRUE,
                       names(DB1)[i] <- "BaroPress",
                       ifelse(str_detect(names(DB1[i]), "Absolute") == TRUE,
                              names(DB1)[i] <- "AbsPress",
                                     DB1 <- DB1))))
                              
}
# remcols
# remcols <- remcols[!is.na(remcols)]
# DB1 <- DB1[,c(-remcols)]
DB1 <- DB1 %>%
  filter(Stage > -8)
DB1 <- DB1 %>%
  mutate(Date = as.POSIXct(mdy_hms(Date))) #changing to same date format as other sheets
DB1 <- DB1 %>%
  mutate(name = "DB")
DB1 <- DB1[,c("Date","Stage","Wtemp","BaroPress", "AbsPress","name")]
write_csv(DB1,"DB.csv")
#ncol(DB1)
# DB1 %>%
#   ggplot(aes(x = Date,
#              y = Stage)) +
#   geom_point()


View(DB1)
#DB1 <- DB1[,c(1,4,5,2,6)]
#SB1 <- SB[,c(1,2,4,7:ncol(SB))]
#datacolnames <- c("Date","Stage","Wtemp","BaroPress","AbsPress") #these are 

colnames(DB1) <- datacolnames

DB1 <- DB1 %>%
  mutate(Date = as.POSIXct(mdy_hms(Date))) #changing to same date format as other sheets
         
DB1 <- DB1 %>%
  mutate(name = as.character("DB1"))
write_csv(DB1, "DB1.csv")



x <- rbind(BBx, BB) #puts the data together vertically
x <- x[!duplicated(x$Date), ] #deletes rows of the same date
y <- write_csv(x, "BB.csv")

#lapply(BB1, class)

str_length(df$Date) > 15
ifelse(str_length(df$Date) >15,
       mutate(df$Date = str_sub(df$Date, 1,15)))
# df %>%
if (str_length(df$Date) > 15) {
  df$Date <- str_sub(df$Date, 1,-4)
}

x <- rbind(df2, df) #puts the data together vertically
x <- x %>%
  arrange(Date)
#print(nrow(x))
x <- x[!duplicated(x$Date), ] #deletes rows of the same date
# print(nrow(x))
# print(paste0(as.character(input$dropdown2), ".csv"))
#x <- x[,c("Date","Stage","Wtemp","BaroPress", "AbsPress","name")]
write_csv(x, paste0(as.character(input$dropdown2),".csv"))
    
  #mutate(Date = Date))

#pato code
ALL=join_all(list(SB,KB,JS1,CBT,CBS,CBC,BB),by="Date")
str(ALL)
ALL=ALL%>%
gather(Site,Stage,CBC:CBT)
head(ALL)

##We want each Site to be an ordinal factor (no particular order to the factors)...
ALL$Site=as.factor(ALL$Site)

##Set up colors for each site for consistency...
color_map=c(SB="darkturquoise",KB="black",JS="yellow3",
              BB="red",CBC="purple",CBS="green3",CBT="orange")
ay=list(ticks= "outside",overlaying = "y",showgrid=FALSE,
          range=c(0,7.5),side = "right",title = "Precipitation (in)")
pp=plot_ly(ALL)%>%
  add_trace(x=~Date,y=~Stage,color=~Site,colors=color_map,type='scatter',mode='lines')%>%
  add_bars(data=HP,x=~Date,y=~Precip,yaxis="y2",name='Precipitation',type='bar',color=I("blue"))%>%
  layout(title = "All Sites - Hourly Precipitation and Stage (September 11th-September 12th, 2018)",
         yaxis=list(range=c(0,7.5),ticks="inside",title="Stage (ft)"),
         yaxis2=ay,
         xaxis=list(title="Date"))
#this makes one large data frame from the list
#then you can write it as csv
df <- do.call(rbind.data.frame, ldf) #makes every element in list into one large datasheet
write_csv(df,"allsitesdischarge.csv") #write_csv doesn't make a redundant first column like write.csv does

#cuts dates into 5 minute intervals
x <- SB %>%
  mutate(Date = cut(Date, breaks = "5 min"))

##a couple helpful lines of code for troubleshooting r shiny
#p <- input$map1_marker_click$id
#print(ldf[[input$map1_marker_click$id]])
#print(lapply(p, class))

#print(stream())
#x <- paste0("ldf$", input$map1_marker_click$id)
#print(stream())
