# Sam Tenney
# Midterm 1
# Movie Gross Box Office Revenue

# http://www.boxofficemojo.com/quarterly/?chart=byquarter&view=releasedate&quarter=QQ where QQ is Q1-Q4 representing a given quarter

# webscraper for Q1 data 
# from http://www.boxofficemojo.com/quarterly/?chart=byquarter&quarter=Q1

# function that allows reading in data of the form $xxx,xxx,xxx.xx
setClass("AccountingNumber")
setAs("character", "AccountingNumber", 
      function(from) as.numeric(gsub(",", "", gsub("[:$:]", "", from) ) ) )
# data from webpage
library(RCurl)
library(XML)
# Q1 box office url
q1boxoffice.url<-paste("https://www.boxofficemojo.com/quarterly/?chart=byquarter&quarter=Q1")
# read webpage and store in memory
q1boxoffice.webpage<-htmlParse(getURL(q1boxoffice.url))
# create R dataset from webpage contents
q1boxoffice<-readHTMLTable(q1boxoffice.webpage,
                           header=TRUE,which=4,
                           colClasses=c("numeric","AccountingNumber","Percent","numeric",
                                        "AccountingNumber","Percent","character",
                                        "AccountingNumber","Percent") )
# keep only year and gross
q1boxoffice<-q1boxoffice[,1:2]
# change variable name so it doesn't have a space
names(q1boxoffice)<-c("year","gross")


##### Q2 #####

# Q2 box office url
q2boxoffice.url<-paste("https://www.boxofficemojo.com/quarterly/?chart=byquarter&quarter=Q2")
# read webpage and store in memory
q2boxoffice.webpage<-htmlParse(getURL(q2boxoffice.url))
# create R dataset from webpage contents
q2boxoffice<-readHTMLTable(q2boxoffice.webpage,
                           header=TRUE,which=4,
                           colClasses=c("numeric","AccountingNumber","Percent","numeric",
                                        "AccountingNumber","Percent","character",
                                        "AccountingNumber","Percent") )
# keep only year and gross
q2boxoffice<-q2boxoffice[,1:2]
# change variable name so it doesn't have a space
names(q2boxoffice)<-c("year","gross")

##### Q3 #####

# Q3 box office url
q3boxoffice.url<-paste("https://www.boxofficemojo.com/quarterly/?chart=byquarter&quarter=Q3")
# read webpage and store in memory
q3boxoffice.webpage<-htmlParse(getURL(q3boxoffice.url))
# create R dataset from webpage contents
q3boxoffice<-readHTMLTable(q3boxoffice.webpage,
                           header=TRUE,which=4,
                           colClasses=c("numeric","AccountingNumber","Percent","numeric",
                                        "AccountingNumber","Percent","character",
                                        "AccountingNumber","Percent") )
# keep only year and gross
q3boxoffice<-q3boxoffice[,1:2]
# change variable name so it doesn't have a space
names(q3boxoffice)<-c("year","gross")


##### Q4 #####

# Q4 box office url
q4boxoffice.url<-paste("https://www.boxofficemojo.com/quarterly/?chart=byquarter&quarter=Q4")
# read webpage and store in memory
q4boxoffice.webpage<-htmlParse(getURL(q4boxoffice.url))
# create R dataset from webpage contents
q4boxoffice<-readHTMLTable(q4boxoffice.webpage,
                           header=TRUE,which=4,
                           colClasses=c("numeric","AccountingNumber","Percent","numeric",
                                        "AccountingNumber","Percent","character",
                                        "AccountingNumber","Percent") )
# keep only year and gross
q4boxoffice<-q4boxoffice[,1:2]
# change variable name so it doesn't have a space
names(q4boxoffice)<-c("year","gross")

# Create 'qtr' for each quarter's dataframe 

q1boxoffice$qtr <- "Q1"
q2boxoffice$qtr <- "Q2"
q3boxoffice$qtr <- "Q3"
q4boxoffice$qtr <- "Q4"

# Combine the four dataframes 
boxoffice <- rbind(q1boxoffice, q2boxoffice, q3boxoffice, q4boxoffice)

# Sort the data from oldest to newest 
boxoffice <- boxoffice[order(boxoffice$year, boxoffice$qtr),]
boxoffice <- boxoffice[-149,]

# Analysis
library(ggplot2)
ggplot(boxoffice, aes(x = year, y = gross, color = qtr)) +
  geom_point() +
  geom_line() +
  labs(y = "Total Gross Box Office Revenue (in $ millions)", x = "Year", title = "Total Gross Box Office Revenue")

# Fit an ARIMA(1,1,1)x(1,1,1)4 model
library(astsa)
out_boxoffice <- sarima(boxoffice$gross, 1,1,1, 1,1,1, 4)

# Make a table to report the estimates and standard errors
out_boxoffice$ttable

boxoffice_future <- sarima.for(boxoffice$gross, n.ahead = 12, 1,1,1, 1,1,1, 4)
title(main = "Total Gross Box Office Revenue for Next 3 Years")

# Forecast quarterly total gross box office for the next 3 years with 95% PI
boxoffice_future_L <- boxoffice_future$pred - qnorm(0.975)*boxoffice_future$se
boxoffice_future_U <- boxoffice_future$pred + qnorm(0.975)*boxoffice_future$se

boxofficeTable <- cbind(boxoffice_future$pred, boxoffice_future_L, boxoffice_future_U)
colnames(boxofficeTable) <- c("Prediction Estimate", "Lower Bound", "Upper Bound")
boxoffice_ts <- ts(boxofficeTable, start = 1, end = 12)
boxoffice_ts

# Add prediction interval data to boxoffice dataset
boxoffice$up.se <- NA
boxoffice$lw.se <- NA
boxoffice <- boxoffice[,c(2,4,5,1,3)]
year <- c(rep(2019, 4), rep(2020,4), rep(2021,4))
qtr <- c("Q1","Q2","Q3","Q4","Q1","Q2","Q3","Q4","Q1","Q2","Q3","Q4")
boxoffice_plot <- cbind(boxoffice_ts, year, qtr)
colnames(boxoffice_plot) <- c("gross", "up.se", "lw.se", "year", "qtr")
box_office_plot <- rbind(boxoffice, boxoffice_plot)

# Change these variables back to numeric so plot be created correctly
box_office_plot$gross <- as.numeric(box_office_plot$gross)
box_office_plot$year <- as.numeric(box_office_plot$year)
box_office_plot$lw.se <- as.numeric(box_office_plot$lw.se)
box_office_plot$up.se <- as.numeric(box_office_plot$up.se)

# Graphic
ggplot() + 
  geom_point(data = subset(box_office_plot, box_office_plot$year < 2019 & box_office_plot$year > 2010), aes(x = year, y = gross, color = qtr)) +
  geom_line(data = subset(box_office_plot, box_office_plot$year < 2020 & box_office_plot$year > 2010),aes(x = year, y = gross, color = qtr)) +
  geom_point(data = subset(box_office_plot, box_office_plot$year > 2018), aes(x = year, y = gross, color = qtr), shape = 1) +
  geom_line(data = subset(box_office_plot, box_office_plot$year > 2018), aes(x = year, y = gross, color = qtr)) +
  geom_line(data = subset(box_office_plot, box_office_plot$year > 2018), aes(x = year, y = lw.se, color = qtr), linetype = "dashed", size = 1) +
  geom_line(data = subset(box_office_plot, box_office_plot$year > 2018), aes(x = year, y = up.se, color = qtr), linetype = "dashed", size = 1) +
  geom_vline(xintercept=2019, linetype="dotted") +
  labs(y = "Gross Box Office Revenue (in $ millions)", x = "Year", title = "Predicted Gross Box Office Revenue for Next 3 Years")
  
  

