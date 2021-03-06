#######################################################################
########Viola Glenn -- RTI Research Data Science Excercise 02#########
#####################################################################

###########CODE BEGINS WITH PROGRAM WRITTEN BY AIRSAFE.COM##########

#Using code constructed by AirSafe.com to: 
#import + QA + prelim descriptives on NTSB XML dataset
#Full report including Git is saved here: 
#http://www.airsafe.com/analyze/ntsb.database.html

#---------- BEGIN AIRSAFE PROGRAM -------------
  
  # Cleaning and summarizing data from the NTSB Incident database
  # Created by Todd Curtis of AirSafe.com (tcurtis@airasfe.com), December 2015
  #
  # The online accessible NTSB aviation accident database contains information from 1982 and later about 
  # civil aviation accidents and selected incidents within the United States, its 
  # territories and possessions, and in international waters.
  # 
  # The database can be accessed at http://www.ntsb.gov/_layouts/ntsb.aviation/index.aspx,
  # and the results can be downloaded into either an XML file callled AviationData.xml
  # or a text file called AviationData.txt.
#
# You must first to to the NTSB database page and specifiy the range of data to be
# downloaded. Then you download it to the hard drive or other location where you will
# also have the R code that you will use to analyze the data.
#
# The online NTSB database allows users to download the output either as a 
# text file or an XML file. The following program takes the XML  version of 
# the output, processes the data to make it suitable for analysis by R, and
# creates a CSV file that can be analyzed by a wide rage of data analysis programs.

# In addition to converting the input file, it provides sevaral example summary 
# statistics concerning the distribution of accidents by geographical area,
# time, and severity of outcomes.

# Step 0: Step 0:  Load the NTSB output file

require(XML)
if (!require("XML")){ 
  install.packages("XML") 
} 
library(XML) 

data <- xmlParse("C:\\Users\\Viola\\Documents\\GitHub\\exercises\\exercise02\\data\\AviationData.xml")

# The XML file is converted to a list format where each row contains both 
# the name of the variable and the value fl that variable
ntsb.data <- xmlToList(data)

# Converts to data frame but transposes values, reverse the transposal and remove 
# unneeded row names
ntsb.data = as.data.frame(ntsb.data) 
ntsb.data = as.data.frame(t(ntsb.data))
rownames(ntsb.data) = NULL

ntsb.data.raw = ntsb.data # Raw data on standby in case of a later problem

# DATA CLEANING

# Step 1: Ensure that the column (variable) names and data types fit the standard set 
# by the process used to convert the text file data into a CSV file

colnames(ntsb.data) = c("Event.Id", "Investigation.Type", "Accident.Number",
                        "Event.Date", "Location",  "Country", "Latitude", "Longitude",
                        "Airport.Code", "Airport.Name","Injury.Severity", "Aircraft.Damage",
                        "Aircraft.Category", "Registration.Number", "Make", "Model",
                        "Amateur.Built","Number.of.Engines", "Engine.Type", "FAR.Description",
                        "Schedule","Purpose.of.Flight", "Air.Carrier", "Total.Fatal.Injuries",
                        "Total.Serious.Injuries", "Total.Minor.Injuries", "Total.Uninjured",
                        "Weather.Condition", "Broad.Phase.of.Flight", "Report.Status",
                        "Publication.Date")

# Step 2: Ensure character variables are of type character. 
char.vecs = c("Event.Id","Investigation.Type","Accident.Number",       
              "Location", "Country", "Airport.Code", "Airport.Name", "Injury.Severity",
              "Aircraft.Damage", "Aircraft.Category", "Registration.Number", "Make",
              "Model", "Amateur.Built", "Engine.Type", "FAR.Description","Schedule",              
              "Purpose.of.Flight", "Air.Carrier", "Weather.Condition",
              "Broad.Phase.of.Flight", "Report.Status")
char.vecs.ndx = which(colnames(ntsb.data) %in% char.vecs)

# As part of the process, need to ensure that there are not extra spaces
# at the beginning or end of each character value

# ================================
# Use function 'stripper', which works like the str_trim() function in the strigr package  
# Using this function to use only base R package.

stripper <- function(x){
  # This function removes leading and trailing spaces from a vector.
  # Space characters include tab, newline, vertical tab, form feed, 
  # carriage return, space.
  # Equivalent to the str_trim() function in the strigr package   
  x = as.character(x)
  x = sub("[[:space:]]+$", "", x) # Remove leading space characters
  x = sub("^[[:space:]]+", "", x) # Remove trailing space characters
  return(x)
}
# ================================

# Ensure that the character variable is of type character, then remove extra spaces
for (i in 1:length(char.vecs.ndx)) {
  ntsb.data[,char.vecs.ndx[i]] = as.character(ntsb.data[,char.vecs.ndx[i]])
  ntsb.data[,char.vecs.ndx[i]] = stripper(ntsb.data[,char.vecs.ndx[i]])
}

# Step 3: Ensure numerical variables are of type numeric

# Find which columns match the following known numerical vectors
num.vecs = c("Latitude", "Longitude", "Number.of.Engines", "Total.Fatal.Injuries",  
             "Total.Serious.Injuries", "Total.Minor.Injuries", "Total.Uninjured")

# Creates a vector for the column numbers for numeric variables
num.vecs.ndx = which(colnames(ntsb.data) %in% num.vecs)

# Note: This step appears to replace missing numeric values with NA
for (i in 1:length(num.vecs.ndx)){
  ntsb.data[,num.vecs.ndx[i]] = as.numeric(as.character(ntsb.data[,num.vecs.ndx[i]]))
}

# Step 4: Change date variables into a format suitable for R

# Dates are in form 01/31/2006, must convert to a date format of yyyy-mm-dd
# Two date variables, Event.Date and Pulication Date
ntsb.data$Event.Date = as.Date(ntsb.data$Event.Date, "%m/%d/%Y")
ntsb.data$Publication.Date = as.Date(ntsb.data$Publication.Date, "%m/%d/%Y")
# Note: This step appears to replace missing date values with NA

# Now will have separate columns for Year, Month, Day, and Weekday for Event.Date
ntsb.data$Year = as.numeric(format(ntsb.data$Event.Date, "%Y")) # Ensure it is a numeric variable
ntsb.data$Month = months(ntsb.data$Event.Date, abbreviate = TRUE)
ntsb.data$Day = format(ntsb.data$Event.Date, "%d")
ntsb.data$Weekday = weekdays(ntsb.data$Event.Date, abbreviate = TRUE)

# DATA CONVERSION: Ordering days of the week and months of the year 

# Make months and days of the week factors and order them as they are in a calendar
ntsb.data$Month = factor(ntsb.data$Month,levels=c("Jan", "Feb","Mar", "Apr","May",
                                                  "Jun","Jul","Aug","Sep","Oct", "Nov","Dec"), ordered=TRUE)


ntsb.data$Weekday = factor(ntsb.data$Weekday,levels=c("Sun","Mon","Tue",
                                                      "Wed","Thu","Fri","Sat"), ordered=TRUE)

# Step 5 Eliminate any rows (records) which has no date (Event.Date is NA)
elim.row = which(is.na(ntsb.data$Event.Date))
if (length(elim.row)>0) ntsb.data = ntsb.data[-elim.row,] # Eliminates all rows with only NA value for Event.Date

# Step 6: Changing blank, "N/A","Unknown", and similar responses to NA

# First, define  a vector of words or phrases that are to be replaced with NA
repl.words = c("", ".", "N/A", "UNK","Unknown","Unavailable")

# Then for varable (column) in the NTSB database, check each character column
# for the presence of replacement words. Each list element will have 
# information on each column, including the the position (row)
# of each replacement word (if any).

# Note that the following checks all columns, event non-character one, but in the end,
# only character columns will have any non-zero values for replacement words
repl.list = apply(ntsb.data,2, function(x) which(x %in% repl.words))

# The number of times replacement words occur for each database variable (column), 
# is placed in a one-column data frame where the row names correspond to the 
# variable names the column has the number of times the replacement words occur
with.missing = as.data.frame(sapply(repl.list,length)) 
colnames(with.missing) = "Replacement.Words"

# Identify columns corresponding with character-based
# variables with at least one replacement word
with.missing.ndx = which(with.missing[,1]>0) 

# Replace replacement words with NAs in those columns containing
# one or more replacement words

for(i in 1:length(with.missing.ndx)){
  repl.vector=ntsb.data[,with.missing.ndx[i]]%in%repl.words 
  ntsb.data[repl.vector,with.missing.ndx[i]]=NA
} 


# Step 7: Specify city name, state abbreviation, and full state name
# for any location in the United states

# Will focus only on rows with a US city address entered, implying the following is true
# ntsb.data$Location is not NA and ntsb.data$Country=="United States"
city.locs = !(is.na(ntsb.data$Location)) & ntsb.data$Country=="United States"

# The following are the locations in the vector where this is true
city.locs.ndx = which(city.locs) 

# STATE CODES: Before adding full state names to data frame, must match
# two-letter state codes with full state names.

# First, create a vector of full state names by agmenting the built-in R 
# lists of names and abbreviations of the 50 states (state.name and state.abb) 
# with all other names and two-letter abbreviations used by the US Postal Service (USPS)

# Source is USPS Publication 28 - postal addressing standards 
# Located at http://pe.usps.gov/text/pub28/welcome.htm accessed 9 December 2015

extra.abb = c("AS", "DC", "FM","GU","MH", "MP", "PW", "PR", "VI", "GM", "AO","PO")
extra.name = c("American Samoa", "District of Columbia", "Federated States of Micronesia",
               "Guam", "Marshall Islands", "Northern Mariana Islands", "Palau",
               "Puerto Rico", "Virgin Islands", "Gulf of Mexico", "Atlantic Ocean",
               "Pacific Ocean")

# Now append them to the R-provided list of 50 state names and abbreviations
usps.abb = c(state.abb,extra.abb)
usps.state = c(state.name,extra.name)

# Next is to identify the number of commas in city identifiers
# This is because the NTSB output did not have separate variable for state
# States in NTSB output are two letter codes preceeded by a comma and a space

# These three vectors each initialized with a number of NA values equal to the
# number of rows to ensure that the final vector will be compatible with ntsb.data
comma.pos = rep(NA,nrow(ntsb.data)) # start with NA for number of commas for all Locations
city.vec = rep(NA,nrow(ntsb.data))
state.vec = rep(NA,nrow(ntsb.data))

for(x in 1:length(city.locs.ndx)){
  # Create a list that contains vector of comma positions
  comma.pos.list = gregexpr(",", ntsb.data$Location[city.locs.ndx[x]], fixed=TRUE)
  comma.pos.vec = comma.pos.list[[1]] # Vector of comma positinos
  comma.pos[x] = comma.pos.vec[length(comma.pos.vec)]
  
  # Get the length of the Location field character string
  num.chars = nchar(ntsb.data$Location[city.locs.ndx[x]]) 
  
  # Determine state code if location has enough characters for comma, space, and code
  if(comma.pos[x] >= 1 & num.chars >= 4){
    # Use last comma position to determine where split character string and find state code
    city.vec[city.locs.ndx[x]] =  substr(ntsb.data$Location[city.locs.ndx[x]], 1,(comma.pos[x]-1)) 
    state.vec[city.locs.ndx[x]] =  substr(ntsb.data$Location[city.locs.ndx[x]], (comma.pos[x]+2),num.chars)       
  } # End of if statement for creating city name and state abbreviation
  
} # End of process for finding US city names and state abbreviations     


# Initialize the full state name vector with a number of NA values equal to the
# number of rows to ensure that the final vector will be compatible with ntsb.data
state.full=rep(NA,nrow(ntsb.data))

for (i in 1:length(city.locs.ndx)){
  if(state.vec[i] %in% usps.abb) {
    state.full[i]=usps.state[grep(state.vec[i], usps.abb)]
  }
  # Erase city and two-letter state code if state code is not to USPS standards
  if(!(state.vec[i] %in% usps.abb)) {
    city.vec[i]=NA
    state.vec[i]=NA
  }
}

# Can now add city and state abbrevications, and full state names to data frame
ntsb.data$City=city.vec
ntsb.data$State.code=state.vec
ntsb.data$State=state.full

# Step 8: Arrange the new columns in logical groupings

new.cols = c("Event.Id", "Investigation.Type", "Accident.Number",
             "Event.Date", "Year", "Month", "Day", "Weekday",
             "Location", "City", "State.code", "State", "Country", 
             "Airport.Code", "Airport.Name", "Latitude", "Longitude",
             "Injury.Severity", "Aircraft.Damage", "Aircraft.Category",
             "Registration.Number", "Make", "Model", "Amateur.Built",
             "Number.of.Engines", "Engine.Type", "FAR.Description", "Schedule",
             "Purpose.of.Flight", "Air.Carrier", "Total.Fatal.Injuries",
             "Total.Serious.Injuries", "Total.Minor.Injuries", "Total.Uninjured",
             "Weather.Condition", "Broad.Phase.of.Flight", "Report.Status",
             "Publication.Date")

ntsb.data = ntsb.data[,new.cols]

# Step 9 (Final step): Save the processed data frame as a CSV file
write.csv(ntsb.data, file = "ntsb_data.csv")

# SUMMARY STATISTICS

print("Summary statistics")

paste("Number of records with a US location - ", format(nrow(ntsb.data[which(ntsb.data$Country=="United States"),]), big.mark=","),".", sep="")

paste("Total number of records involving fatalities - ", format(sum(ntsb.data$Total.Fatal.Injuries>=1, na.rm=TRUE), big.mark=","),".", sep="") 

paste("Total fatalities - ", format(sum(ntsb.data$Total.Fatal.Injuries, na.rm=TRUE), big.mark=","),".", sep="") 

print("Table of reports by state: 1982-2015")
table(ntsb.data[which(ntsb.data$Year>=1982),]$State.code)

print("Table of top 15 states by number of events: 1982-2015")
sort(table(ntsb.data[ntsb.data$Year>=1982,]$State.code), decreasing = TRUE)[1:15]


# Vector of fatal events from 1982 onwards
fatal.vec = which(ntsb.data$Total.Fatal.Injuries>=1 & ntsb.data$Year>=1982)

# Data frame of all fatals from 1982 and beyond
fatal.df = ntsb.data[fatal.vec,]

# Histogram of top 15 states by number of events: 1982-2015
barplot(sort(table(ntsb.data[ntsb.data$Year>=1982,]$State.code),
             decreasing = TRUE)[1:15], col="dodgerblue",xlab="State", ylab="Events", 
        cex.names = 0.7, main="Top 15 states by number of events 1982-2015")


print("Table of top 15 states by number of fatal events: 1982-2015")
# Vector of all fatals for events with US state code from 1982 and beyond
sort(table(ntsb.data[fatal.vec,]$State.code), decreasing = TRUE)[1:15]

# Top 15 states by total fatalities
print("Table of top 15 states by number of fatalities: 1982-2015")
sort(as.table(tapply(fatal.df$Total.Fatal.Injuries, fatal.df$State.code, sum)), decreasing = TRUE)[1:15]

# Histogram of top 15 states by number of fatal events 1982-2015
barplot(sort(table(ntsb.data[fatal.vec,]$State.code), decreasing = TRUE)[1:15],
        col="dodgerblue",xlab="State", ylab="Fatal events",
        cex.names = 0.7, main="Top 15 states by number of fatal events: 1982-2015")


# Table of events by day of the week 
print("Events by day of the week: 1982-2015")
table(ntsb.data[ntsb.data$Year>=1982,]$Weekday)

# Histogram events by day of the week
barplot(table(ntsb.data[ntsb.data$Year>=1982,]$Weekday), col="dodgerblue",
        xlab="Day", ylab="Events", main="Events by day of week: 1982 - 2015")

# Table of fatal events by day of the week 
print("Fatal events by day of the week 1982-2015")
table(fatal.df$Weekday)

# Histogram of fatal events by day of the week
barplot(table(fatal.df$Weekday), col="dodgerblue", cex.names = 1.2,
        xlab="Day", ylab="Events", main="Fatal events by day of week: 1982 - 2015")

# Table of events by month of the year
print("Events by month of the year 1982-2015")
table(ntsb.data[ntsb.data$Year>=1982,]$Month)

# Histogram of events by month of the year
barplot(table(ntsb.data[which(ntsb.data$Year>=1982),]$Month), col="dodgerblue", cex.names = 0.8,
        xlab="Month", ylab="Events", main="Events by month: 1982-2015")

# Table of fatal events by month of the year
print("Events by month of the year 1982-2015")
table(fatal.df$Month)

# Histogram of fatal events by month of the year
barplot(table(fatal.df$Month), col="dodgerblue", cex.names = 0.8,
        xlab="Day", ylab="Fatal events", main="Fatal events by month: 1982-2015")



# Plot of events by year 1982-2015
barplot(table(ntsb.data[ntsb.data$Year>=1982,]$Year), col="dodgerblue",
        xlab="Year", ylab="Events", main="Events by year: 1982 - 2015")


# Histogram of fatal events by year 1982 - 2015
barplot(table(ntsb.data[fatal.vec,]$Year), col="dodgerblue",
        xlab="Year", ylab="Fatal events", main="Fatal events by year: 1982 - 2015")

# -------
# Fatalites by year 1982 - 2015

# Do a tapply for sums by category then ensure it is table
death.table = as.table(tapply(fatal.df$Total.Fatal.Injuries, fatal.df$Year, sum))

barplot(death.table, col="dodgerblue",
        xlab="Year", ylab="Fatalities", main="Fatalities by year: 1982 - 2015")
# -------

# Total fatalities by US state and territory
as.table(tapply(fatal.df$Total.Fatal.Injuries, fatal.df$State.code, sum))

# Total fatal events  by US state and territory
table(fatal.df$State.code)

# Total fatal events by US state and territory sorted
sort(table(fatal.df$State.code), decreasing=TRUE)

#---------- END AIRSAFE PROGRAM -------------

#Load required packages for import, exploration, text 

library(XML)
library(jsonlite)
library(plyr)
library(tm)

#Viola Analysis Step 1:
#Visual inspection of categorical variables which are observable or controllable
#by average air traveler 

#Convert all character variables to factors for easier analysis
ntsb.data.work <- as.data.frame(unclass(ntsb.data))

summary(ntsb.data.work)
attach(ntsb.data.work) 
#Nominate variables for further investigation: 
#Investigation.Type, Aircraft.Category, Make, Amateur.Built, Number.of.Engines, 
#Broad.Phase.of.Flight, Weather.Condition

#Total Events by Nominated variable category
par(mfrow=c(3,3))
sapply(ntsb.data.work[ntsb.data.work$Year>=1982, 
         c("Investigation.Type","Year", "Month", "Weekday",  
            "Aircraft.Category", "Amateur.Built", "Number.of.Engines", 
            "Engine.Type", "Schedule", "Purpose.of.Flight", 
            "Broad.Phase.of.Flight", "Weather.Condition")], function(x)
         barplot(sort(table(x), decreasing=TRUE), 
            col="dodgerblue", ylab="Events", 
            cex.names=0.7, las=2))

detach(ntsb.data.work)
attach(fatal.df)

#Total FATAL Events by Nominated variable category 
#I am interested in where these differ, will focus on fatal events going forward
par(mfrow=c(3,3))
sapply(fatal.df[fatal.df$Year>=1982, 
          c("Investigation.Type","Year", "Month", "Weekday",  
            "Aircraft.Category", "Amateur.Built", "Number.of.Engines", 
            "Engine.Type", "Schedule", "Purpose.of.Flight", 
            "Broad.Phase.of.Flight", "Weather.Condition")], function(x)
          barplot(sort(table(x), decreasing=TRUE), 
            col="darkturquoise", ylab="Fatal Events",
            cex.names=0.7, las=2)) 


#Largest differences in total events vs. fatal events occurs:
# Schedule, Broad.Phase.of.Flight, Weather.Condition 
# Nominate these for further investigation

attach(ntsb.data.work)

#Create an indicator variable to flag fatal incidents 
#This will allow for the calculation of percent of incidents with at least 1 fatality
ntsb.data.work$fatal.id <- 0
ntsb.data.work$fatal.id[ntsb.data.work$Total.Fatal.Injuries>0] <- 1

#QA variable created above (fatal.id)- PASS
#ntsb.data.work$nonfatal.id <- 1
#ntsb.data.work$nonfatal.id[ntsb.data.work$fatal.id>0] <- 0
#table(ntsb.data.work$nonfatal.id, ntsb.data.work$fatal.id)

#Rates of fatal incidents (fatal incidents/total incidents) for nominated variables

#NOTE: Would optimize below w/ more time

ag.schedule <- aggregate(fatal.id ~ Schedule, data=ntsb.data.work, FUN=mean)
ggplot(data=ag.schedule, aes(x=ag.schedule$Schedule, y=ag.schedule$fatal.id)) + 
  geom_bar(stat="identity", fill="#66CC99") +
  labs(x="Schedule", y="Fatal Incident Proportion", title="Fatal Incident Proportion, by Schedule") +
  theme(axis.text.x=element_text(angle=50))

ag.phase <- aggregate(fatal.id ~ Broad.Phase.of.Flight, data=ntsb.data.work, FUN=mean)
ggplot(data=ag.phase, aes(x=ag.phase$Broad.Phase.of.Flight, y=ag.phase$fatal.id)) + 
  geom_bar(stat="identity", fill="#66CC99") +
  labs(x="Phase", y="Fatal Incident Proportion", title="Fatal Incident Proportion, by Flight Phase") +
  theme(axis.text.x=element_text(angle=50))

ag.weather <- aggregate(fatal.id ~ Weather.Condition, data=ntsb.data.work, FUN=mean)
ggplot(data=ag.weather, aes(x=ag.weather$Weather.Condition, y=ag.weather$fatal.id)) + 
  geom_bar(stat="identity", fill="#66CC99") +
  labs(x="Weather", y="Fatal Incident Proportion", title="Fatal Incident Proportion, by Weather") +
  theme(axis.text.x=element_text(angle=50))

#---------- BEGIN Json PROGRAM -------------
#The following section loads 144 individual .json files with additional
#narrative text and Event.id to match above data. It then performs some
#basic text analysis using package tm.


#Import each JSON file from working directory
setwd("C:\\Users\\Viola\\Documents\\GitHub\\exercises\\exercise02\\data")
list.json = list.files(pattern="*.json")
for (i in 1:length(list.json)) assign(list.json[i], fromJSON(list.json[i]))

#NOTE: I've not been able to outline an approach to convert all .json files
#to a single dataframe for analysis. Below are 3 options to convert a single
#nested item to the appropriate dataframe.

##** Remaining analysis will use a single JSON file to outline procedure.

test <- fromJSON("NarrativeData_000.json")$data
test2 <- as.data.frame(NarrativeData_000.json$data)
test3 <- unnest(NarrativeData_000.json) #in library(tidyr)

#Append all JSON together
#NOTE: Again - This is only pulling 1 JSON file currently due to issue noted above
list.json <- lapply(ls(pattern = "Narrative"), get)
all.json <- do.call("rbind.fill", list.json)

#Add new information to original dataset
all.json$Event.Id <- all.json$EventId
ntsb.data.json <- merge(ntsb.data.work, all.json, by="Event.Id")

#Begin frequency counts
#Many more ideas for how to implement this, but time does not allow

library(tm)

#Pool all words from narrative fields, tokenize, 
#and calculate proportion of total with word.freq function

word.freq=function(x) {
  alldocs <- paste(x, collapse=" ")
  corpus <- Corpus(VectorSource(alldocs))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeWords, stopwords(kind="en"))
  dtm <- DocumentTermMatrix(corpus)
  dtm2 <- as.matrix(dtm)
  frequency <- colSums(dtm2)
  frequency <- sort(frequency, decreasing=TRUE)
  head(frequency, 10)/sum(frequency)
}

#Apply word.freq to nominated variables 
text.schedule <- by(ntsb.data.json$probable_cause, ntsb.data.json$Schedule, word.freq)
text.phase <- by(ntsb.data.json$probable_cause, ntsb.data.json$Broad.Phase.of.Flight, word.freq)
text.weather <- by(ntsb.data.json$probable_cause, ntsb.data.json$Weather.Condition, word.freq)
text.year <- by(ntsb.data.json$probable_cause, ntsb.data.json$Year, word.freq)
