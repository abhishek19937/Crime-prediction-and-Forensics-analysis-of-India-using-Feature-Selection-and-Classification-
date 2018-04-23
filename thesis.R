

India_rawdata=read.csv("C:\\Users\\lenovo\\Desktop\\crime1.csv",header=TRUE)
India_rawdata

# summary of the data
summary(India_rawdata)

dim(India_rawdata)
head(India_rawdata)
tail(India_rawdata)

colnames(India_rawdata)
attach(India_rawdata)


library(reshape2)
library(ggplot2)
d <- melt(India_rawdata[,-c(2:4)])
ggplot(d,aes(x = value)) + 
  facet_wrap(~variable,scales = "free_x") + 
  geom_histogram()




table(India_rawdata$PdDistrict)
data <- sample(2, nrow(India_rawdata), replace = TRUE, prob = c(0.67,0.33))
trainD <- India_rawdata[data==1,]
testD <- India_rawdata[data==2,]
nrow(trainD)
nrow(testD)
library(e1071)
e1071model <- naiveBayes(PdDistrict ~ .,data=trainD)
e1071model
e1071prediction <- predict(e1071model, testD)
e1071prediction
library(rminer)
mmetric(testD$PdDistrict, e1071prediction, c("ACC","PRECISION","TPR","F1"))# (mmetric is a predefine function we use that function for computing classification error metrics)






#Fuzzy logy

library(chron)


library(ggplot2)

India_rawdata <- read.csv(file = "C:\\Users\\lenovo\\Desktop\\crime1.csv")

save(India_rawdata, file="India_rawdata.rdata")


load("India_rawdata.rdata")


nrow(India_rawdata)
str(India_rawdata)


summary(India_rawdata)

head(India_rawdata)
India_rawdata <- India_rawdata[order(India_rawdata$IncidntNum),]
head(India_rawdata)
nrow(India_rawdata)
India_rawdata <- subset(India_rawdata, !duplicated(India_rawdata$IncidntNum))

nrow(India_rawdata)

India_rawdata$datetime <- paste(India_rawdata$Date, paste(India_rawdata$Time,":00",sep=''))

India_rawdata$Incident_Date <- as.POSIXlt(India_rawdata$datetime, format="%m/%d/%Y %H:%M")

#Split time from the Incident_Date into Incident_Time to perform analysis on the data by time of the day
India_rawdata$Incident_Time <- times(format(India_rawdata$Incident_Date, "%H:%M:%S"))

#Split Date from the Incident_Date field to perform analysis on day of the month
India_rawdata$Incident_Date <- as.POSIXlt(strptime(India_rawdata$Incident_Date, format="%Y-%m-%d"))

#Drop columns that are not needed for this analysis to keep it clean. The chosen columns are stored in a Vector
drops <- c("Descript", "PdDistrict", "Address", "Location", "datetime", "Date", "Time")

India_rawdata <- India_rawdata[, !(names(India_rawdata) %in% drops)]
head(India_rawdata)

colnames(India_rawdata)[c(5,6)] <- c("longitude", "latitude")

#The frequency of crimes need not be consistent throughout the day as certain crimes happen more in the night than the rest of the #hour. To check this, we can bucket the timestamps into few categories and then analyze the distribution of the crimes across different create 4 types Early Morning, Morning, Evening, and Night by grouping certain hours together.

time.tag <- chron(times=c("00:00:00", "06:00:00", "12:00:00", "18:00:00", "23:59:59"))

#Create a new column Incident_Time_Tag and use Chron's cut function to put different hours into the time breaks

India_rawdata$Incident_Time_Tag <- cut(India_rawdata$Incident_Time, breaks=time.tag, labels=c("Early Morning","Morning", "Evening", "Night"), include.lowest=TRUE)

head(India_rawdata)

India_rawdata$Incident_Month <- months(India_rawdata$Incident_Date, abbreviate=TRUE)

#Explore different crime categories that comes with the raw data and identify total number of crime categories

table(India_rawdata$Category)
length(unique(India_rawdata$Category))




#Create a new column Crime_Category from Category column and Group it
India_rawdata$Crime_Category <- as.character(India_rawdata$Category)

India_rawdata$Crime_Category <- ifelse(India_rawdata$Crime_Category %in% c("SEX OFFENSES, FORCIBLE", "PROSTITUTION", "SEX OFFENSES, NON FORCIBLE", "PORNOGRAPHY/OBSCENE MAT"), 'SEX', India_rawdata$Crime_Category)

India_rawdata$Crime_Category <- ifelse(India_rawdata$Crime_Category %in% c("DRIVING UNDER THE INFLUENCE", "DRUG/NARCOTIC", "DRUNKENNESS", "LIQUOR LAWS"), 'DRUGS', India_rawdata$Crime_Category)

India_rawdata$Crime_Category <- ifelse(India_rawdata$Crime_Category %in% c("FORGERY/COUNTERFEITING", "FRAUD", "BAD CHECKS"), 'FRAUD', India_rawdata$Crime_Category)

India_rawdata$Crime_Category <- ifelse(India_rawdata$Crime_Category %in% c("BURGLARY", "ROBBERY", "STOLEN PROPERTY", "EXTORTION"), 'ROBBERY', India_rawdata$Crime_Category)

India_rawdata$Crime_Category <- ifelse(India_rawdata$Crime_Category %in% c("LARCENY/THEFT", "VEHICLE THEFT", "RECOVERED VEHICLE", "EMBEZZLEMENT", "RECOVERED VEHICLE"), 'THEFT', India_rawdata$Crime_Category)

India_rawdata$Crime_Category <- ifelse(India_rawdata$Crime_Category %in% c("VANDALISM", "ARSON"), 'ARSON', India_rawdata$Crime_Category)

India_rawdata$Crime_Category <- ifelse(India_rawdata$Crime_Category %in% c("MISSING PERSON", "KIDNAPPING"), 'KIDNAPPING', India_rawdata$Crime_Category)

India_rawdata$Crime_Category <- ifelse(India_rawdata$Crime_Category %in% c("BRIBERY", "DISORDERLY CONDUCT", "FAMILY OFFENSES", "GAMBLING", "LOITERING", "RUNAWAY", "OTHER OFFENSES", "SUSPICIOUS OCC"), 'OTHER OFFENSES', India_rawdata$Crime_Category)

India_rawdata$Crime_Category <- ifelse(India_rawdata$Crime_Category %in% c("NON-CRIMINAL", "SUICIDE"), 'NON-CRIMINAL', India_rawdata$Crime_Category)

#Explore the crime categories after grouping similar crimes together. The crime categories are now grouped into 13 categories instead #of 37 which will be easy for analysis purpose.

table(India_rawdata$ Crime_Category)
length(unique(India_rawdata$ Crime_Category))
qplot(India_rawdata$Crime_Category, xlab = "Crime_Category", main = "Crimes in INDIA") + scale_y_continuous("# Crimes")
qplot(India_rawdata$Incident_Time_Tag, xlab="Time of day", main = "Crimes by time of day") + scale_y_continuous("Number of crimes")
qplot(India_rawdata$DayOfWeek, xlab= "Day of week", main= "Crimes by day of week") + scale_y_continuous("Number of crimes")
India_rawdata$Incident_Month <- factor(India_rawdata$Incident_Month, levels= c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

qplot(India_rawdata$Incident_Month, xlab= "Incident_Month", main= "Crimes by month") + scale_y_continuous("Number of crimes")


#Aggregate the Crime Category by different time buckets
incidents_by_time <- aggregate(India_rawdata$Crime_Category, by = list(India_rawdata$Crime_Category, India_rawdata$Incident_Time_Tag), FUN = length)

#Name the columns of the new data frame
names(incidents_by_time) <- c("Crime_Category", "Incident_Time_Tag", "Count")
ggplot(incidents_by_time, aes(x= Crime_Category, y= factor(Incident_Time_Tag))) +
  geom_tile(aes(fill= Count)) + scale_x_discrete("Crime", expand = c(0,0)) +
  scale_y_discrete("Time of day", expand = c(0,-2)) +
  scale_fill_gradient("Number of crimes", low = "gray", high = "red") +
  theme_bw() + ggtitle("Crimes by time of day in India") +
  theme(panel.grid.major = element_line(colour = NA), panel.grid.minor = element_line
        (colour = NA))

incidents_by_dayofweek <- aggregate(India_rawdata$Crime_Category, by = list(India_rawdata$Crime_Category, India_rawdata$DayOfWeek), FUN = length)

names(incidents_by_dayofweek) <- c("Crime_Category", "DayOfWeek", "Count")


ggplot(incidents_by_dayofweek, aes(x= Crime_Category, y= factor(DayOfWeek))) +
  geom_tile(aes(fill= Count)) + scale_x_discrete("Crime", expand = c(0,0)) +
  scale_y_discrete("Day of Week", expand = c(0,-2)) +
  scale_fill_gradient("Number of crimes", low = "gray", high = "red") +
  theme_bw() + ggtitle("Crimes by day of week in INDIA") +
  theme(panel.grid.major = element_line(colour = NA), panel.grid.minor = element_line
        (colour = NA))

incidents_by_monthofyear <- aggregate(India_rawdata$Crime_Category, by = list(India_rawdata$Crime_Category, India_rawdata$Incident_Month), FUN = length)

names(incidents_by_monthofyear) <- c("Crime_Category", "Incident_Month", "Count")

ggplot(incidents_by_monthofyear, aes(x= Crime_Category, y= factor(Incident_Month))) +
  geom_tile(aes(fill= Count)) + scale_x_discrete("Crime", expand = c(0,0)) +
  scale_y_discrete("Month of Year", expand = c(0,-2)) +
  scale_fill_gradient("Number of crimes", low = "gray", high = "red") +
  theme_bw() + ggtitle("Crimes by month of year") +
  theme(panel.grid.major = element_line(colour = NA), panel.grid.minor = element_line
        (colour = NA))
