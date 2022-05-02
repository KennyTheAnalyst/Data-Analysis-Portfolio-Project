remove.packages(c("ggplot2", "data.table"))
install.packages('Rcpp', dependencies = TRUE)
install.packages('ggplot2', dependencies = TRUE)
install.packages('data.table', dependencies = TRUE)
install.packages('ggthemes', dependencies = TRUE)
install.packages('dplyr', dependencies = TRUE)
install.packages('lubridate', dependencies = TRUE)
install.packages('scales', dependencies = TRUE)
install.packages('tidyr', dependencies = TRUE)
install.packages('DT', dependencies = TRUE)
install.packages('vroom', dependencies = TRUE)
install.packages('magrittr', dependencies = TRUE)
install.packages("rmarkdown", dependencies = TRUE)

#Importing all the files/library
library(ggplot2) #visualization matplotlib in case of python
library(ggthemes) #add-on with ggplot
library(dplyr) #data manipulation pandas in case of pandas
library(lubridate) #date time
library(scales) #graphical scaling
library(tidyr) #tidy data
library(vroom) #read rectangular data
library(DT)#table formatted result
library(magrittr) #improve readability and maintainability of code

#Reading the chunk of data
#6 months of data
uber_raw_data_apr14 <- read_csv("Uber-dataset/uber-raw-data-apr14.csv")
uber_raw_data_may14 <- read_csv("Uber-dataset/uber-raw-data-may14.csv")
uber_raw_data_jun14 <- read_csv("Uber-dataset/uber-raw-data-jun14.csv")
uber_raw_data_jul14 <- read_csv("Uber-dataset/uber-raw-data-jul14.csv")
uber_raw_data_aug14 <- read_csv("Uber-dataset/uber-raw-data-aug14.csv")
uber_raw_data_sep14 <- read_csv("Uber-dataset/uber-raw-data-sep14.csv")

#Combining all of the data
data_2014 <-rbind(uber_raw_data_apr14,uber_raw_data_may14,uber_raw_data_jun14,uber_raw_data_jul14,uber_raw_data_aug14,uber_raw_data_sep14)

#Visualizing the data
head(data_2014)

#Structure of the data
str(data_2014)

#Summary of the statistics
summary(data_2014)

#What are our primary observations from date/time we get the time frame

#Starting the analysis
data_2014$`Date/Time` <- as.POSIXct(data_2014$`Date/Time`,format = "%m/%d/%Y %H:%M:%S")

#Summary of the statistics
summary(data_2014)

#Extracting time from date/time
data_2014$Time <- format(as.POSIXct(data_2014$`Date/Time`,format = "%m/%d/%Y %H:%M:%S"), format = "%H:%M:%S")

data_2014$`Date/Time` <- ymd_hms(data_2014$`Date/Time`) #formatting
data_2014$day <- format(day(data_2014$`Date/Time`)) #day
data_2014$month <- format(month(data_2014$`Date/Time`, label = TRUE)) #month
data_2014$year <- format(year(data_2014$`Date/Time`, label = TRUE)) #year
data_2014$dayofweek <- format(wday(data_2014$`Date/Time`, label = TRUE)) #daysoftheweek

#Hour-Minute-Second
data_2014$hour <- factor(hour(hms(data_2014$Time))) #we want all of these as factors
data_2014$minute <- factor(minute(hms(data_2014$Time)))
data_2014$second <- factor(second(hms(data_2014$Time)))

#full data
head(data_2014)

#Let`s start the visualization

#Plotting the trip by hours in a day
hour_data <- data_2014 %>%
  group_by(hour) %>%
  summarise(Total = n())   #grouping the wrt hour and count

#Let`s see in tabular form
datatable(hour_data)

#Visualizing the data
ggplot(hour_data, aes(hour,Total)) +
  geom_bar(stat = "identity", fill = "black", color = "blue") +
  ggtitle("Trips By Hour") + 
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)

#Finding 1
#Most operations happen from 15:00 to 21:00 

month_hour_data <- data_2014 %>%
  group_by(month,hour) %>%
  summarise(Total= n())

#Let`s see in tabular form
datatable(month_hour_data)

#Let`s plot the same
ggplot(month_hour_data, aes(hour,Total, fill=month)) +
  geom_bar(stat = "identity") +
  ggtitle("Trips By Hour and month") +
  scale_y_continuous(labels = comma)

#Finding 2
#September has more rides than ever

sept_hour <- data_2014 %>%
  group_by(hour, month) %>%
  filter(month == "Sep") %>%
  summarise(Total =n())

ggplot(month_hour_data, aes(hour,Total, fill=hour)) +
  geom_bar(stat = "identity") +
  ggtitle("Trips By Hour and month for September") +
  scale_y_continuous(labels = comma)

#Finding 3
#The most trips are ordered at 5pm

apr_hour <- data_2014 %>%
  group_by(hour, month) %>%
  filter(month == "Apr") %>%
  summarise(Total =n())

ggplot(month_hour_data, aes(hour,Total, fill=hour)) +
  geom_bar(stat = "identity") +
  ggtitle("Trips By Hour and month for April") +
  scale_y_continuous(labels = comma)

#Plot the data grouped by day
day_data <- data_2014 %>%
  group_by(day) %>%
  summarise(Total = n())   #grouping the wrt hour and count

#Let`s see in tabular form
datatable(day_data)

#Visualizing the data
ggplot(day_data, aes(day,Total)) +
  geom_bar(stat = "identity", fill = "black", color = "blue") +
  ggtitle("Trips By Day") + 
  theme(legend.position = "none") +
  scale_y_continuous(labels = comma)

#Finding 4
#It is uniformingly distributed

#Month and day grouping
month_day_data <- data_2014 %>%
  group_by(month,day) %>%
  summarise(Total= n())

#Let`s see in tabular form
datatable(month_day_data)

#Let`s plot the same
ggplot(month_day_data, aes(day,Total, fill=month)) +
  geom_bar(stat = "identity") +
  ggtitle("Trips By day and month") +
  scale_y_continuous(labels = comma)

#September data
sept_day <- data_2014 %>%
  group_by(day, month) %>%
  filter(month == "Sep") %>%
  summarise(Total =n())

ggplot(sept_day, aes(day,Total, fill=day)) +
  geom_bar(stat = "identity") +
  ggtitle("Trips By day and month for September") +
  scale_y_continuous(labels = comma)

#Finding 5
#Day 13 has the highest number of rides

#Monthly trend
month_data <- data_2014 %>% group_by(month) %>% summarise(Total =n())
datatable(month_data)

#August and September had the highest kind of rides

ggplot(month_data, aes(month, Total, fill=month)) + 
  geom_bar(stat = "identity") +
  ggtitle("Trips By day and month") +
  scale_y_continuous(labels = comma)

#No particular trend was spotted

#Month-weekday
month_weekday_data <- data_2014 %>% group_by(month, dayofweek) %>% summarise(Total =n())
datatable(month_weekday_data)

ggplot(month_weekday_data, aes(month, Total, fill=dayofweek)) + 
  geom_bar(stat = "identity") +
  ggtitle("Trips By day and weekday") +
  scale_y_continuous(labels = comma)

#Only for the weekday
weekday_data <- data_2014 %>% group_by(dayofweek) %>% summarise(Total =n())
datatable(weekday_data)

ggplot(weekday_data, aes(dayofweek, Total, fill=dayofweek)) + 
  geom_bar(stat = "identity") +
  ggtitle("Trips By weekday") +
  scale_y_continuous(labels = comma)

#Analysis of bases
ggplot(data_2014, aes(Base)) + 
  geom_bar(fill = "darkred") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips By Bases") 

#It seems B02512 and B02764 are not much profitable

#Trip based on bases and months
ggplot(data_2014, aes(Base, fill=month)) + 
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips By Bases and Month") 

#Events in September in B02764 and event in April and May in B02617 must be observed

#Same as days of the week
ggplot(data_2014, aes(Base, fill=dayofweek)) + 
  geom_bar(position = "dodge") +
  scale_y_continuous(labels = comma) +
  ggtitle("Trips By Bases and DayofWeek") 

#Day and hour
day_and_hour <- data_2014 %>%
  group_by(day, hour) %>%
  dplyr::summarize(Total =n())
datatable(day_and_hour)

ggplot(day_and_hour, aes(day, hour, fill=Total)) +
  geom_tile(color = "white") +
  ggtitle("Heat Map by Hour and Day") 

#As seen from previous bar chart, 15:00 to 21:00 are the most profitable hours

#Plotting geo-distribution
min_lat <- 40.5774
max_lat <- 40.9176
min_long <- 74.15
max_long <-73.7004 #from summary stat
ggplot(data_2014, aes(x=Lon,y=Lat)) +
  geom_point(size=1) +
  scale_x_continuous(limits = c(min_long, max_long)) +
  scale_y_continuous(limits = c(min_lat, max_lat)) +
  theme_map() +
  ggtitle("NYC(lat-long chart) MAP BASED ON UBER RIDES DURING 2014 (APR-SEP) by BASE")


