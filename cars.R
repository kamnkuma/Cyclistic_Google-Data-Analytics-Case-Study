#install.packages("tidyverse")
#install.packages("ggplot2")  # Only required if not already installed
library(ggplot2)

library(tidyverse)
library(readxl)
library(dplyr)
rm(list=ls())
region_25<- read.csv("region25_en.csv")

View(region_25)

#region_25 %>% distinct()
str(region_25)


## To Look for any missing values in the dataset
region_25[region_25 == "" | region_25 == " "] <- NA       ## Replace all the blank & space by NA
sum(is.na(region_25))                 ## There were 166703 cells with missing data or with NA out of 15,13,200 which accounts to xyz% of the total dataset 

## To check which columns have the missing values
colSums(is.na(region_25))  


## Cleaning the dataset by removing the na values
region_25_cleaned_set <- na.omit(region_25)
sum(is.na(region_25_cleaned_set))
#View(region_25_cleaned_set)       ## The cleaned set now has 1002096
str(region_25_cleaned_set)

## Removing the unit "LTR" from engineDisplacement
region_25$engineDisplacement<-gsub(" LTR"," ",as.character(region_25$engineDisplacement))

##Converting the engineDisplacement column into numeric format
region_25 <- mutate(region_25, engineDisplacement = as.numeric(engineDisplacement))

## Verifying the above conversions are done
class(region_25$engineDisplacement)  


## checking for any NA values
sum(is.na(region_25))  
colSums(is.na(region_25)) 

region_25_cleaned_set <- na.omit(region_25_cleaned_set)



#region_25$year <- as.integer(region_25$year)
#egion_25$mileage <- as.integer(region_25$mileage)
#region_25$power <- as.integer(region_25$power)
#region_25$date <- as_datetime(region_25$date)
#region_25$parse_date <- as.Date(region_25$parse_date)

# Assuming 'df' is your data frame and 'column_name' is the name of the column to omit
#region_25 <- region_25[, -which(names(region_25) == "vehicleConfiguration")]
# Assuming 'df' is your data frame and 'column_name' is the name of the column to omit
#region_25$vehicleConfiguration <- NULL


# Assuming 'df' is your data frame and 'column_name' is the name of the column to omit
#vehconfig <- region_25[, -which(names(region_25) == "vehicleConfiguration")]
# Assuming 'df' is your data frame and 'column_name' is the name of the column to omit
#vehconfig$vehicleConfiguration <- NULL



extract_listing_id <- function(link) {
  # Use regular expression to extract the numeric identifier
  listingId <- gsub(".*/(\\d+)\\.html", "\\1", link)
  return(listingId)
}

# Apply the function to the entire link column
region_25$listingId <- sapply(region_25$link, extract_listing_id)




# Generate car_id values (unique identifiers)
carId <- 1:1513200

# Insert car_id as the first column in the dataset
region_25 <- cbind(carId, region_25)
last_row <- tail(region_25, n = 20)



#Convert 
#install.packages("httr")
library(httr)
get_exchange_rate <- function() {
  url <- "https://api.exchangerate-api.com/v4/latest/USD"
  response <- httr::GET(url)
  data <- httr::content(response, as = "parsed")
  return(data$rates$RUB)
}
rub_to_usd_rate <- get_exchange_rate()


# Convert prices from Russian Rubles to USD
region_25$priceUSD <- region_25$price / rub_to_usd_rate


# Assuming 'df' is your data frame and 'column_name' is the name of the column to omit
#region_25_cleaned_set_new <- region_25_cleaned_set_new[, -which(names(region_25_cleaned_set_new) == "price")]
# Assuming 'df' is your data frame and 'column_name' is the name of the column to omit
#region_25_cleaned_set_new$price <- NULL


colnames(region_25_cleaned_set_new)[colnames(region_25_cleaned_set_new) == "Power"] <- "Horsepower"
colnames(region_25_cleaned_set_new)[colnames(region_25_cleaned_set_new) == "name"] <- "modelName"
colnames(region_25_cleaned_set_new)[colnames(region_25_cleaned_set_new) == "brand"] <- "brandName"
colnames(region_25_cleaned_set_new)[colnames(region_25_cleaned_set_new) == "year"] <- "yearOfManufacture"


# Get the current year
current_year <- as.numeric(format(Sys.Date(), "%Y"))

# Calculate the age of each car
region_25$yearsOfUsage <- current_year - region_25$year


# Calculate fuel efficiency (kilometers per liter) using mileage and engine displacement
region_25$fuelEfficiency <- region_25$mileage / region_25$engineDisplacement


# Calculate priceMileageRatio (kilometers per liter) using mileage and engine displacement
region_25$priceMileageRatio <- region_25$priceUSD / region_25$mileage


region_25$listingId <- as.integer(region_25$listingId)



str(region_25)










######################## 5. ANALYSIS #########################

## Extracting the day, month, year  from date 
region_25_cleaned_set$date <- as.Date(region_25_cleaned_set$date)
region_25_cleaned_set$month <- format(as.Date(region_25_cleaned_set$date), "%m")
#region_25_cleaned_set$year1 <- format(as.Date(region_25_cleaned_set$date), "%Y")
region_25_cleaned_set$day <- format(as.Date(region_25_cleaned_set$date), "%A")
region_25_cleaned_set$time <- format(as.POSIXct(region_25_cleaned_set$date), format = "%H:%M:%S")

View(region_25_cleaned_set)
glimpse(region_25_cleaned_set)
str(region_25_cleaned_set)




              ### Mileage ###
## Number of observations in the column: Mileage
nrow(region_25)
nrow(region_25_cleaned_set)

#Range of Mileage
Range<- max(region_25$mileage, na.rm=TRUE) - min(region_25$mileage, na.rm=TRUE); Range
Range<- max(region_25_cleaned_set$mileage) - min(region_25_cleaned_set$mileage); Range
#Also...
range(region_25$mileage, na.rm=TRUE)
range(region_25_cleaned_set$mileage)


#Min and max of the column 
max(region_25$mileage, na.rm=TRUE)
min(region_25$mileage, na.rm=TRUE)

max(region_25_cleaned_set$mileage)
min(region_25_cleaned_set$mileage)

#Mean, standard deviation/variance of the column 
mean(region_25$mileage, na.rm=TRUE)
mean(region_25_cleaned_set$mileage, na.rm=TRUE) #na.rm=TRUE excludes missing values. 

mean(region_25_cleaned_set$mileage, na.rm=TRUE, trim= .05) #Exclude the top and bottom 5% of values. 

#Variance and Standard Deviation of Mileage
var(region_25$mileage, na.rm=TRUE)
sd(region_25$mileage, na.rm=TRUE) 

var(region_25_cleaned_set$mileage)
sd(region_25_cleaned_set$mileage) 

#Mode, median, percentiles (25%, 50%, 75%, 95%) 
median(region_25$mileage, na.rm=TRUE)  
median(region_25_cleaned_set$mileage, na.rm=TRUE)  

region_25_cleaned_set$Mileage <- na.omit(region_25_cleaned_set$Mileage)
sum(is.na(region_25_cleaned_set$Mileage))
View(region_25_cleaned_set)       ## The cleaned set now has 1002096
str(region_25_cleaned_set)



unique_mileage<- unique(region_25$mileage); unique_mileage 
mode<- function(x) {
  ux<- unique(x)
  tab<- tabulate(match(x, ux))
  ux[tab==max(tab)]
}

Mode<- mode(region_25$mileage); Mode
length(which(region_25$mileage==Mode)) #The number of observations recognized as the mode





##Default function: 0%, 25%, 50%, 75%, 100%
quantile(region_25$mileage, na.rm=TRUE) 

quantile(region_25_cleaned_set$mileage) 

##Other Percentiles. For example...
quantile(region_25$mileage, na.rm=TRUE, probs=c(.10, .95)) 
quantile(region_25_cleaned_set$mileage, probs=c(.10, .9))


hist(region_25$mileage,
     breaks=8, #Define different Number of Breaks. 
     freq=T,#freq=F --> hist of rel freq,comment out ylim,ylab,and labels.
     main="Histogram of Mileage",
     xlab="Mileage",
     ylab="Frequency",
     labels=T,#label the values
     xlim=c(1e+03,1e+06),
     ylim=c(0,7e+05),
     col="grey",
     border="red",
     cex=1.2,cex.lab=1.2,cex.axis=1.2,
     las=1,#rotate the value of y-axis
     #las=2,#rotate the value of x-axis
)



            ### POWER ####
## Number of observations in the column: Power
nrow(region_25)

nrow(region_25_cleaned_set)

#Range of Power
Range<- max(region_25$power, na.rm=TRUE) - min(region_25$power, na.rm=TRUE); Range
Range<- max(region_25_cleaned_set$power) - min(region_25_cleaned_set$power); Range
#Also...
range(region_25$power, na.rm=TRUE)
range(region_25_cleaned_set$power)


#Min and max of the column 
max(region_25$power, na.rm=TRUE)
min(region_25$power, na.rm=TRUE)


max(region_25_cleaned_set$power)
min(region_25_cleaned_set$power)


#Mean, standard deviation/variance of the column 
mean(region_25$power, na.rm=TRUE) #na.rm=TRUE excludes missing values. 
mean(region_25_cleaned_set$power, na.rm=TRUE) #na.rm=TRUE excludes missing values. 

mean(region_25_cleaned_set$power, na.rm=TRUE, trim= .05) #Exclude the top and bottom 5% of values. 


#Variance and Standard Deviation of Power
var(region_25$power, na.rm=TRUE)
sd(region_25$power, na.rm=TRUE) 

var(region_25_cleaned_set$power)
sd(region_25_cleaned_set$power) 

#Mode, median, percentiles (25%, 50%, 75%, 95%) 
median(region_25$power, na.rm=TRUE)  

median(region_25_cleaned_set$power, na.rm=TRUE)  



unique_power<- unique(region_25$power); unique_power
mode<- function(x) {
  ux<- unique(x)
  tab<- tabulate(match(x, ux))
  ux[tab==max(tab)]
}

Mode<- mode(region_25$power); Mode
length(which(region_25$power==Mode)) #The number of observations recognized as the mode



##Default function: 0%, 25%, 50%, 75%, 100%
quantile(region_25$power, na.rm=TRUE) 

quantile(region_25_cleaned_set$power) 

##Other Percentiles. For example...
quantile(region_25$power, na.rm=TRUE, probs=c(.10, .95))
quantile(region_25_cleaned_set$power, probs=c(.10, .9))



# Calculate the missing quantity
missing_quantity <- sum(is.na(region_25$power))

# Calculate the total number of observations
total_rows <- length(region_25$power)

# Calculate the percentage of missing values
missing_rate <- (missing_quantity / total_rows) * 100

# Print the results
print(paste("Missing Quantity:", missing_quantity))
print(paste("Missing Rate (%):", missing_rate))






hist(region_25_cleaned_set$power)

hist(region_25$power,
     breaks=8, #Define different Number of Breaks. 
     freq=T,#freq=F --> hist of rel freq,comment out ylim,ylab,and labels.
     main="Histogram of Power",
     xlab="Power",
     ylab="Frequency",
     labels=T,#label the values
     xlim=c(9,1000),
     ylim=c(0,10e+05),
     col="green",
     border="red",
     cex=1.2,cex.lab=1.2,cex.axis=1.2,
     las=1,#rotate the value of y-axis
     #las=2,#rotate the value of x-axis
)







                            ### PRICE ####
## Number of observations in the column: Price
nrow(region_25)
nrow(region_25_cleaned_set)

#Range of Power
Range<- max(region_25$price) - min(region_25$price); Range
Range<- max(region_25_cleaned_set$price) - min(region_25_cleaned_set$price); Range
#Also...
range(region_25$price, na.rm=TRUE)
range(region_25_cleaned_set$price)


#Min and max of the column 
max(region_25$price)
min(region_25$price)

max(region_25_cleaned_set$price)
min(region_25_cleaned_set$price)

#Mean, standard deviation/variance of the column 
mean(region_25$price, na.rm=TRUE) #na.rm=TRUE excludes missing values. 

mean(region_25_cleaned_set$price, na.rm=TRUE) #na.rm=TRUE excludes missing values. 
mean(region_25_cleaned_set$price, na.rm=TRUE, trim= .05) #Exclude the top and bottom 5% of values. 


#Variance and Standard Deviation of price
var(region_25$price, na.rm=TRUE)
sd(region_25$price, na.rm=TRUE) 


var(region_25_cleaned_set$price)
sd(region_25_cleaned_set$price) 

#Mode, median, percentiles (25%, 50%, 75%, 95%) 
median(region_25$price, na.rm=TRUE)  

median(region_25_cleaned_set$price, na.rm=TRUE)  


unique_price<- unique(region_25$price); unique_price
mode<- function(x) {
  ux<- unique(x)
  tab<- tabulate(match(x, ux))
  ux[tab==max(tab)]
}

Mode<- mode(region_25$price); Mode
length(which(region_25$price==Mode)) #The number of observations recognized as the mode




##Default function: 0%, 25%, 50%, 75%, 100%
quantile(region_25$price, na.rm=TRUE) 

quantile(region_25_cleaned_set$price) 

##Other Percentiles. For example...
quantile(region_25$price, na.rm=TRUE, probs=c(.10, .95))
quantile(region_25_cleaned_set$price, probs=c(.10, .9))

hist(region_25_cleaned_set$price)

hist(region_25$price,
     breaks=8, #Define different Number of Breaks. 
     freq=T,#freq=F --> hist of rel freq,comment out ylim,ylab,and labels.
     main="Histogram of Price",
     xlab="Price",
     ylab="Frequency",
     labels=T,#label the values
     xlim=c(15000,41500000),
     ylim=c(0,15e+05),
     col="lightblue",
     border="red",
     cex=1.2,cex.lab=1.2,cex.axis=1.2,
     las=1,#rotate the value of y-axis
     #las=2,#rotate the value of x-axis
)





                              ### Engine Displacement ####
## Number of observations in the column: engineDisplacement
nrow(region_25)
nrow(region_25_cleaned_set)

## Removing the unit "LTR" from engineDisplacement
region_25$engineDisplacement<-gsub(" LTR"," ",as.character(region_25$engineDisplacement))

##Converting the engineDisplacement column into numeric format
region_25 <- mutate(region_25, engineDisplacement = as.numeric(engineDisplacement))

## Verifying the above conversions are done
class(region_25$engineDisplacement)  



#Range of Power
Range<- max(region_25$engineDisplacement, na.rm=TRUE) - min(region_25$engineDisplacement, na.rm=TRUE); Range
Range<- max(region_25_cleaned_set$engineDisplacement) - min(region_25_cleaned_set$engineDisplacement); Range
#Also...
range(region_25$engineDisplacement, na.rm=TRUE)
range(region_25_cleaned_set$engineDisplacement)


#Min and max of the column 
max(region_25_cleaned_set$engineDisplacement, na.rm=TRUE)
min(region_25_cleaned_set$engineDisplacement, na.rm=TRUE)

max(region_25_cleaned_set$engineDisplacement)
min(region_25_cleaned_set$engineDisplacement)


#Mean, standard deviation/variance of the column 
mean(region_25$engineDisplacement, na.rm=TRUE) #na.rm=TRUE excludes missing values. 

mean(region_25_cleaned_set$engineDisplacement, na.rm=TRUE) #na.rm=TRUE excludes missing values.
mean(region_25_cleaned_set$engineDisplacement, na.rm=TRUE, trim= .05) #Exclude the top and bottom 5% of values. 


#Variance and Standard Deviation of engineDisplacement
var(region_25$engineDisplacement, na.rm=TRUE)
sd(region_25$engineDisplacement, na.rm=TRUE) 

var(region_25_cleaned_set$engineDisplacement)
sd(region_25_cleaned_set$engineDisplacement) 

#Mode, median, percentiles (25%, 50%, 75%, 95%) 
median(region_25$engineDisplacement, na.rm=TRUE)  

median(region_25_cleaned_set$engineDisplacement, na.rm=TRUE)  


unique_engineDisplacement<- unique(region_25_cleaned_set$engineDisplacement); unique_engineDisplacement
mode<- function(x) {
  ux<- unique(x)
  tab<- tabulate(match(x, ux))
  ux[tab==max(tab)]
}

Mode<- mode(region_25_cleaned_set$engineDisplacement); Mode
length(which(region_25_cleaned_set$engineDisplacement==Mode)) #The number of observations recognized as the mode



##Default function: 0%, 25%, 50%, 75%, 100%
quantile(region_25$engineDisplacement, na.rm=TRUE) 

quantile(region_25_cleaned_set$engineDisplacement) 

##Other Percentiles. For example...
quantile(region_25$engineDisplacement, na.rm=TRUE, probs=c(.10, .95))
quantile(region_25_cleaned_set$engineDisplacement, probs=c(.10, .9))

hist(region_25_cleaned_set$engineDisplacement)

hist(region_25$engineDisplacement,
     breaks=8, #Define different Number of Breaks. 
     freq=T,#freq=F --> hist of rel freq,comment out ylim,ylab,and labels.
     main="Histogram of engineDisplacement",
     xlab="Engine Displacement",
     ylab="Frequency",
     labels=T,#label the values
     xlim=c(0.5,7),
     ylim=c(0,8e+05),
     col="lightblue",
     border="red",
     cex=1.2,cex.lab=1.2,cex.axis=1.2,
     las=1,#rotate the value of y-axis
     #las=2,#rotate the value of x-axis
)





                            ### Year ####
## Number of observations in the column: year
nrow(region_25_cleaned_set)

#Range of year
Range<- max(region_25$year, na.rm=TRUE) - min(region_25$year, na.rm=TRUE); Range

Range<- max(region_25_cleaned_set_new$year) - min(region_25_cleaned_set_new$year); Range
#Also...
range(region_25$year, na.rm=TRUE)
range(region_25_cleaned_set_new$year)


#Min and max of the column 
max(region_25$year, na.rm=TRUE)
min(region_25$year, na.rm=TRUE)

max(region_25_cleaned_set_new$year)
min(region_25_cleaned_set_new$year)

#Mean, standard deviation/variance of the column
mean(region_25$year, na.rm=TRUE) #na.rm=TRUE excludes missing values. 

mean(region_25_cleaned_set_new$year, na.rm=TRUE) #na.rm=TRUE excludes missing values. 

mean(region_25_cleaned_set_new$year, na.rm=TRUE, trim= .05) #Exclude the top and bottom 5% of values. 


#Variance and Standard Deviation of year
var(region_25$year, na.rm=TRUE)
sd(region_25$year, na.rm=TRUE) 

var(region_25_cleaned_set_new$year)
sd(region_25_cleaned_set_new$year) 

#Mode, median, percentiles (25%, 50%, 75%, 95%) 
median(region_25$year, na.rm=TRUE) 

median(region_25_cleaned_set$year, na.rm=TRUE)  

unique_year<- unique(region_25_cleaned_set_new$year); unique_year
mode<- function(x) {
  ux<- unique(x)
  tab<- tabulate(match(x, ux))
  ux[tab==max(tab)]
}

Mode<- mode(region_25_cleaned_set_new$year); Mode
length(which(region_25_cleaned_set_new$year==Mode)) #The number of observations recognized as the mode



##Default function: 0%, 25%, 50%, 75%, 100%
quantile(region_25$year, na.rm=TRUE) 

quantile(region_25_cleaned_set$year) 

##Other Percentiles. For example...
quantile(region_25$year, na.rm=TRUE, probs=c(.10, .95))
quantile(region_25_cleaned_set$year, probs=c(.10, .9))

hist(region_25_cleaned_set$year)

hist(region_25$year,
     breaks=8, #Define different Number of Breaks. 
     freq=T,#freq=F --> hist of rel freq,comment out ylim,ylab,and labels.
     main="Histogram of Year",
     xlab="Year",
     ylab="Frequency",
     labels=T,#label the values
     xlim=c(1943,2028),
     ylim=c(0,8e+05),
     col="pink",
     border="black",
     cex=1.2,cex.lab=1.2,cex.axis=1.2,
     las=1,#rotate the value of y-axis
     #las=2,#rotate the value of x-axis
)





###############################################################################################
###############################################################################################
             ## Character/Categorical Columns ##
                          ## BRAND ## 
n=nrow(region_25_cleaned_set); n #total number of observations

Freq<- region_25 %>%
  group_by(brand) %>%  
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency)); Freq


df <- data.frame(
  brand = c("Toyota", "Honda", "Nissan", "Lexus", "Subaru", "Mitsubishi", "Suzuki", "Mazda", "Mercedes-Benz", "Daihatsu"),
  Frequency = c(686119, 202379, 192529, 61946, 57536, 56330, 46735, 40878, 31277, 25383)
)

total_obs <- sum(df$Frequency)
df <- transform(df, Percent_Frequency = (Frequency / total_obs) * 100)
df <- transform(df, Cumulative_Frequency = cumsum(Frequency))
df <- transform(df, Cumulative_Percent_Frequency = (Cumulative_Frequency / total_obs) * 100)
print(df)

ggplot(df, aes(x = brand, y = Frequency)) +
  geom_bar(stat = "identity", fill = "red") +
  labs(x = "Brand", y = "Frequency", title = "Histogram of Brand") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



                                ## NAME ## 
n=nrow(region_25_cleaned_set); n #total number of observations

Freq<- region_25_cleaned_set %>%
  group_by(name) %>%  
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency)); Freq

Freq<- region_25 %>%
  group_by(name) %>%  
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency)); Freq


df <- data.frame(
  name = c("Land Cruiser Prado", "Prius", "Fit", "Harrier", "Corolla Fielder", "Note", "Vitz", "Crown", "Vezel", "Land Cruiser"),
  Frequency = c(64258, 54984, 42622, 36849, 30564, 30520, 27794, 27537, 27220, 26855)
)
total_obs <- sum(df$Frequency)
df <- transform(df, Percent_Frequency = (Frequency / total_obs) * 100)
df <- transform(df, Cumulative_Frequency = cumsum(Frequency))
df <- transform(df, Cumulative_Percent_Frequency = (Cumulative_Frequency / total_obs) * 100)
print(df)

ggplot(df, aes(x = name, y = Frequency)) +
  geom_bar(stat = "identity", fill = "darkgoldenrod") +
  labs(x = "Name", y = "Frequency", title = "Histogram of Name") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))







                              ## BODY TYPE ## 
n=nrow(region_25_cleaned_set); n #total number of observations

Freq<- region_25_cleaned_set %>%
  group_by(bodyType) %>%  
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency)); Freq


Freq<- region_25 %>%
  group_by(bodyType) %>%  
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency)); Freq

df <- data.frame(
  bodyType = c("jeep 5 doors", "hatchback 5 doors", "sedan", "minivan", "station wagon", "liftback", "jeep 3 doors", "pickup", "hatchback 3 door", "coupe", "open"),
  Frequency = c(491432, 275234, 252774, 223131, 130249, 73711, 29915, 17040, 9532, 7903, 2279)
)
total_obs <- sum(df$Frequency)
df <- transform(df, Percent_Frequency = (Frequency / total_obs) * 100)
df <- transform(df, Cumulative_Frequency = cumsum(Frequency))
df <- transform(df, Cumulative_Percent_Frequency = (Cumulative_Frequency / total_obs) * 100)
print(df)

ggplot(df, aes(x = bodyType, y = Frequency)) +
  geom_bar(stat = "identity", fill = "darkred") +
  labs(x = "Body Type", y = "Frequency", title = "Histogram of Body Type") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






                              ## COLOR ## 
n=nrow(region_25_cleaned_set); n #total number of observations

Freq<- region_25_cleaned_set %>%
  group_by(color) %>%  
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency)); Freq

Freq<- region_25 %>%
  group_by(color) %>%  
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency)); Freq

df <- data.frame(
  color = c("white", "grey", "black", NA, "silver", "blue", "red", "green", "burgundy", "brown", "violet", "golden", "beige", "yellow", "orange", "pink"),
  Frequency = c(457827, 289259, 285298, 109734, 104259, 101946, 34791, 32268, 26690, 24322, 13460, 11482, 9556, 5632, 3902, 2774)
)
total_obs <- sum(df$Frequency, na.rm = TRUE)
df <- transform(df, Percent_Frequency = (Frequency / total_obs) * 100)
df <- transform(df, Cumulative_Frequency = cumsum(Frequency))
df <- transform(df, Cumulative_Percent_Frequency = (Cumulative_Frequency / total_obs) * 100)
print(df)


ggplot(df, aes(x = color, y = Frequency)) +
  geom_bar(stat = "identity", fill = "darkorchid") +
  labs(x = "Color", y = "Frequency", title = "Histogram of Color")





                          ## FUEL TYPE ## 
n=nrow(region_25_cleaned_set); n #total number of observations

Freq<- region_25_cleaned_set %>%
  group_by(fuelType) %>%  
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency)); Freq

Freq<- region_25 %>%
  group_by(fuelType) %>%  
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency)); Freq

df <- data.frame(
  fuelType = c("Gasoline", "Diesel", "Electro", NA),
  Frequency = c(1358566, 137350, 13724, 3560)
)
total_obs <- sum(df$Frequency, na.rm = TRUE)
df <- transform(df, Percent_Frequency = (Frequency / total_obs) * 100)
df <- transform(df, Cumulative_Frequency = cumsum(Frequency))
df <- transform(df, Cumulative_Percent_Frequency = (Cumulative_Frequency / total_obs) * 100)
print(df)

ggplot(df, aes(x = fuelType, y = Frequency)) +
  geom_bar(stat = "identity", fill = "coral") +
  labs(x = "Fuel Type", y = "Frequency", title = "Histogram of Fuel Type")





                            ## TRANSMISSION ## 
n=nrow(region_25_cleaned_set); n #total number of observations

Freq<- region_25_cleaned_set %>%
  group_by(transmission) %>%  
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency)); Freq

Freq<- region_25 %>%
  group_by(transmission) %>%  
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency)); Freq

df <- data.frame(
  transmission = c("CVT", "AT", "Manual", "Automatic", "Robot", NA),
  Frequency = c(677023, 645355, 73331, 59233, 55193, 3065)
)
total_obs <- sum(df$Frequency, na.rm = TRUE)
df <- transform(df, Percent_Frequency = (Frequency / total_obs) * 100)
df <- transform(df, Cumulative_Frequency = cumsum(Frequency))
df <- transform(df, Cumulative_Percent_Frequency = (Cumulative_Frequency / total_obs) * 100)
print(df)

ggplot(df, aes(x = transmission, y = Frequency)) +
  geom_bar(stat = "identity", fill = "darkcyan") +
  labs(x = "Transmission", y = "Frequency", title = "Histogram of Transmission")




                        ## ENGINE NAME ## 
n=nrow(region_25_cleaned_set); n #total number of observations

Freq<- region_25_cleaned_set %>%
  group_by(engineName) %>%  
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency)); Freq

df <- data.frame(
  engineName = c("2ZR-FXE", "1NZ-FE", "LEB", "2TR-FE", "1NZ-FXE", "1KR-FE", "3ZR-FAE", "2ZR-FAE", "LDA", "HR15DE"),
  Frequency = c(60172, 48167, 33559, 31564, 22544, 22460, 20414, 19834, 16737, 15667)
)

total_obs <- sum(df$Frequency)
df <- transform(df, Percent_Frequency = (Frequency / total_obs) * 100)
df <- transform(df, Cumulative_Frequency = cumsum(Frequency))
df <- transform(df, Cumulative_Percent_Frequency = (Cumulative_Frequency / total_obs) * 100)
print(df)

ggplot(df, aes(x = engineName, y = Frequency)) +
  geom_bar(stat = "identity", fill = "darkolivegreen") +
  labs(x = "Engine Name", y = "Frequency", title = "Histogram of Engine Name")





                            ## LOCATION ## 
n=nrow(region_25_cleaned_set); n #total number of observations

Freq<- region_25_cleaned_set %>%
  group_by(location) %>%  
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency)); Freq

Freq<- region_25 %>%
  group_by(location) %>%  
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency)); Freq

df <- data.frame(
  location = c("Vladivostok", "Ussurijsk", "Nahodka", "Artem", "Arsenev", "Spassk-Dalnij", "Dalnegorsk", "Bolshoj Kamen", "Dalnerechensk", "Partizansk"),
  Frequency = c(909075, 254463, 103972, 68121, 22644, 19926, 15302, 14940, 12533, 11240)
)
total_obs <- sum(df$Frequency)
df <- transform(df, Percent_Frequency = (Frequency / total_obs) * 100)
df <- transform(df, Cumulative_Frequency = cumsum(Frequency))
df <- transform(df, Cumulative_Percent_Frequency = (Cumulative_Frequency / total_obs) * 100)
print(df)

ggplot(df, aes(x = location, y = Frequency)) +
  geom_bar(stat = "identity", fill = "chocolate") +
  labs(x = "Location", y = "Frequency", title = "Histogram of Location")






                    ## VEHICLE CONFIGURATION ## 
n=nrow(region_25_cleaned_set); n #total number of observations

Freq<- region_25_cleaned_set %>%
  group_by(location) %>%  
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency)); Freq

Freq<- region_25 %>%
  group_by(vehicleConfiguration) %>%  
  summarise(Frequency=n()) %>%
  arrange(desc(Frequency)); Freq

# Replace "400 4MATIC Особая серия" with "400 4MATIC Special Edition" in the 'vehicle_configuration' column
region_25_cleaned_set$vehicle_configuration <- gsub("400 4MATIC Особая серия", "400 4MATIC Special Edition", region_25_cleaned_set$vehicle_configuration)


unique_vehicle_config<- unique(region_25_cleaned_set$vehicleConfiguration); unique_vehicle_config
mode<- function(x) {
  ux<- unique(x)
  tab<- tabulate(match(x, ux))
  ux[tab==max(tab)]
}

df <- data.frame(
  vehicleConfiguration = c("1.8 S", "1.5 G", "2.7 TX L Package 7 seat 4WD", "1.5 X 4WD", "1.3 G", "1.3 L", "Hybrid 1.8 G LED Edition", "1.5 X", "2.7 TX 7 seat 4WD"),
  Frequency = c(14739, 11360, 9211, 6474, 6367, 5512, 4934, 4798, 4606)
)

total_obs <- sum(df$Frequency)
df <- transform(df, Percent_Frequency = (Frequency / total_obs) * 100)
df <- transform(df, Cumulative_Frequency = cumsum(Frequency))
df <- transform(df, Cumulative_Percent_Frequency = (Cumulative_Frequency / total_obs) * 100)
print(df)


ggplot(df, aes(x = vehicleConfiguration, y = Frequency)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Vehicle Configuration", y = "Frequency", title = "Histogram of Vehicle Configuration")



# Calculate the missing quantity
missing_quantity <- sum(is.na(region_25$vehicleConfiguration))

# Calculate the total number of observations
total_rows <- length(region_25$vehicleConfiguration)

# Calculate the percentage of missing values
missing_rate <- (missing_quantity / total_rows) * 100

# Print the results
print(paste("Missing Quantity:", missing_quantity))
print(paste("Missing Rate (%):", missing_rate))






## Exporting the data frame to save the csv file in my laptop
write.csv(region_25,file="region_25.csv")

## Exporting the data frame to save the csv file in my laptop
write.csv(region_25_cleaned_set_new,file="region_25_cleaned_set_new.csv")


