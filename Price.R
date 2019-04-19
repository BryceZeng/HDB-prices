#Housing Data Prices
#Date: 10/2/2019
#Changelog:
#10/2 - using resale price and feature only
#15/2 - adding resale price index
#       Rent and Median resale price cannot be use as it is future dependent
#16/2 - Rent and resale index - use for interpolation
####

library(httr);library(jsonlite)
library(lubridate);library(tidyverse)
library(data.table)
library(earth); library(splines)
library(h2o)
library(Metrics);library(pdp)
setwd("C:/Users/Bryce X1/Dropbox/R Scripts")

#Get HDB Resale Price Index
url  <- "https://data.gov.sg/api/action/datastore_search?resource_id=52e93430-01b7-4de0-80df-bc83d0afed40&limit=200"
#&sort_order=desc
raw.result <- GET(url = url)
status_code(raw.result) #200 is valid, 400 is error on their server, 300 is error on your side
raw.result <- rawToChar(raw.result$content)
raw.result <- fromJSON(raw.result)
HDB.resale.index <- raw.result$result$records
HDB.resale.index$year <- as.numeric(str_sub(HDB.resale.index$quarter, start = 1, end = 4))
HDB.resale.index$qtr <- as.numeric(str_sub(HDB.resale.index$quarter, start = 7, end = 7))
HDB.resale.index <- HDB.resale.index[,c(1,4,5)]

#Get HDB Resale Price
url  <- "https://data.gov.sg/api/action/datastore_search?resource_id=1b702208-44bf-4829-b620-4615ee19b57c&limit=80000"
raw.result <- GET(url = url)
status_code(raw.result) #200 is valid, 400 is error on their server, 300 is error on your side
raw.result <- rawToChar(raw.result$content)
raw.result <- fromJSON(raw.result)
HDB.Resale <- raw.result$result$records
HDB.Resale[,c(4,6,8,9)] <- lapply(HDB.Resale[,c(4,6,8,9)], as.numeric)
url  <- "https://data.gov.sg/api/action/datastore_search?resource_id=83b2fc37-ce8c-4df4-968b-370fd818138b&limit=80000"
raw.result <- GET(url = url)
status_code(raw.result) #200 is valid, 400 is error on their server, 300 is error on your side
raw.result <- rawToChar(raw.result$content)
raw.result <- fromJSON(raw.result)
temp <- raw.result$result$records
temp[,c(4,6,8)] <- lapply(temp[,c(4,6,8)], as.numeric)
temp$remaining_lease <- 99-(as.numeric(substr(temp$month,1,4))-temp$lease_commence_date)
HDB.Resale <- rbind(HDB.Resale, temp)
rm(temp)

#Get HDB Features
url  <- "https://data.gov.sg/api/action/datastore_search?resource_id=482bfa14-2977-4035-9c61-c85f871daf4e&limit=80000"
raw.result <- GET(url = url)
status_code(raw.result) #200 is valid, 400 is error on their server, 300 is error on your side
raw.result <- rawToChar(raw.result$content)
raw.result <- fromJSON(raw.result)
HDB.Feature <- raw.result$result$records
# misc features include admin office, childcare centre, education centre, RC

HDB.Feature$`_id` <- paste(HDB.Feature$blk_no, HDB.Feature$street, sep = " ")
HDB.Resale$`_id` <- paste(HDB.Resale$block, HDB.Resale$street_name, sep = " ")

#Joining Resale prices with HDB features
HDB.Resale <- left_join(HDB.Resale, HDB.Feature)
HDB.Resale$year <- str_sub(HDB.Resale$month, start = 1, end = 4)
HDB.Resale[,c(13,18,21,23,26,27,29,31:35,37)] <- lapply(HDB.Resale[,c(13,18,21,23,26,27,29,31:35,37)], as.numeric)
HDB.Resale$age <- HDB.Resale$year - HDB.Resale$year_completed
HDB.Resale$month <- as.numeric(str_sub(HDB.Resale$month, start = 6, end = 7))
HDB.Resale$qtr <- if_else(HDB.Resale$month<=3,1,
                           if_else(HDB.Resale$month<=6,2,
                                   if_else(HDB.Resale$month<=9,3,4)))
#Joining Resale with Resale index
HDB.Resale <- left_join(HDB.Resale, HDB.resale.index)
HDB.Resale$index <- as.numeric(if_else(is.na(HDB.Resale$index), max(HDB.Resale$index, na.rm = T),HDB.Resale$index))
HDB.Resale$flat_type <- if_else(HDB.Resale$flat_type == "MULTI-GENERATION", "EXECUTIVE", HDB.Resale$flat_type)
#Clearing data
rm(url,HDB.resale.index, HDB.Feature, raw.result)
gc()


#Get HDB Rent
url  <- "https://data.gov.sg/api/action/datastore_search?resource_id=6b1ec2ff-7c38-4ce9-9bbb-af865b4d78cb&limit=20000"
raw.result <- GET(url = url)
status_code(raw.result) #200 is valid, 400 is error on their server, 300 is error on your side
raw.result <- rawToChar(raw.result$content)
raw.result <- fromJSON(raw.result)
HDB.Rent <- raw.result$result$records
HDB.Rent$year <- as.numeric(str_sub(HDB.Rent$quarter, start = 1, end = 4))
HDB.Rent$qtr <- as.numeric(str_sub(HDB.Rent$quarter, start = 7, end = 7))
HDB.Rent <- HDB.Rent[,c(1,2,5:7)]
HDB.Rent$median_rent <- as.numeric(HDB.Rent$median_rent)
HDB.Rent$town <- if_else(HDB.Rent$town == "CENTRAL", "CENTRAL AREA", HDB.Rent$town)
HDB.Rent$flat_type <- case_when(
  HDB.Rent$flat_type == "1-RM" ~ "1 ROOM",
  HDB.Rent$flat_type == "2-RM" ~ "2 ROOM",
  HDB.Rent$flat_type == "3-RM" ~ "3 ROOM",
  HDB.Rent$flat_type == "4-RM" ~ "4 ROOM",
  HDB.Rent$flat_type == "5-RM" ~ "5 ROOM",
  HDB.Rent$flat_type == "EXEC" ~ "EXECUTIVE"
)
temp <- aggregate(median_rent~flat_type+year,data = HDB.Rent, FUN = median)
temp.join <- subset(HDB.Rent, is.na(HDB.Rent$median_rent))
temp.join$median_rent <- NULL
temp.join <- left_join(temp.join, temp)
HDB.Rent <- subset(HDB.Rent, !is.na(HDB.Rent$median_rent))
HDB.Rent <- rbind(HDB.Rent, temp.join)
rm(temp, temp.join); gc()
#Joining Resale with Rent
HDB.Resale <- left_join(HDB.Resale, HDB.Rent)
rm(url, HDB.Rent, raw.result)

#renaming cols
setnames(HDB.Resale, old = c("1room_rental","2room_rental","3room_rental","1room_sold","2room_sold","3room_sold","4room_sold","5room_sold"),
         new = c("room1_rental","room2_rental","room3_rental","room1_sold","room2_sold","room3_sold","room4_sold","room5_sold"))

#Get Resale Supply
url  <- "https://data.gov.sg/api/action/datastore_search?resource_id=144dbc6b-6113-4fa4-a583-c471eafce539&limit=500"
raw.result <- GET(url = url)
status_code(raw.result) #200 is valid, 400 is error on their server, 300 is error on your side
raw.result <- rawToChar(raw.result$content)
raw.result <- fromJSON(raw.result)
HDB.Supply <- raw.result$result$records
HDB.Supply$year <- as.numeric(str_sub(HDB.Supply$quarter, start = 1, end = 4))
HDB.Supply$qtr <- as.numeric(str_sub(HDB.Supply$quarter, start = 7, end = 7))
HDB.Supply$flat_type <- case_when(
  HDB.Supply$flat_type == "1-room" ~ "1 ROOM",
  HDB.Supply$flat_type == "2-room" ~ "2 ROOM",
  HDB.Supply$flat_type == "3-room" ~ "3 ROOM",
  HDB.Supply$flat_type == "4-room" ~ "4 ROOM",
  HDB.Supply$flat_type == "5-room" ~ "5 ROOM",
  HDB.Supply$flat_type == "Executive" ~ "EXECUTIVE"
)
HDB.Supply <- HDB.Supply[,c(1,4:6)]
HDB.Resale <- left_join(HDB.Resale, HDB.Supply)
HDB.Resale$no_of_resale_applications <- as.numeric(HDB.Resale$no_of_resale_applications)
rm(url, HDB.Supply, raw.result)

#Complete data - model 1
HDB.Complete <- HDB.Resale[complete.cases(HDB.Resale),]

#Create new data
#HDB.new <- HDB.Resale[0,]
#write.csv(HDB.new, "HDB.input.csv", row.names = F)
HDB.new <- fread("HDB.input.csv")


#Spliting data to train and test
ind <- sample(2, nrow(HDB.Complete),replace = T, prob = c(.85,.15))
test <- HDB.Complete[ind == 2,]
train <- HDB.Complete[ind == 1,]
rm(ind)
gc()



#Common Regression Techniques
color.gradient <- function(x, colors=c("red","yellow","green"), colsteps=100) {
  return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
}

m.linear <- lm(log(resale_price)~ns(floor_area_sqm,3)+ns(remaining_lease,3)+storey_range+town+flat_type+flat_model+log(total_dwelling_units)+max_floor_lvl+precinct_pavilion+market_hawker+multistorey_carpark+commercial+miscellaneous+ns(log(age),2),HDB.Resale )
m.linear <- lm(log(resale_price)~floor_area_sqm+remaining_lease+storey_range+town+flat_model+log(total_dwelling_units)+max_floor_lvl+precinct_pavilion+market_hawker+multistorey_carpark+commercial+miscellaneous+log(age),HDB.Resale )
m.linear <- lm(log(resale_price)~log(floor_area_sqm)+log(remaining_lease)+town+storey_range+flat_model+flat_type+log(total_dwelling_units)+log(max_floor_lvl)+log(age)+precinct_pavilion+market_hawker+multistorey_carpark+commercial+miscellaneous+
                 index+log(median_rent)+room1_sold+room2_sold+room3_sold+room4_sold+room5_sold+log(no_of_resale_applications),train)
m.linear <- step(m.linear, trace = 0)
summary(m.linear)
plot(exp(predict(m.linear,test)),test$resale_price,  col = color.gradient(test$floor_area_sqm))
rmse(test$resale_price, exp(predict(m.linear,test)))
mae(test$resale_price, exp(predict(m.linear,test)))

HDB.new$resale_price <- exp(predict(m.linear,HDB.new))


m.mars <- earth(log(resale_price)~log(floor_area_sqm)+log(remaining_lease)+storey_range+town+flat_type+flat_model+log(total_dwelling_units)+log(max_floor_lvl)+precinct_pavilion+market_hawker+multistorey_carpark+commercial+miscellaneous+
                  log(age)+index+log(median_rent)+room1_sold+room2_sold+room3_sold+room4_sold+room5_sold,train,
                degree = 5,
                pmethod = "cv",
                ncross=3,trace = .5,nfold = 3)
m.mars <- earth(log(resale_price)~ns(log(floor_area_sqm),3)+ns(remaining_lease,3)+storey_range+town+flat_type+flat_model+log(total_dwelling_units)+log(max_floor_lvl)+precinct_pavilion+market_hawker+multistorey_carpark+commercial+miscellaneous+
                  log(age)+index+log(median_rent)+room1_sold+room2_sold+room3_sold+room4_sold+room5_sold,train,
                degree = 5)
summary(m.mars)
plot(predict(m.mars,train),log(train$resale_price), col = color.gradient(train$floor_area_sqm))
earth::evimp(m.mars)
partial(m.mars, "remaining_lease", plot = T)

out <- predict(m.mars,HDB.new)

gc()
h2o.init()
out = h2o.automl(
  x = c(1:4,8,10,16,18,21,23,24,26:36,37,40,41), 
  y = 6,
  training_frame = as.h2o(train), 
  validation_frame = as.h2o(test), 
  max_runtime_secs = 7200 #2h runtime
)

championModel = out@leader
summary(championModel)
t <- as.data.frame(predict(championModel, as.h2o(test)))
test <- cbind(test, t)
rmse(test$resale_price, test$predict)
plot(test$predict, test$resale_price)

h2o.varimp_plot(championModel)
h2o.partialPlot(championModel, as.h2o(train), cols = c("floor_area_sqm","median_rent","age") , plot = T)
