###############################################################################
#####Analysis of customers
###############################################################################

#Clean data
rm(list = ls(all=TRUE))
getwd()
#Set working directories
setwd("/home/dima/Automation/Reports/Stages")

#Load library
library(lubridate)
library(zoo)
library(dplyr)
library(mongolite)
library(jsonlite)
library(ggplot2)
library(data.table)


#Open connection
collection = c("intwash_orders", "intwash_customers")

orders <- mongo(collection = collection[1], db = "uk_live", 
                url = "mongodb://172.31.51.215:27017",verbose = TRUE)
customers <- mongo(collection = collection[2], db="uk_live",
                   url = "mongodb://172.31.51.215:27017",verbose = TRUE )


#Parse thru collection and find the keys 
orders_data <- orders$find(fields = '{"_id":1, "reference":1,"createdAt":1,"customer":1,"state":1,"locationIdentifier":1}')
length(unique(orders_data$reference))
customers <- customers$find(fields = '{"_id":1,"reference":1,"internalData.segmentation":1}')
length(unique(customers$reference))

#Flatten and reshuffle columns
orders_data <- flatten(orders_data)
orders_data <- orders_data[,c(1,3,4,2,5,6)]

#Flatten and reshuffle columns, rename
customers <- flatten(customers)
customers <- customers[,c(1,3,2)]
colnames(customers) <- c("customer","segmentation","reference")


orders_data <- merge(x=orders_data,y=customers,by="customer",all.x = TRUE)
orders_data <- orders_data[,c(8,3:7)]
colnames(orders_data) <- c("customer","order","createdAt","state","location", "segmentation")

#Check classes
str(orders_data)

#Change datatype
orders_data$createdAt <- as.Date(orders_data$createdAt, "%Y-%m-%d")
orders_data <- flatten(orders_data)
orders_data <- data.table(orders_data)

#Add isvalid column (first, initialize default value, then, adjust by particular state)
orders_data$isvalid = 1 #initialize isvalid
orders_data$isvalid[which(orders_data$state %in% c("new","payment_authorisation_error",
                                                   "canceled","reserved"))] = 0 #adjust

#Create function for handling dates
som <- function(x) {
  as.Date(format(x,"%Y-%m-01"))
}

#Create dates to calculate first date of current month and first date of last month
startThisMonth <- som(som(Sys.Date()))
startLastMonth <- som(som(Sys.Date())-1)

########################################################################
#Now I take 2 datasets, sliced by startThisMonth and startLastMonth
########################################################################

orders_dataBeforeLastMonth <- subset(orders_data, orders_data$createdAt <= startLastMonth)
orders_dataLastMonth <- subset(orders_data,
                               orders_data$createdAt > startLastMonth & 
                               orders_data$createdAt <= startThisMonth)



########################################################################
#Now focus on those before last month
########################################################################
MinMaxBeforeLastMonth <- orders_dataBeforeLastMonth[isvalid == 1,.(min(createdAt),max(createdAt),sum(isvalid)),by=.(customer)] 
colnames(MinMaxBeforeLastMonth) <- c("customer","firstOrder","lastOrder","TotalValidOrders")

########################################################################
#Create a variable with global average frequency for that subsetfor returning customers irrespective of segments
########################################################################
AverageBeforeLastMonth <- MinMaxBeforeLastMonth[TotalValidOrders>1,.((lastOrder-firstOrder)/(TotalValidOrders-1))]
colnames(AverageBeforeLastMonth) <- c("average_frequencyBeforeLastMonth")
AverageBeforeLastMonth$average_frequencyBeforeLastMonth <- as.numeric(AverageBeforeLastMonth$average_frequencyBeforeLastMonth)
MedianBeforeLastMonth <- median(AverageBeforeLastMonth$average_frequencyBeforeLastMonth)


#Define individual frequency
MinMaxBeforeLastMonth$average_freq <- MinMaxBeforeLastMonth[,.((lastOrder-firstOrder)/(TotalValidOrders-1))] #individual
MinMaxBeforeLastMonth$average_freq <- as.numeric(MinMaxBeforeLastMonth$average_freq)
MinMaxBeforeLastMonth$average_freq[which(is.nan(MinMaxBeforeLastMonth$average_freq))] = MedianBeforeLastMonth

#Define recency
MinMaxBeforeLastMonth$days_since = as.numeric(difftime(time1 = startLastMonth,
                                           time2 = MinMaxBeforeLastMonth$lastOrder,
                                           units = 'days'))

#Create stages
MinMaxBeforeLastMonth$stages = "NA"
MinMaxBeforeLastMonth$stages[which(MinMaxBeforeLastMonth$TotalValidOrders==1 & MinMaxBeforeLastMonth$days_since < MinMaxBeforeLastMonth$average_freq)] <- 'Stage 1A'
MinMaxBeforeLastMonth$stages[which(MinMaxBeforeLastMonth$TotalValidOrders==2 & MinMaxBeforeLastMonth$days_since < MinMaxBeforeLastMonth$average_freq)] <- 'Stage 1B'
MinMaxBeforeLastMonth$stages[which(MinMaxBeforeLastMonth$TotalValidOrders > 2 & MinMaxBeforeLastMonth$days_since < MinMaxBeforeLastMonth$average_freq)] <- 'Stage 2'
MinMaxBeforeLastMonth$stages[which(MinMaxBeforeLastMonth$stages=="NA")] <- 'Stage 3'

prop.table(table(MinMaxBeforeLastMonth$stages))


########################################################################
#Now focus on those who acted in the last month, group the dt after the cut date
########################################################################
MinMaxLastMonth <- orders_dataLastMonth[isvalid == 1,.(min(createdAt),max(createdAt),sum(isvalid)),by=.(customer)] 
colnames(MinMaxLastMonth) <- c("customer","firstOrder","lastOrder","TotalValidOrders")

CombinedDT <- merge(MinMaxBeforeLastMonth,MinMaxLastMonth,by="customer",all.x = T)

#Check non NA entries
length(which(!is.na(CombinedDT$firstOrder.y)))

colnames(CombinedDT) <- c("customer","firstOrder","lastOrderBeforeCut","OrdersBeforeCut",
                          "AverageFrequencyBeforeCut","DaysSinceBeforeCut",
                          "FormerStages","firstOrderAfterCut","lastAfterCut","OrdersAfterCut")

CombinedDT$OrdersAfterCut[which(is.na(CombinedDT$OrdersAfterCut))] = 0
CombinedDT$TotalOrders <- CombinedDT$OrdersBeforeCut+CombinedDT$OrdersAfterCut


#Define recency with AfterCut threshold
CombinedDT$lastAfterCut[(is.na(CombinedDT$lastAfterCut))] <- CombinedDT$lastOrderBeforeCut[(is.na(CombinedDT$lastAfterCut))]
CombinedDT$DaysSinceAfterCut = as.numeric(difftime(time1 = startThisMonth,
                                                       time2 = CombinedDT$lastAfterCut,
                                                       units = 'days'))

#Define individual frequency
CombinedDT$AverageFrequencyAfterCut <- CombinedDT[,.((lastAfterCut - firstOrder)/( TotalOrders - 1))] #individual
CombinedDT$AverageFrequencyAfterCut <- as.numeric(CombinedDT$AverageFrequencyAfterCut)
CombinedDT$AverageFrequencyAfterCut[which(is.na(CombinedDT$AverageFrequencyAfterCut)) ] = MedianBeforeLastMonth
CombinedDT$AverageFrequencyAfterCut <- as.numeric(CombinedDT$AverageFrequencyAfterCut)


#Create stages
CombinedDT$NewStages = "NA"
CombinedDT$NewStages[which(CombinedDT$TotalOrders == 1 & CombinedDT$DaysSinceAfterCut < CombinedDT$AverageFrequencyAfterCut)] <- 'Stage 1A'
CombinedDT$NewStages[which(CombinedDT$TotalOrders == 2 & CombinedDT$DaysSinceAfterCut < CombinedDT$AverageFrequencyAfterCut)] <- 'Stage 1B'
CombinedDT$NewStages[which(CombinedDT$TotalOrders > 2 & CombinedDT$DaysSinceAfterCut < CombinedDT$AverageFrequencyAfterCut)] <- 'Stage 2'
CombinedDT$NewStages[which(CombinedDT$NewStages=="NA")] <- 'Stage 3'

#Check the distribution of the feature
prop.table(table(CombinedDT$NewStages))


#Create transition matrix
#Vertical col shows where they were, horizontal col shows where they went to 
transition = table(CombinedDT$FormerStages, CombinedDT$NewStages)
#Print transition
transition


x = prop.table(transition,1)*100 #percentages by row
#prop.table(transition,2) #percentrage by column
rownames(x) = paste(rownames(x),"Existing",sep = "In")
colnames(x) = paste(colnames(x),"New",sep = "In")


write.csv(CombinedDT,"/home/dima/powerbi-share/R_outputs/transition.csv",row.names = F)

#Sum of all customers
sum(transition)


