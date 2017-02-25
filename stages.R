
getwd()
setwd("/home/dima/Automation/Reports/Stages")
rm(list = ls(all=T))


library(mongolite)
library(jsonlite)
library(ggplot2)
library(data.table)

collection = c("intwash_orders", "intwash_customers")

#Load orders
orders <- mongo(collection = collection[1], db = "uk_live", 
                url = "mongodb://172.31.51.215:27017",verbose = TRUE)
customers <- mongo(collection = collection[2], db="uk_live",
                   url = "mongodb://172.31.51.215:27017",verbose = TRUE )

#Parse thru collection and find the keys 
orders_data <- orders$find(fields = '{"_id":1, "reference":1,"createdAt":1,"customer":1,"state":1,"locationIdentifier":1}')
customers <- customers$find(fields = '{"_id":1,"reference":1,"internalData.segmentation":1}')
colnames(customers) <- c("customer","segmentation","reference")
orders_data <- merge(x=orders_data,y=customers,by="customer",all.x = TRUE)

#Reshuffle and subset orders_data
orders_data <- orders_data[,c("reference.y","state","reference.x","createdAt","locationIdentifier","segmentation")]
colnames(orders_data) <- c("customer","state","order","createdAt","location", "segmentation")

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

#Main function to get BOTH min and max order date and sum(isvalid), SELECT min, max where isvalid = 1 group by customer
minmax <- orders_data[isvalid == 1, .(min(createdAt),max(createdAt),sum(isvalid)),by=.(customer,segmentation.segmentation,location)] 
colnames(minmax) <- c("customer","segment","location","firstOrder","lastOrder","totalValidOrders")


#Check the number of orders
sum(minmax$totalValidOrders)
sum(orders_data$isvalid)


#Create a variable with global average frequency for returning customers irrespective of segments
global_average <- minmax[totalValidOrders>1,.((lastOrder-firstOrder)/(totalValidOrders-1))]
colnames(global_average) <- c("average_frequency")
global_average$average_frequency <- as.numeric(global_average$average_frequency)
average_frequency <- median(global_average$average_frequency)

#Create a variable with global average frequency for returning customers by segments
# DONT FORGET HOW to use SELECT HERE, parenthesis before function!
# .( mean () )
segment_average <- minmax[totalValidOrders>1 & !is.na(segment),
                          .(mean((lastOrder-firstOrder)/(totalValidOrders-1))),
                          by=segment]

colnames(segment_average) <- c("segment","mean")
segment_average$mean <- as.numeric(segment_average$mean)
mean(segment_average$mean)


# Paste thos figures into the initial dataset
minmax <- merge(x = minmax, y = segment_average, by="segment", all.x = T)

minmax$average_freq <- minmax[,.((lastOrder-firstOrder)/(totalValidOrders-1))] #individual
minmax$average_freq <- as.numeric(minmax$average_freq)


#Here I fill in the frequency of orders
minmax$frequency <- minmax$average_freq
minmax$frequency[is.nan(minmax$frequency) & !is.na(minmax$segment)] = minmax$mean[is.nan(minmax$frequency) & !is.na(minmax$segment)]
minmax$frequency[which(is.na(minmax$frequency) & is.na(minmax$segment))] = average_frequency
minmax$frequency[which(is.na(minmax$frequency))] = average_frequency
minmax$frequency <- as.numeric(minmax$frequency)  
median(minmax$frequency)

#Create a var for since last order 
minmax$sinceLastOrder <- difftime(time1 = Sys.Date(),
                                  time2 = minmax$lastOrder,
                                  units = "days")


#Create stages
minmax$stages = "NA"
minmax$stages[which(minmax$totalValidOrders==1 & minmax$sinceLastOrder < minmax$frequency)] <- 'Stage 1A'
minmax$stages[which(minmax$totalValidOrders==2 & minmax$sinceLastOrder < minmax$frequency)] <- 'Stage 1B'
minmax$stages[which(minmax$totalValidOrders>2 & minmax$sinceLastOrder < minmax$frequency)] <- 'Stage 2'
minmax$stages[which(minmax$stages=="NA")] <- 'Stage 3'

#Summarize
prop.table(table(minmax$stages))


#Visualize
#Barplot
#bp <- ggplot(minmax, aes(x=minmax$stages, fill=minmax$stages))+geom_bar(width = 1)
#bp



minmax$lifespan <- difftime(time1 = minmax$lastOrder,
                            time2 = minmax$firstOrder,
                            units = 'days')

write.csv(minmax,"/home/dima/powerbi-share/R_outputs/stages.csv",row.names = F)



