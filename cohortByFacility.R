rm(list = ls(all=TRUE))
getwd()
setwd("/home/dima/Automation/Reports/Cohorts")

library(zoo)
library(mongolite)
library(sqldf)
library(data.table)
library(dplyr)
library(DT)
library(plyr)


#Load collections
collections = c("intwash_facilities", "intwash_orders_subprocesses",
                 "intwash_orders","intwash_driver_deliveries")

#Fetch collections
#Loading facility
facility = mongo(collection = collections[1], db = "uk_live", 
                 url = "mongodb://172.31.51.215:27017",verbose = TRUE)
df_facility = facility$find(fields = '{"_id":1,"name":1}')

subprocesses = mongo(collection = collections[2], db = "uk_live", 
             url = "mongodb://172.31.51.215:27017",verbose = TRUE)
df_subprocesses = subprocesses$find(fields = '{"_id":1,"fulfillment":1,"dropOff":1 }')

orders = mongo(collection = collections[3], db = "uk_live", 
                     url = "mongodb://172.31.51.215:27017",verbose = TRUE)
df_orders = orders$find(fields = '{"_id":1,"reference":1,"subProcesses":1,"customer":1,"createdAt":1,"state":1}')

deliveries = mongo(collection = collections[4], db = "uk_live", 
                    url = "mongodb://172.31.51.215:27017",verbose = TRUE)
df_deliveries = deliveries$find(fields = '{"_id":1, "facility":1}')



#Start with orders
str(df_orders) #I have to unlist it
df <- df_orders[,c("customer","state","reference","createdAt")]
temp <- df_orders[,c("reference","subProcesses")]

#Convert temp to data table to unlist it efficiently
class(temp)
temp <- as.data.table(temp)[,unlist(subProcesses),by=reference]
str(temp)
colnames(temp)[names(temp)=="V1"] <- "subProcesses"

#Merge back to orders
df <- merge(x = df, y = temp, by="reference",all.x = T)

#Rename to be able to merge
colnames(df_subprocesses) <- c("subProcesses","fulfillment","dropOff")

df <- merge(x = df, y = df_subprocesses, by="subProcesses", all.x = T)

colnames(df_deliveries)<- c("dropOff","facility")

df <- merge(x = df, y = df_deliveries, by = "dropOff", all.x = T)

colnames(df_facility) <- c("facility","name")

df <- merge(x = df, y = df_facility, by = "facility", all.x = T)


