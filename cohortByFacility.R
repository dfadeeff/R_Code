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

#Make a subset
df_subset <- df[,c("customer","state","createdAt","name")]
#Make a data table
df_subset <- data.table(df_subset)

df_subset$isvalid = 1 #initialize isvalid
df_subset$isvalid[which(df_subset$state %in% c("new","payment_authorisation_error",
                                                   "canceled","reserved"))] = 0 #adjust

firstOrder <- df_subset[isvalid==1,min(createdAt),by=.(customer)]
#Merge back
df_subset <- merge(x = df_subset, y = firstOrder, by="customer",all.x = T)

df_subset$createdAt <- as.Date(df_subset$createdAt,"%Y-%m-%d")

df_subset$createdAt <- as.Date(df_subset$createdAt, "%Y-%m-%d")
colnames(df_subset)[names(df_subset)=="V1"] <- "firstOrder"
df_subset$firstOrder <- as.Date(df_subset$firstOrder, "%Y-%m-%d")

df_subset$customerStatus = 0
df_subset$customerStatus[which(df_subset$createdAt>df_subset$firstOrder)] <- 1


df_subset$sinceFirstOrder <- 12 * as.numeric((as.yearmon(df_subset$createdAt)-as.yearmon(df_subset$firstOrder)))
df_subset$initialcohort <-as.yearmon(df_subset$firstOrder)

#Make a subset of only completed orders
nsubset <- df_subset[df_subset$state == 'completed']

#Take the first facility
firstFacility <- nsubset[customerStatus==0,min(name),by=.(customer)]
colnames(firstFacility)[names(firstFacility)=='V1'] <- "firstFacility"

#Check uniqueness
length(unique(firstFacility$customer))


#Merge back to nsubset
nsubset <- merge(x = nsubset, y = firstFacility, by="customer", all.x = T)


y = arrange(nsubset[customerStatus==0, .(length(unique(customer))), by=.(initialcohort,firstFacility)],desc(initialcohort))
x = arrange(nsubset[customerStatus==1, .(length(unique(customer))), 
                    by=.(initialcohort, sinceFirstOrder,firstFacility)],desc(sinceFirstOrder))

colnames(y) <- c("initialcohort","firstFacility","cohort")
colnames(x) <- c("initialcohort","sinceFirstOrder","firstFacility","returning")
class(x)
class(y)

z = merge(x = x, y = y, by = c("initialcohort","firstFacility"),all.x = T)
z$cohort[is.na(z$cohort)] <- 0

#Check NA in z
sum(is.na(z$cohort))


write.csv(z, file = "/home/dima/powerbi-share/R_outputs/facilitycohorts.csv",row.names = FALSE)



