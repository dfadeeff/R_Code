getwd()
setwd("/home/bi_user/Automation/Reports/CRM")

rm(list = ls(all=TRUE))
#Provide new paths to libraries
.libPaths("/home/bi_user/R/x86_64-pc-linux-gnu-library/3.3/")

####################################################################################
#Learn how to copy files from one to another folder
####################################################################################

#Set the folders
from_folder <- "/home/bi_user/powerbi-share/Xchange_rate"
to_folder <- "/home/bi_user/Automation/Reports/CRM/"

#Identify files
list_files <- list.files(from_folder,"exchange_rates.csv",full.names = T)

#Copy the files
file.copy(list_files,to=to_folder,overwrite = T)

exchange_rate <- read.csv("exchange_rates.csv",header = T,sep = ";")
exchange_rate <- subset(exchange_rate, exchange_rate$currency_code=="GBP")
exchange_rate <- exchange_rate[,c(1:3)]
exchange_rate$exchange_rate_date <- as.Date(exchange_rate$exchange_rate_date, "%Y-%m-%d")


####################################################################################
#Load the collections
####################################################################################

#Load library
library(lubridate)
library(zoo)
library(dplyr)
library(mongolite)
library(jsonlite)
library(ggplot2)
library(data.table)
library(stringr)


#Open connection
collection = c("intwash_orders", "intwash_customers","intwash_invoices","intwash_voucher_redemptions")

orders <- mongo(collection = collection[1], db = "uk_live", 
                url = "mongodb://172.31.51.215:27017",verbose = TRUE)
customers <- mongo(collection = collection[2], db="uk_live",
                   url = "mongodb://172.31.51.215:27017",verbose = TRUE )
invoices <- mongo(collection = collection[3], db="uk_live",
                  url = "mongodb://172.31.51.215:27017",verbose = TRUE )
#vouchers <- mongo(collection = collection[4], db="uk_live",
#                  url = "mongodb://172.31.51.215:27017",verbose = TRUE )

#Parse thru collection and find the keys 
orders_table <- orders$find(fields = '{"_id":1,"invoice":1,"reference":1,"createdAt":1,
                           "customer":1,"voucherCode":1,"state":1,"locationIdentifier":1,"chosenServiceClass.reference":1,
                            "languageCode":1,"createdBy.userAgent":1}')

customers_table <- customers$find(fields = '{"reference":1,"_id":1,
                        "internalData.segmentation":1,"person.name":1,"userAccount.email":1,
                        "internalData.gender":1,"internalData.basketGender":1,
                        "addresses.addressLine":1,"addresses.addressLine2":1,
                        "addresses.zip":1,"phones.number":1,"phones.type":1,"meta.tags":1,"meta.language":1}')

invoices <- invoices$find(fields = '{"_id":1,"grossTotal":1,"grossTotalWithoutDiscounts":1,"netTotal":1,"netTotalWithoutDiscounts":1}')

#vouchers <- vouchers$find(fields = '{"_id":1, "campaignId":1, "blockId":1,
#                          "modelId":1, "code":1, "customerReference":1, "reference":1}')



#Flatten customers table and orders_table
customers_table <- flatten(customers_table)
orders_table <- flatten(orders_table)


#Subset and rename customers
customers <- customers_table[,c(1,8,3)]
colnames(customers) <- c("customer","segmentation","reference")
#Subset orders

orders_data <- orders_table[,c("_id","customer","state","reference","locationIdentifier","invoice","createdAt")]
orders_data <- merge(x=orders_data,y=customers,by="customer",all.x = TRUE)

names(orders_data)
orders_data <- orders_data[,c("reference.y","_id","state","reference.x","createdAt","locationIdentifier","invoice","segmentation")]
colnames(orders_data) <- c("customer","id","state","order","createdAt","location","invoice","segmentation")

#Rename the field to merge
colnames(invoices) <- c("invoice","NetTotalAfterDiscount","NetTotalBeforeDiscount",
                        "GrossTotalAfterDiscount","GrossTotalBeforeDiscount")

orders_data <- merge(x=orders_data,y=invoices,by="invoice",all.x = T)

####################################################################################
#Merging with the X rates
####################################################################################
orders_data$exchange_rate_date <- as.Date(orders_data$createdAt,"%Y-%m-%d") 
orders_data <- merge(x=orders_data,y=exchange_rate,by="exchange_rate_date",all.x = T)

####################################################################################
#Optimize the data frame
####################################################################################
unique(orders_data$location)
orders_data$NetTotalAfterDiscount <- ifelse(orders_data$location=="gb_london",
                                            orders_data$NetTotalAfterDiscount/orders_data$exchange_rate_value,
                                            orders_data$NetTotalAfterDiscount)
orders_data$NetTotalBeforeDiscount <- ifelse(orders_data$location=="gb_london",
                                            orders_data$NetTotalBeforeDiscount/orders_data$exchange_rate_value,
                                            orders_data$NetTotalBeforeDiscount)
orders_data$GrossTotalAfterDiscount <- ifelse(orders_data$location=="gb_london",
                                            orders_data$GrossTotalAfterDiscount/orders_data$exchange_rate_value,
                                            orders_data$GrossTotalAfterDiscount)
orders_data$GrossTotalBeforeDiscount <- ifelse(orders_data$location=="gb_london",
                                            orders_data$GrossTotalBeforeDiscount/orders_data$exchange_rate_value,
                                            orders_data$GrossTotalBeforeDiscount)

#Add isvalid column (first, initialize default value, then, adjust by particular state)
orders_data$isvalid = 1 #initialize isvalid
orders_data$isvalid[which(orders_data$state %in% c("new","payment_authorisation_error",
                                 "canceled","reserved"))] = 0 


#Now get the subset
df <- orders_data[,c(3,5:8,13,16)]

df <- data.table(df)
df <- df[isvalid==1 & !is.na(GrossTotalBeforeDiscount)]


#Subset data.table
mydata <- df[,.(df$customer,df$GrossTotalBeforeDiscount, df$createdAt,df$isvalid)]

colnames(mydata) <- c('customer_id','purchase_amount','date_of_purchase','isvalid')
mydata$date_of_purchase = as.Date(mydata$date_of_purchase,format='%Y-%m-%d')


#Here the dataset grouped by customer is created
MinMax <- mydata[,.(min(date_of_purchase),max(date_of_purchase),sum(isvalid),mean(purchase_amount)),by=.(customer_id)] 

colnames(MinMax) <- c("customer_id","firstOrder","lastOrder","frequency","amount")


MinMax$recency = as.numeric(difftime(time1 = Sys.Date(),
                                        time2 = MinMax$lastOrder,
                                        units = 'days'))

MinMax <- MinMax[,.(customer_id,recency,frequency,amount)]

#Check the overall numbers
sum(MinMax$frequency)
length(unique(MinMax$customer_id))


####################################################################################
#Assign the ratings to the data table
####################################################################################

#hist(MinMax$recency)
#hist(MinMax$frequency, xlim = c(0,30),breaks = 90)
#hist(MinMax$amount, xlim = c(0,100), breaks = 200)


#Create new data frame, copying customers
new_data <- MinMax

#Now assign points
RecencyPoints <- seq(100,20,-20)
RecencyWeight <- 0.4

FrequencyPoints <- seq(20,100,20)
FrequencyWeight <- 0.3

MonetaryPoints <- seq(20,100,20)
MonetaryWeight <- 0.3





#Method to cut the new_date table

#rankR 1 is very recent while rankR 5 is least recent
breaksRec = c(quantile(new_data$recency, probs = c(0,0.1,0.2,0.3,0.5,1)))
labelsRecencyOneFive <- factor(5:1)
new_data$rankR <- labelsRecencyOneFive[findInterval(new_data$recency,breaksRec,rightmost.closed = T)]

labelsRecencyPoints <- factor(RecencyPoints*RecencyWeight)
new_data$rankRecencyPoints = as.numeric(paste(labelsRecencyPoints[findInterval(new_data$recency, breaksRec,rightmost.closed = T)]))
#Check thresholds
c(quantile(new_data$recency, probs = c(0,0.1,0.2,0.3,0.5,1)))

#rankF 1 is least frequent while rankF 5 is most frequent
breaksFreq = c(quantile(new_data$frequency, probs = c(0,0.55,0.7,0.85,0.95,1)))
labelsFrequencyOneFive <- factor(1:5)
new_data$rankF <- labelsFrequencyOneFive[findInterval(new_data$frequency,breaksFreq,rightmost.closed = T)]

labelsFrequencyPoints <- factor(FrequencyPoints*FrequencyWeight)
new_data$rankFrequencyPoints = as.numeric(paste(labelsFrequencyPoints[findInterval(new_data$frequency,breaksFreq,rightmost.closed = T)]))
#Check thresholds
c(quantile(new_data$frequency, probs = c(0,0.55,0.7,0.85,0.95,1)))


#rankM 1 is lowest sales while rankM 5 is highest sales
#new_data$rankM = cut(new_data$amount, 5, labels = F)
breaksMon = c(quantile(new_data$amount, probs = c(0,0.3,0.5,0.7,0.9,1)))
labelsMonetaryOneFice <- factor(1:5)
new_data$rankM = labelsMonetaryOneFice[findInterval(new_data$amount,breaksMon,rightmost.closed = T)]

labelsMonetaryPoints <- factor(MonetaryPoints*MonetaryWeight)
new_data$rankMonetaryPoints = as.numeric(paste(labelsMonetaryPoints[findInterval(new_data$amount,breaksMon,rightmost.closed = T)]))
#Check thresholds
c(quantile(new_data$amount, probs = c(0,0.3,0.5,0.7,0.9,1)))


attach(new_data)
new_data$sum <- rankMonetaryPoints + rankRecencyPoints + rankFrequencyPoints
new_data <- new_data[order(new_data$sum,decreasing=TRUE),]

#Now codify the customers
finalbreakpoints <- seq(min(new_data$sum),max(new_data$sum),length.out = 6)
finalbreakpoints
new_data$class = findInterval(new_data$sum,finalbreakpoints,rightmost.closed = T)


#Assign names
library(plyr)
new_data$classfactor <- as.factor(new_data$class)
new_data$names <- revalue(new_data$classfactor, c("5"="Loyal","4"="Active","3"="Prospective","2"="Dormant","1"="Churned"))
table(new_data$names)



#Transform the data
#new_data = transform(new_data,score=interaction(new_data$rankR,new_data$rankF,new_data$rankM, sep = ''))


#Add cities to new_data
#colnames(new_data)[1] <- c("customer_id")
#new_data <- merge(x = new_data, y = orders_data[,c("customer", "location")], by = 'customer', all.x = T)

#Create city df
orders_data <- data.table(orders_data)
city <- orders_data[,.(min(location)), by = .(customer)]

colnames(city) <- c("customer_id", "location")

names(city)
names(new_data)
#Merge with new_data
new_data <- merge(x = new_data, y = city, by = "customer_id", all.x = T)



#Write to a specific folder that is shared with another machine/server

write.csv(new_data, file="/home/bi_user/powerbi-share/R_outputs/RFM.csv",row.names = FALSE)

save(customers_table,orders_table,new_data, file = "/home/bi_user/Automation/Reports/CRM/DataToCRM.dat")
save(customers_table,orders_table,new_data, file = "/home/bi_user/Automation/Reports/Cohorts/DataToCRM.dat")

save(orders_data, file = "/home/bi_user/Automation/Reports/Cohorts/DataToCohorts.dat" )
save(orders_data, file = "/home/bi_user/Automation/Reports/Stages/DataToCohorts.dat" )
write.csv(orders_data, file = "/home/bi_user/powerbi-share/Python_inputs/orders_data.csv",row.names = FALSE)

