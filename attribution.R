#Set wd
getwd()
setwd("/home/dima/Automation/Reports/Cohorts")

rm(list = ls(all=TRUE))

#Load libraries
library(mongolite)
library(zoo)
library(DT)
library(data.table)
library(dplyr)
library(DT)
library(plyr)
library(htmlwidgets)

#Load data 
load("DataToCRM.dat")

#Set the folders
from_folder <- "/home/dima/powerbi-share/Attribution"
to_folder <- "/home/dima/Automation/Reports/Cohorts/"

#Identify files
list_files <- list.files(from_folder,"output.csv",full.names = T)

#Copy the files
file.copy(list_files,to = to_folder,overwrite = T)

funnel <- read.csv("output.csv",header = T,sep = ";")
funnel <- funnel[,c("order_reference","first_channel","last_channel")]
colnames(funnel)[1] <- c("reference")

orders <- orders_table[,c("customer","voucherCode","reference","createdAt",
                          "state","locationIdentifier","createdBy.userAgent")]

#Load voucher channels
collections = c("intwash_voucher_blocks","intwash_voucher_campaigns",
                "intwash_voucher_items","intwash_voucher_models")

blocks <- mongo(collection = collections[1], db = "uk_live", 
                url = "mongodb://172.31.51.215:27017",verbose = TRUE)
#campaigns <- mongo(collection = collections[2], db="uk_live",
#                   url = "mongodb://172.31.51.215:27017",verbose = TRUE )
items <- mongo(collection = collections[3], db = "uk_live", 
                url = "mongodb://172.31.51.215:27017",verbose = TRUE)
models <- mongo(collection = collections[4], db="uk_live",
                   url = "mongodb://172.31.51.215:27017",verbose = TRUE )

df_blocks <- blocks$find(fields = '{"campaignId":1,"reference":1,"name":1}')
#df_campaigns <- campaigns$find(fields = '{"name":1, "reference":1}')
df_items <- items$find(fields = '{"_id":1,"code":1,"modelId":1}')
df_models <- models$find(fields = '{"_id":1,"blockId":1,"template":1}')
 
#Get the channel name from blocks
df_blocks$channel <- sub('-.*',"",df_blocks$name)
#Rename to join
colnames(df_models)[names(df_models)=="blockId"] <- "reference"
df_models <- merge(x = df_models, y = df_blocks[,c("reference","channel")], by = "reference", all.x = T)
colnames(df_models)[names(df_models)=="_id"] <- "modelId"
df_items <- merge(x = df_items, y = df_models[,c("modelId","channel")], by = "modelId", all.x = T)

colnames(df_items)[names(df_items) == "code"] <- 'voucherCode'
unique(df_items$channel)

#Now I have to restructure, group and rename the channels 
df_items$final_channel <- "NA"

crm_campaign = "Campaign"
df_items$final_channel[which(sapply(crm_campaign, function(x) grepl(x,df_items$channel))==TRUE | df_items$channel == "CRM")] <- "CRM-Campaign"
crm_journey = "Journey"
df_items$final_channel[which(sapply(crm_journey, function(x) grepl(x,df_items$channel))==TRUE)] <- "CRM-Journey"
cooperations = c("PR","Cooperations","Airbnb","Enjoy")
df_items$final_channel[which(sapply(paste(cooperations,collapse = "|"), function(x) grepl(x,df_items$channel))==TRUE)] <- "Cooperations"
customer_care = c("Apology","CC","Thank you","Survey","Thank")
df_items$final_channel[which(sapply(paste(customer_care,collapse = "|"), function(x) grepl(x,df_items$channel))==TRUE)] <- "CC"
flyering = c("Free","Helper","Flyering","Friends","Percentage","Santa","Friday","GrabOne","HubSpot","Lorenzo","Mailings","Rabbit","Reclean",
             "XMAS","Fiksu","FixedBlock","£","Google","Groupon","Scheme","Rabit","Welcome","External","Berenberg","Promo","Huei","SANTA",
             "EUR15","St Giles","Hassle","Big Boss","Expansion")
df_items$final_channel[which(sapply(paste(flyering,collapse = "|"), function(x) grepl(x,df_items$channel))==TRUE)] <- "Flyering"
display = c("Appstore","Display","Deal")
df_items$final_channel[which(sapply(paste(display,collapse = "|"), function(x) grepl(x,df_items$channel))==TRUE)] <- "Display"
fb = c("Facebook","Social Media Paid")
df_items$final_channel[which(sapply(paste(fb,collapse = "|"), function(x) grepl(x,df_items$channel))==TRUE)] <- "Facebook"
internal = c("Internal","Team")
df_items$final_channel[which(sapply(paste(internal,collapse = "|"), function(x) grepl(x,df_items$channel))==TRUE)] <- "Internal"
out_of_home = c("OOH","Out of Home")
df_items$final_channel[which(sapply(paste(out_of_home,collapse = "|"), function(x) grepl(x,df_items$channel))==TRUE)] <- "Out Of Home"
sea = c("SEA","SEM")
df_items$final_channel[which(sapply(paste(sea,collapse = "|"), function(x) grepl(x,df_items$channel))==TRUE)] <- "SEM"
twitter = "Twitter"
df_items$final_channel[which(sapply(twitter, function(x) grepl(x,df_items$channel))==TRUE)] <- "Twitter"
flash = "Flash Sales"
df_items$final_channel[which(sapply(flash, function(x) grepl(x,df_items$channel))==TRUE)] <- "Flash Sales"
refer_friend = c("Refer a Friend","Refer a Friend ","Refer")
df_items$final_channel[which(sapply(paste(refer_friend,collapse = "|"), function(x) grepl(x,df_items$channel))==TRUE)] <- "Refer a Friend"
affiliate = "Affiliate"
df_items$final_channel[which(sapply(affiliate, function(x) grepl(x,df_items$channel))==TRUE)] <- "Affiliate"
appco = "Appco/Direct Sales"
df_items$final_channel[which(sapply(appco, function(x) grepl(x,df_items$channel))==TRUE)] <- "Appco/Direct Sales"
printads = c("Print","Print Ad","Print Ads")
df_items$final_channel[which(sapply(paste(printads,collapse = "|"), function(x) grepl(x,df_items$channel))==TRUE)] <- "Print Ads"
website = "Website"
df_items$final_channel[which(sapply(website, function(x) grepl(x,df_items$channel))==TRUE)] <- "Website"
seo = "SEO"
df_items$final_channel[which(sapply(seo, function(x) grepl(x,df_items$channel))==TRUE)] <- "SEO"
unpaid = c("Socia Media Unpaid", "Socia Media Unpaid ", "Unpaid")  
df_items$final_channel[which(sapply(paste(unpaid,collapse = "|"), function(x) grepl(x,df_items$channel))==TRUE)] <- "Social Media Unpaid"

df_items$final_channel[which(df_items$final_channel=="NA")] <- 'Other'


#Merge back to orders
orders <- merge(x = orders, y = df_items[,c("voucherCode","final_channel")], by = "voucherCode", all.x = T)

#Work with export from Google Analytics
funnel <- funnel[!duplicated(funnel),]
length(unique(funnel$reference))

#Transform to data table and make unique inputs
funnel <- data.table(funnel)
funnel <- funnel[,.(min(first_channel),min(last_channel)),by = .(reference)]
colnames(funnel) <- c("reference","first_channel","last_channel")

#Merge back to orders
orders <- merge(x = orders, y = funnel, by = "reference", all.x = T)
colnames(orders)[names(orders)=="final_channel"] <- "voucher_channel"

#Rename, first set to the same class
orders$first_channel <- as.character(orders$first_channel)
orders$last_channel <- as.character(orders$last_channel)

orders[,c(8:10)][is.na(orders[,c(8:10)])] <- "not set"
orders[,c(8:10)][(orders[,c(8:10)])=="(unavailable)"] <- "not set"

#Attach weights for voucher channel, first and last channel
orders$voucherScore <- 1
orders$voucherScore[which(orders$voucher_channel == "not set")] <- 0
orders$voucherScore[which(orders$first_channel == "not set" & orders$last_channel != "not set" & orders$voucher_channel != "not set")] <- 0.5
orders$voucherScore[which(orders$first_channel == "not set" & orders$last_channel == "not set" & orders$voucher_channel != "not set")] <- 1
orders$voucherScore[which(orders$first_channel != "not set" & orders$last_channel == "not set" & orders$voucher_channel != "not set")] <- 0.5
orders$voucherScore[which(orders$first_channel != "not set" & orders$last_channel != "not set" & orders$voucher_channel != "not set")] <- 0.33

orders$firstScore <- 1
orders$firstScore[which(orders$first_channel == "not set")] <- 0
orders$firstScore[which(orders$voucher_channel == "not set" & orders$last_channel != "not set" & orders$first_channel != "not set")] <- 0.5
orders$firstScore[which(orders$voucher_channel == "not set" & orders$last_channel == "not set" & orders$first_channel != "not set")] <- 1
orders$firstScore[which(orders$voucher_channel != "not set" & orders$last_channel == "not set" & orders$first_channel != "not set")] <- 0.5
orders$firstScore[which(orders$voucher_channel != "not set" & orders$last_channel != "not set" & orders$first_channel != "not set")] <- 0.33

orders$lastScore <- 1
orders$lastScore[which(orders$last_channel == "not set")] <- 0
orders$lastScore[which(orders$voucher_channel == "not set" & orders$first_channel != "not set" & orders$last_channel != "not set")] <- 0.5
orders$lastScore[which(orders$voucher_channel == "not set" & orders$first_channel == "not set" & orders$last_channel != "not set")] <- 1
orders$lastScore[which(orders$voucher_channel != "not set" & orders$first_channel == "not set" & orders$last_channel != "not set")] <- 0.5
orders$lastScore[which(orders$voucher_channel != "not set" & orders$first_channel != "not set" & orders$last_channel != "not set")] <- 0.33

#Rest channel
orders$trackedScore <- orders$voucherScore + orders$firstScore + orders$lastScore
orders$restScore <-  1 - orders$trackedScore 
orders$nottracked_channel <- "not set"


#Subset df as weights
weights <- orders[,c("reference","voucher_channel","first_channel","last_channel","nottracked_channel",
                     "voucherScore","firstScore","lastScore","restScore")]
class(weights)
weights <- data.table(weights)
str(weights)
#####################################
## Converting from wide to long and long to wide
## Reshaping function for data tables:
# from wide to long "melt"
# from long to wide "dcast"
#####################################

colA = names(weights)[2:5]
colB = names(weights)[6:9]
weights_long = melt(weights, measure = list(colA, colB), value.name = "reference")
colnames(weights_long) <- c("reference","var","channel","weight")



#Assign final channel
weights_long$final_channel <- weights_long$channel
organic = c("SEO","Organic Search")
weights_long$final_channel[which(sapply(paste(organic,collapse = "|"), function(x) grepl(x,weights_long$channel))==TRUE)] <- "SEO"
nonorganic = c("SEM","Paid Search")
weights_long$final_channel[which(sapply(paste(nonorganic,collapse = "|"), function(x) grepl(x,weights_long$channel))==TRUE)] <- "SEM"
referral = c("Refer a Friend","Referral","Refer")
weights_long$final_channel[which(sapply(paste(referral,collapse = "|"), function(x) grepl(x,weights_long$channel))==TRUE)] <- "Refer a Friend"
crmj = c("email","Email","Journey")
weights_long$final_channel[which(sapply(paste(crmj,collapse = "|"), function(x) grepl(x,weights_long$channel))==TRUE)] <- "CRM-Journey"
social_network = c("Facebook","Network")
weights_long$final_channel[which(sapply(paste(social_network,collapse = "|"), function(x) grepl(x,weights_long$channel))==TRUE)] <- "Facebook"



#Combine with the new split of channels
mixed_df <- weights_long[,.(sum(weight)),by = .(reference,final_channel)]
colnames(mixed_df)[3] <- c("sum_weight")
sum(mixed_df$sum_weight)

#Merge with data from orders
mixed_df <- merge(x = mixed_df, y = orders[,c("reference","createdAt","state",
                                              "locationIdentifier","createdBy.userAgent","customer")],by="reference",all.x = T)


#Add isvalid column (first, initialize default value, then, adjust by particular state)
mixed_df$isvalid = 1 #initialize isvalid
mixed_df$isvalid[which(mixed_df$state %in% c("new","payment_authorisation_error",
                                                    "canceled","reserved"))] = 0 

mixed_df$device <- NA
mixed_df$device[(grepl("iPhone",mixed_df$createdBy.userAgent)==T)] = "iOS app"
mixed_df$device[(grepl("Android",mixed_df$createdBy.userAgent)==T)] = "android app"
mixed_df$device[(!is.na(mixed_df$createdBy.userAgent) 
                     & !grepl("^(iOS)",mixed_df$device) 
                     & !grepl("^(android)",mixed_df$device))] = "web app"


#Find Status by getting first order
firstOrder <- mixed_df[isvalid==1,.(min(createdAt)),by=.(customer)]
mixed_df <- merge(x = mixed_df, y = firstOrder, by = "customer",all.x = T)
colnames(mixed_df)[11] <- "firstOrder"

mixed_df$customerStatus = "New"
mixed_df$customerStatus[which(mixed_df$createdAt > mixed_df$firstOrder)] <- "Returning"
mixed_df$createdAt <- as.Date(mixed_df$createdAt, format = "%Y-%m-%d")
mixed_df$firstOrder <- as.Date(mixed_df$firstOrder, "%Y-%m-%d")

#Write to specific folder
write.csv(mixed_df, file="/home/dima/powerbi-share/R_outputs/attribution.csv",row.names = FALSE)
