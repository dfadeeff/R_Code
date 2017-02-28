
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
library(jsonlite)


#####################
# Connect to Adjust
#####################

collection = c("events","intwash_orders")

adjust = mongo(collection = collection[1], db = "adjust", 
                 url = "mongodb://10.10.15.193:27017",verbose = TRUE)

df_adjust = adjust$find(query = '{"event":"submit_order"}',fields = '{"reference_id":1,"network":1,"event":1}')


#####################
# Get Google Analytics extractor
#####################
#Set the folders
from_folder <- "/home/dima/powerbi-share/R_inputs"
to_folder <- "/home/dima/Automation/Reports/Cohorts"
#Identify files
list_files <- list.files(from_folder,"ga_extractor_onefile.csv",full.names = T)

#Copy the files
file.copy(list_files,to=to_folder,overwrite = T)
ga = read.csv("ga_extractor_onefile.csv",header = T,sep = ",")


#####################
# Get voucher channel
#####################
load("DataToMrkt.dat")
load("DataToCRM.dat")

orders <- orders_table[,c("voucherCode","reference","customer","createdAt","state","locationIdentifier")]

#####################
# Get referral
#####################


intwash <- mongo(collection = collection[2], db = "uk_live", 
                 url = "mongodb://172.31.51.215:27017",verbose = TRUE)

referral <- intwash$find(fields = '{"reference":1,"referral":1}')

#Flatten the referral dataframe
referral <- flatten(referral)

#Merge with referrals
orders <- merge(x = orders, y = referral[,c("reference","referral.referrerId")],by = "reference",all.x = TRUE)

#Merge with voucher channels
names(df_items)[names(df_items)=="final_channel"] <- "voucher_channel"
orders <- merge(x = orders, y = df_items[,c("voucherCode","voucher_channel")], by = 'voucherCode',all.x = TRUE)

#Merge with adjust
names(df_adjust)[names(df_adjust)=="reference_id"] <- "reference"
df_adjust <- df_adjust[!duplicated(df_adjust$reference),]
orders <- merge(x = orders, y = df_adjust[,c("network","reference")], by = 'reference',all.x = TRUE)

#Merge with Google Analytics
names(ga)[names(ga)=="ga.transactionID"] <- "reference"
ga <- ga[!duplicated(ga$reference),]

#Change datatypes for multiple columns
unfactorize <- c("ga.source","ga.medium","ga.keyword")
ga[,unfactorize] = lapply(unfactorize,function(x) as.character(as.factor(ga[,x])))
ga$initial_channel <- ifelse(ga$ga.medium=="(none)" | ga$ga.medium=="(not set)",ga$ga.source,ga$ga.medium)

to_replace = c("(not set)","(direct)","Unknown")
ga$ga_channel <- ga$initial_channel
ga$ga_channel[which(sapply(paste(to_replace,collapse = "|"), function(x) grepl(x,ga$ga_channel))==TRUE)] <- "Direct"

organic <- c("Organic","organic")
ga$ga_channel[which(sapply(paste(organic,collapse = "|"), function(x) grepl(x,ga$ga_channel))==TRUE)] <- "SEO"

#Merge to orders
orders <- merge(x = orders, y = ga[,c("reference","ga_channel")], by = "reference", all.x = TRUE)

#Code the marketing channel
orders$mrkt_channel <- "NA"
orders$mrkt_channel[which(!is.na(orders$referral.referrerId))] <- "Refer a Friend"
orders$mrkt_channel[which(is.na(orders$referral.referrerId) 
                          & !is.na(orders$voucher_channel))] <- orders$voucher_channel[is.na(orders$referral.referrerId) & !is.na(orders$voucher_channel)]
orders$mrkt_channel[which(orders$mrkt_channel=="NA" 
                          & !is.na(orders$network))] <- orders$network[orders$mrkt_channel=="NA" & !is.na(orders$network)]
orders$mrkt_channel[which(orders$mrkt_channel=="NA" 
                          & !is.na(orders$ga_channel))] <- orders$ga_channel[orders$mrkt_channel=="NA" & !is.na(orders$ga_channel)]
orders$mrkt_channel[which(orders$mrkt_channel=="NA")] <- "Direct"

unique(orders$mrkt_channel)

#################
###Recode the marketing channel
#################

to_sem <- c("SEM DE","SEM","cpc","SEM UK","SEM FR")
orders$mrkt_channel[which(sapply(paste(to_sem,collapse = "|"), function(x) grepl(x,orders$mrkt_channel))==TRUE)] <- "SEM"

to_cooperations <- c("Partnerships","cooperation","Cooperations","referral","partnerships")
orders$mrkt_channel[which(sapply(paste(to_cooperations,collapse = "|"), function(x) grepl(x,orders$mrkt_channel))==TRUE)] <- "Cooperations"

to_crmcampagin <- c("email","CRM-Campaign","Email","leadads","newsletter","crm")
orders$mrkt_channel[which(sapply(paste(to_crmcampagin,collapse = "|"), function(x) grepl(x,orders$mrkt_channel))==TRUE)] <- "CRM-Campaign"

to_seo <- c("SEO","googlemybusiness","Google Universal App Campaigns")
orders$mrkt_channel[which(sapply(paste(to_seo,collapse = "|"), function(x) grepl(x,orders$mrkt_channel))==TRUE)] <- "SEO"

to_fb <- c("Facebook","maia","Off-Facebook Installs","Instagram Installs","Social_Facebook","Facebook Installs")
orders$mrkt_channel[which(sapply(paste(to_fb,collapse = "|"), function(x) grepl(x,orders$mrkt_channel))==TRUE)] <- "Facebook"

to_aff <- c("affliate","zanox","Affiliate")
orders$mrkt_channel[which(sapply(paste(to_aff,collapse = "|"), function(x) grepl(x,orders$mrkt_channel))==TRUE)] <- "Affiliate"

to_display <- c("Deep Link","UIM","Display","display","Plista")
orders$mrkt_channel[which(sapply(paste(to_display,collapse = "|"), function(x) grepl(x,orders$mrkt_channel))==TRUE)] <- "Display"

to_social <- c("social","Social Media Unpaid")
orders$mrkt_channel[which(sapply(paste(to_social,collapse = "|"), function(x) grepl(x,orders$mrkt_channel))==TRUE)] <- "Social Media Unpaid"

to_offline <- c("Offline","Flyer","Flyering")
orders$mrkt_channel[which(sapply(paste(to_offline,collapse = "|"), function(x) grepl(x,orders$mrkt_channel))==TRUE)] <- "Flyering"

to_website <- c("website","Website")
orders$mrkt_channel[which(sapply(paste(to_website,collapse = "|"), function(x) grepl(x,orders$mrkt_channel))==TRUE)] <- "Website"

to_twitter <- c("Twitter","Twitter Installs")
orders$mrkt_channel[which(sapply(paste(to_twitter,collapse = "|"), function(x) grepl(x,orders$mrkt_channel))==TRUE)] <- "Twitter"

to_direct <- c("Organic","Direct")
orders$mrkt_channel[which(sapply(paste(to_direct,collapse = "|"), function(x) grepl(x,orders$mrkt_channel))==TRUE)] <- "Direct"

#Add valid 
orders$isvalid = 1 #initialize isvalid
orders$isvalid[which(orders$state %in% c("new","payment_authorisation_error",
                                               "canceled","reserved"))] = 0 #adjust

#Check total number of valid orders
sum(orders$isvalid)



################
## Prepare transformations
################

#Convert to data table
orders <- as.data.table(orders)

#Get the first order for each customer
firstOrder <- orders[isvalid==1,min(createdAt),by=customer]

#Merge back to orders
orders <- merge(x = orders, y = firstOrder, by = 'customer', all.x = TRUE)

#Change foramt of createdAt to date
orders$createdAt <- as.Date(orders$createdAt, "%Y-%m-%d")
#Rename newly merged column
names(orders)[names(orders)=="V1"] <- "firstOrder"
#Change format of firstOrder to date
orders$firstOrder <- as.Date(orders$firstOrder, "%Y-%m-%d")

#Create column with customer Status
orders$customerStatus <- 0
orders$customerStatus[which(orders$createdAt > orders$firstOrder)] <- 1


#Make a subset
df_subset <- orders[, .(customer,state,createdAt,mrkt_channel,isvalid,customerStatus,firstOrder,locationIdentifier)]

#Calculate difference btw first Order and subsequent orders in months and initial cohort
df_subset$sinceFirstOrder <- 12 * as.numeric((as.yearmon(df_subset$createdAt)-as.yearmon(df_subset$firstOrder)))
df_subset$initialcohort <-as.yearmon(df_subset$firstOrder)

#Make a subset of only completed orders
nsubset <- df_subset[df_subset$state == 'completed']

length(unique(nsubset$customer))
sum(nsubset$isvalid)

#Take the first channel!
firstChannel <- nsubset[customerStatus==0, min(mrkt_channel) ,by=.(customer)]
colnames(firstChannel)[names(firstChannel)=="V1"] <- "firstChannel"

#Check uniqueness
length(unique(firstChannel$customer))

#Merge back to nsubset
nsubset <- merge(x = nsubset, y = firstChannel, by="customer",all.x = T)


#Arrange the datasets
y = arrange(nsubset[customerStatus==0, .(length(unique(customer))), by=.(initialcohort, firstChannel, locationIdentifier)],desc(initialcohort))
x = arrange(nsubset[customerStatus==1, .(length(unique(customer))), 
                    by=.(initialcohort, sinceFirstOrder, firstChannel,locationIdentifier)],desc(sinceFirstOrder))

#Rename the col names
colnames(y) <- c("initialcohort","firstChannel","location","cohort")
colnames(x) <- c("initialcohort","sinceFirstOrder","firstChannel","location","returning")
class(x)
class(y)

#Append variables from Y, i.e. initial cohort numbers
z = merge(x = x, y = y, by = c("initialcohort","firstChannel", "location"),all.x = T)
z$cohort[is.na(z$cohort)] <- 0

#Check NA in z
sum(is.na(z$cohort))
names(z)
write.csv(z, file = "/home/dima/powerbi-share/R_outputs/channelcohorts.csv",row.names = FALSE)



