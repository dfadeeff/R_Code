Sys.setenv(RSTUDIO_PANDOC="/usr/lib/rstudio-server/bin/pandoc")

#Set wd
getwd()
setwd("/home/bi_user/Automation/Reports/Cohorts")

rm(list = ls(all=TRUE))

#Provide new paths to libraries
.libPaths("/home/bi_user/R/x86_64-pc-linux-gnu-library/3.3/")
#Load libraries
library(zoo)
library(DT)
library(data.table)
library(dplyr)
library(DT)
library(plyr)
library(htmlwidgets)

#Load data 
load("DataToCohorts.dat")

#Subset the dataframe
dataset <- subset(orders_data, select = c(3,5:8,13,16))

#Convert createdAt to date
dataset$createdAt <- as.Date(dataset$createdAt, format = "%Y-%m-%d")

#Create a datatable
dataset = data.table(dataset)

#Create a dt with the min created (aka, first order), aggregation by customer
newdf_firstorder <- dataset[isvalid == 1 , min(createdAt) , by = .(customer)]

#Merge back to dataset
dataset <- merge(x = dataset, y= newdf_firstorder, by = "customer", all.x = TRUE)

#Rename the first order col
colnames(dataset)[8] <- c("firstOrder")

#Get first order
dataset$sinceFirstOrder <- 12 * as.numeric((as.yearmon(dataset$createdAt)-as.yearmon(dataset$firstOrder)))
#Change to year-month format
dataset$initial_cohort <- as.yearmon(dataset$firstOrder)

#Create Customer Status, i.e. convert to 0 if New and 1 to Returning
dataset$BinCustomerStatus = 0 
dataset$BinCustomerStatus[which(dataset$createdAt > dataset$firstOrder)] = 1

#Subset dataset with completed state
nsubset <- dataset[dataset$state == 'completed']


#Check the count of returning customers, note "by" function: aggregate by BOTH sinceFirstOrder and initiacohort
cohort = arrange(nsubset[BinCustomerStatus==1, .(sum(isvalid),length(unique(customer))), 
                    by=.(initial_cohort, sinceFirstOrder)],desc(sinceFirstOrder))
colnames(cohort) <- c("initialcohort","sinceFirstOrder","orders","customers")

#Create percentages
cohort$proportion <- round(cohort$orders /cohort$customers,2)
#Subset the dataset, deleting two columns
cohort$orders <- NULL
cohort$customers <- NULL

#Employ the dcast library
output <- arrange(dcast(cohort, initialcohort~round(sinceFirstOrder), value.var = "proportion"),initialcohort)
output = data.table(output)

#Make intitial cohort character for displaying purposes
output$initialcohort <- as.character(output$initialcohort)


#Create output
cohort_table <- datatable(output,extensions = list("Buttons"= NULL), options = list("paging"= F,"searching"=T,
                                                                                    "autoWidth"= F, dom = 'Bfrtip',
                                                                    buttons = c( 'csv','excel')))


saveWidget(cohort_table, file="/home/bi_user/shinyserver/Cohorts/OrdersPerUser_cohorts_global.html",selfcontained = F)


##########################################################################################################
#### NOW BY CITY
##########################################################################################################

#Create a df from nsubset with regard to cities
cohortByCities = arrange(nsubset[BinCustomerStatus==1, .(sum(isvalid),length(unique(customer))), 
                         by=.(initial_cohort, sinceFirstOrder,location)],desc(initial_cohort))

colnames(cohortByCities) <- c("initial_cohort","sinceFirstOrder","location","orders","customers")

cohortByCities <- data.table(cohortByCities)
##################################
#London
##################################
london <- cohortByCities[cohortByCities$location == "gb_london"]
london <- na.omit(london)
#Create percentages
london$proportion <- round(london$orders / london$customers,2)
#Subset the dataset, deleting two columns
london$orders <- NULL
london$customers <- NULL


#Employ the dcast library
lon_output <- arrange(dcast(london, initial_cohort~round(sinceFirstOrder), value.var = "proportion"), initial_cohort)
lon_output = data.table(lon_output)

lon_output$initial_cohort <- as.character(lon_output$initial_cohort)
#Create output
lon_table <- datatable(lon_output,extensions = list("Buttons"= NULL), options = list("paging"= F,"searching"=T,
                                                                                    "autoWidth"= F, dom = 'Bfrtip',
                                                                                    buttons = c( 'csv','excel')))

saveWidget(lon_table, file="/home/bi_user/shinyserver/Cohorts/OrdersPerUser_cohorts_London.html",selfcontained = F)


##################################
#Berlin
##################################
berlin <- cohortByCities[cohortByCities$location == "de_berlin"]
berlin <- na.omit(berlin)
#Create percentages
berlin$proportion <- round(berlin$orders / berlin$customers,2)
#Subset the dataset, deleting two columns
berlin$orders <- NULL
berlin$customers <- NULL

#Subset everything after launch
berlin <- berlin[berlin$initial_cohort >= "Jan 2015"]

#Employ the dcast library
ber_output <- arrange(dcast(berlin, initial_cohort~round(sinceFirstOrder), value.var = "proportion"), initial_cohort)
ber_output = data.table(ber_output)


ber_output$initial_cohort <- as.character(ber_output$initial_cohort)
#Create output
ber_table <- datatable(ber_output,extensions = list("Buttons"= NULL), options = list("paging"= F,"searching"=T,
                                                                                     "autoWidth"= F, dom = 'Bfrtip',
                                                                                     buttons = c( 'csv','excel')))

saveWidget(ber_table, file="/home/bi_user/shinyserver/Cohorts/OrdersPerUser_cohorts_Berlin.html",selfcontained = F)


##################################
#Paris
##################################
paris <- cohortByCities[cohortByCities$location == "fr_paris"]
paris <- na.omit(paris)
#Create percentages
paris$proportion <- round(paris$orders / paris$customers,2)
#Subset the dataset, deleting two columns
paris$orders <- NULL
paris$customers <- NULL

#Subset everything after launch
paris <- paris[paris$initial_cohort >= "May 2016"]

#Employ the dcast library
par_output <- arrange(dcast(paris, initial_cohort~round(sinceFirstOrder), value.var = "proportion"), initial_cohort)
par_output = data.table(par_output)


par_output$initial_cohort <- as.character(par_output$initial_cohort)
#Create output
par_table <- datatable(par_output,extensions = list("Buttons"= NULL), options = list("paging"= F,"searching"=T,
                                                                                     "autoWidth"= F, dom = 'Bfrtip',
                                                                                     buttons = c( 'csv','excel')))

saveWidget(par_table, file="/home/bi_user/shinyserver/Cohorts/OrdersPerUser_cohorts_Paris.html",selfcontained = F)

