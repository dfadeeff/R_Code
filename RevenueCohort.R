

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

#Create df with total revenue for new customers per cohort, sorted by initialcohort
y = arrange(nsubset[BinCustomerStatus==0, .(sum(GrossTotalBeforeDiscount)), by=initial_cohort],desc(initial_cohort))
colnames(y) <- c('initialcohort','number')


#Check the count of returning customers, note "by" function: aggregate by BOTH sinceFirstOrder and initiacohort
x = arrange(nsubset[BinCustomerStatus==1, .(sum(GrossTotalBeforeDiscount)), 
                    by=.(initial_cohort, sinceFirstOrder)],desc(sinceFirstOrder))
colnames(x) <- c("initialcohort","sinceFirstOrder","Returning")

#Merge the stuff (note all.x, doing left join)
cohort <- merge(x = x, y = y, by = "initialcohort",all.x = TRUE)

#Create percentages
cohort$percent <- round(cohort$Returning/cohort$number,4)
#Subset the dataset, deleting two columns
cohort$Returning<- NULL
cohort$number <- NULL

#Employ the dcast library
output <- arrange(dcast(cohort, initialcohort~round(sinceFirstOrder),value.var = "percent"),initialcohort)
output = data.table(output)

#Make intitial cohort character for displaying purposes
output$initialcohort <- as.character(output$initialcohort)

#Output html using DT library
brks <- quantile(output[,2:ncol(output),with = FALSE], probs = seq(0,1,0.1), na.rm = TRUE)
#brks <- seq(0,max(output[,2:ncol(output),with=FALSE],na.rm = TRUE), length.out = 50) #use hard coded seq with uniform dist
clrs <- colorRamp(c("white", "green"))
clrs <- rgb(clrs(seq(0,1,length.out = length(brks)+1)),max=255)

cohort_table <- datatable(output,extensions = list("Buttons"= NULL),options=list("paging"= F,"searching"=T,"autoWidth"= F, dom = 'Bfrtip',
                                                                                 buttons = c( 'csv','excel')))%>%formatPercentage(c(2:ncol(output)),0)%>%formatStyle(c(2:ncol(output)),
                                                                                 backgroundColor = styleInterval(brks, clrs))

saveWidget(cohort_table, file="/home/bi_user/shinyserver/Cohorts/Revenue_cohorts_global.html",selfcontained = F)



##########################################################################################################
#### NOW BY CITY
##########################################################################################################

#Create a df from nsubset with regard to cities
initial_cohortByCities <- arrange(nsubset[BinCustomerStatus==0, .(sum(GrossTotalBeforeDiscount)),
                                 by=.(initial_cohort,location)],desc(initial_cohort))
colnames(initial_cohortByCities) <- c("initial_cohort", "location", "number") 

returning_ByCities <- arrange(nsubset[BinCustomerStatus==1, .(sum(GrossTotalBeforeDiscount)), 
                                      by=.(initial_cohort, sinceFirstOrder,location)],desc(sinceFirstOrder))

colnames(returning_ByCities) <- c("initial_cohort","sinceFirstOrder","location","Returning")

#Merge the stuff (note all.x, doing left join)
cohort_ByCities <- merge(x = returning_ByCities, y = initial_cohortByCities, by = c("initial_cohort","location"),all.x = TRUE)
cohort_ByCities$percent <- round(cohort_ByCities$Returning/cohort_ByCities$number,4)

cohort_ByCities$Returning <- NULL
cohort_ByCities$number <- NULL

#Convert to data table
cohort_ByCities <- data.table(cohort_ByCities)

################
#London
################

london <- cohort_ByCities[cohort_ByCities$location == "gb_london"]
london <- na.omit(london)

#Employ the dcast library
london_out <- arrange(dcast(london, initial_cohort~round(sinceFirstOrder),value.var = "percent"),initial_cohort)
london_out = data.table(london_out)

#Convert yearmon to character
london_out$initial_cohort <- as.character(london_out$initial_cohort)


lon_table <- datatable(london_out,extensions = list("Buttons"= NULL),options=list("paging"= F,"searching"=T,"autoWidth"= F, dom = 'Bfrtip',
                                                                                 buttons = c( 'csv','excel')))%>%formatPercentage(c(2:ncol(london_out)),0)%>%formatStyle(c(2:ncol(london_out)),
                                                                                backgroundColor = styleInterval(brks, clrs))

saveWidget(lon_table, file="/home/bi_user/shinyserver/Cohorts/Revenue_cohorts_london.html",selfcontained = F)

################
#Berlin
################

berlin <- cohort_ByCities[cohort_ByCities$location == "de_berlin"]
berlin <- na.omit(berlin)

#Employ the dcast library
berlin_out <- arrange(dcast(berlin, initial_cohort~round(sinceFirstOrder),value.var = "percent"),initial_cohort)
berlin_out <- data.table(berlin_out)

#Convert yearmon to character
berlin_out$initial_cohort <- as.character(berlin_out$initial_cohort)

ber_table <- datatable(berlin_out,extensions = list("Buttons"= NULL),options=list("paging"= F,"searching"=T,"autoWidth"= F, dom = 'Bfrtip',
                                                                                  buttons = c( 'csv','excel')))%>%formatPercentage(c(2:ncol(berlin_out)),0)%>%formatStyle(c(2:ncol(berlin_out)),
                                                                                  backgroundColor = styleInterval(brks, clrs))

saveWidget(ber_table, file="/home/bi_user/shinyserver/Cohorts/Revenue_cohorts_berlin.html",selfcontained = F)



################
#Paris
################

paris <- cohort_ByCities[cohort_ByCities$location == "fr_paris"]
paris <- na.omit(paris)

#Employ the dcast library
paris_out <- arrange(dcast(paris, initial_cohort~round(sinceFirstOrder),value.var = "percent"),initial_cohort)
paris_out <- data.table(paris_out)

#Convert yearmon to character
paris_out$initial_cohort <- as.character(paris_out$initial_cohort)

par_table <- datatable(paris_out,extensions = list("Buttons"= NULL),options=list("paging"= F,"searching"=T,"autoWidth"= F, dom = 'Bfrtip',
                                                                                  buttons = c( 'csv','excel')))%>%formatPercentage(c(2:ncol(paris_out)),0)%>%formatStyle(c(2:ncol(paris_out)),
                                                                                  backgroundColor = styleInterval(brks, clrs))

saveWidget(par_table, file="/home/bi_user/shinyserver/Cohorts/Revenue_cohorts_paris.html",selfcontained = F)

