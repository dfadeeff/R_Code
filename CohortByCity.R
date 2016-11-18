rm(list = ls(all=TRUE))
getwd()
setwd("/home/dima/Automation/Reports/Cohorts")

#install.packages("data.table")
#install.packages("dplyr")
#install.packages("zoo")
#install.packages("DT")
library(zoo)
library(mongolite)
library(sqldf)
library(data.table)
library(dplyr)
library(DT)
library(plyr)
library(htmlwidgets)


#Initialize collections to import
collection = c("intwash_orders", "intwash_customers")

#Load orders
orders <- mongo(collection = collection[1], db = "uk_live", 
                url = "mongodb://172.31.51.215:27017",verbose = TRUE)

#Parse thru collection and find the keys 
orders_data <- orders$find(fields = '{"_id":1, "reference":1,"createdAt":1,"customer":1,"state":1,"locationIdentifier":1}')

#Add isvalid column (first, initialize default value, then, adjust by particular state)
orders_data$isvalid = 1 #initialize isvalid
orders_data$isvalid[which(orders_data$state %in% c("new","payment_authorisation_error",
                                                                 "canceled","reserved"))] = 0 #adjust

#Change datatype
orders_data$createdAt <- as.Date(orders_data$createdAt, "%Y-%m-%d")

#Create a datatable
orders_data = data.table(orders_data)


#Create a dt with the min created (aka, first order), aggregation by customer
newdf_firstorder <- orders_data[isvalid == 1 , min(createdAt) , by = .(customer)]

#Merge tables
newdf <- merge(x = orders_data, y= newdf_firstorder, by = "customer", all.x = TRUE)

#Get first order
newdf$sinceFirstOrder <- 12 * as.numeric((as.yearmon(newdf$createdAt)-as.yearmon(newdf$V1)))
#Change format to year/month 
newdf$initialcohort <-as.yearmon(newdf$V1)
#Create Customer Status, i.e. convert to 0 if New and 1 to Returning
newdf$BinCustomerStatus = 0 
newdf$BinCustomerStatus[which(newdf$createdAt > newdf$V1)] = 1


#Subset the data table where all rows have state 'completed'
nsubset <- newdf[newdf$state == 'completed']

#Check the count of initial cohort, sorted by initialcohort
y = arrange(nsubset[BinCustomerStatus==0, .(length(unique(customer))), by=initialcohort],desc(initialcohort))
#Rename colnames
colnames(y) <- c('initialcohort','number')

#Check the count of returning customers, note "by" function: aggregate by BOTH sinceFirstOrder and initiacohort
x = arrange(nsubset[BinCustomerStatus==1, .(length(unique(customer))), 
                by=.(initialcohort, sinceFirstOrder)],desc(sinceFirstOrder))
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

output$initialcohort <- as.character(output$initialcohort)

y$initialcohort <- as.character(y$initialcohort)

#output$initialcohort <- paste(output$initialcohort, "(", y[match(output$initialcohort,y$initialcohort),"number"],")")

#Output html using DT library
brks <- quantile(output[,2:ncol(output),with = FALSE], probs = seq(0,1,0.1), na.rm = TRUE)
#brks <- seq(0,max(output[,2:ncol(output),with=FALSE],na.rm = TRUE), length.out = 50) #use hard coded seq with uniform dist
clrs <- colorRamp(c("white", "green"))
clrs <- rgb(clrs(seq(0,1,length.out = length(brks)+1)),max=255)


cohort_table <- datatable(output,extensions = list("Buttons"= NULL),options=list("paging"= F,"searching"=T,"autoWidth"= F, dom = 'Bfrtip',
                                                                 buttons = c( 'csv','excel')))%>%formatPercentage(c(2:ncol(output)),0)%>%formatStyle(c(2:ncol(output)),
                                                                 backgroundColor = styleInterval(brks, clrs))
saveWidget(cohort_table, file="/home/dima/shinyserver/Cohorts/cohorts.html",selfcontained = TRUE)


########################################################################
# Now Distribute by City
########################################################################

set <- arrange(nsubset[BinCustomerStatus==0,
                          .(length(unique(customer))),
                          by=c("initialcohort","locationIdentifier")],desc(initialcohort))


#Rename colnames
colnames(set) <- c('initialcohort','city','number')


#Check the count of returning customers, note "by" function: aggregate by BOTH sinceFirstOrder and initiacohort
CohortByCities = arrange(nsubset[BinCustomerStatus==1, .(length(unique(customer))), 
                    by=.(initialcohort, sinceFirstOrder,locationIdentifier)],
                    desc(sinceFirstOrder))
colnames(CohortByCities) <- c("initialcohort","sinceFirstOrder","city","Returning")

#Merge the stuff (note all.x, doing left join)
cohortCities <- merge(x = CohortByCities, y = set, by = c("initialcohort","city"),all.x = TRUE)

#Create percentages
cohortCities$percent <- round(cohortCities$Returning/cohortCities$number,4)

#Subset the dataset, deleting two columns
cohortCities$Returning<- NULL
cohortCities$number <- NULL

#Create function for percentages
#percent <- function(x, digits = 0, format = "f", ...) {
#  paste0(formatC(100 * x, format = format, digits = digits, ...), "%")
#}


#Split by city to create a list
#cohortSplit <- split(cohortCities,cohortCities$city)

#For every city, apply brute force conversion
#London
cohortLondon <- cohortCities[cohortCities$city=='gb_london']
cohortLondon <- arrange(dcast(cohortLondon, initialcohort~round(sinceFirstOrder),value.var = "percent"),initialcohort)
cohortLondon = data.table(cohortLondon)
cohortLondon$initialcohort <- as.character(cohortLondon$initialcohort)
cohortLondon$initialcohort <- as.character(cohortLondon$initialcohort)

cohort_table_London <- datatable(cohortLondon,extensions = list("Buttons"= NULL),options=list("paging"= F,"searching"=T,"autoWidth"= F, dom = 'Bfrtip',
                                                                                 buttons = c( 'csv','excel')))%>%formatPercentage(c(2:ncol(cohortLondon)),0)%>%formatStyle(c(2:ncol(cohortLondon)),
                                                                                 backgroundColor = styleInterval(brks, clrs))
saveWidget(cohort_table_London, file="/home/dima/shinyserver/Cohorts/UserCohortsLondon.html",selfcontained = TRUE)

#Berlin
cohortBerlin <- cohortCities[cohortCities$city=='de_berlin']
cohortBerlin <- arrange(dcast(cohortBerlin, initialcohort~round(sinceFirstOrder),value.var = "percent"),initialcohort)
cohortBerlin = data.table(cohortBerlin)
cohortBerlin$initialcohort <- as.character(cohortBerlin$initialcohort)
cohortBerlin$initialcohort <- as.character(cohortBerlin$initialcohort)

cohort_table_Berlin <- datatable(cohortBerlin,extensions = list("Buttons"= NULL),options=list("paging"= F,"searching"=T,"autoWidth"= F, dom = 'Bfrtip',
                                                                                              buttons = c( 'csv','excel')))%>%formatPercentage(c(2:ncol(cohortBerlin)),0)%>%formatStyle(c(2:ncol(cohortBerlin)),
                                                                                                                                                                                        backgroundColor = styleInterval(brks, clrs))
saveWidget(cohort_table_Berlin, file="/home/dima/shinyserver/Cohorts/UserCohortsBerlin.html",selfcontained = TRUE)

#Paris
cohortParis <- cohortCities[cohortCities$city=='fr_paris']
cohortParis <- arrange(dcast(cohortParis, initialcohort~round(sinceFirstOrder),value.var = "percent"),initialcohort)
cohortParis = data.table(cohortParis)
cohortParis$initialcohort <- as.character(cohortParis$initialcohort)
cohortParis$initialcohort <- as.character(cohortParis$initialcohort)

cohort_table_Paris <- datatable(cohortParis,extensions = list("Buttons"= NULL),options=list("paging"= F,"searching"=T,"autoWidth"= F, dom = 'Bfrtip',
                                                                                              buttons = c( 'csv','excel')))%>%formatPercentage(c(2:ncol(cohortParis)),0)%>%formatStyle(c(2:ncol(cohortParis)),
                                                                                                                                                                                        backgroundColor = styleInterval(brks, clrs))
saveWidget(cohort_table_Paris, file="/home/dima/shinyserver/Cohorts/UserCohortsParis.html",selfcontained = TRUE)


