##########################
## Prepare the working env
##########################
getwd()
setwd("/home/dima/Automation/Reports/Stages")
rm(list = ls(all=T))


##########################
## Load libraries
##########################
library(zoo)

##########################
## Load datafiles from other scripts
##########################
load("stages.dat")
load("DataToCohorts.dat")

##########################
## Work with initial minmax 
##########################
#Check unique customers and total orders
length(unique(minmax$customer))
sum(minmax$totalValidOrders)

#Aggregate by customer
class(minmax)

#Check duplicates due to different city
minmax[duplicated(minmax$customer)]

##########################
## Aggregate on unique customer basis
##########################
unique_cust <- minmax[,list(location[1],sum(totalValidOrders),stages[1],head(firstOrder,1),tail(lastOrder,1)),by=customer]
colnames(unique_cust) <- c("customer","location","totalOrders","stage","firstOrder","lastOrder")

#Check total orders
sum(unique_cust$totalOrders)
length(unique(unique_cust$customer))

#Add lifespan
unique_cust$lifespan <- difftime(time1 = unique_cust$lastOrder,
                                 time2 = unique_cust$firstOrder,
                                 units = 'days')
##########################
## Compute average lifespan
##########################
average_lifespan <- unique_cust[totalOrders > 1, mean(lifespan),]
lifespanbycity = unique_cust[totalOrders >1,median(lifespan),by=location]
colnames(lifespanbycity) <- c("location","lifespan")
lifespanbycity$lifespan <- as.numeric(lifespanbycity$lifespan)

##########################
## Get orders and customers per month
##########################
sum(orders_data$isvalid)
orders_data$createdAtYM <- as.yearmon(orders_data$createdAt)
orders_per_user <- orders_data[isvalid==1,list(length(unique(customer)),sum(isvalid)),by=.(createdAtYM,location)]
colnames(orders_per_user) <- c("monthYear","location","customers","orders")
sum(orders_per_user$orders)

orders_per_user$orderperuser <- orders_per_user$orders/orders_per_user$customers

##########################
## Get revenue per order
##########################
aov <- orders_data[!is.na(GrossTotalBeforeDiscount),list(sum(GrossTotalBeforeDiscount),sum(isvalid)),by=.(createdAtYM,location)]
colnames(aov) <- c("monthYear","location","grossBeforeEur","orders")
aov$aov <- aov$grossBeforeEur/aov$orders

##########################
## Merge three points alltogether
##########################

orders_per_user <- merge(x = orders_per_user, y = aov, by = c("monthYear","location"),all.x = TRUE )
orders_per_user <- merge(x = orders_per_user, y = lifespanbycity, by = "location",all.x = TRUE)

##########################
## Replace lifespan
##########################
orders_per_user$finallifespan <- orders_per_user$lifespan
orders_per_user$finallifespan[which(orders_per_user$location=="fr_paris")] <- average_lifespan
str(orders_per_user)

##########################
## Get simple CLV
##########################
orders_per_user$clv <- orders_per_user$aov*orders_per_user$finallifespan/30*orders_per_user$orderperuser
