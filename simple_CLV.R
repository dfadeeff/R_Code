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
library(data.table)

##########################
## Load datafiles from other scripts
##########################
load("stages.dat")
load("DataToCohorts.dat")
load("channelcohorts.dat")

##########################
## Work with initial minmax 
##########################
#Check unique customers and total orders
length(unique(minmax$customer))
sum(minmax$totalValidOrders)

##########################
## Add marketing channel
##########################

#Merge to get the customer reference
orders_table <- merge(x = orders_table, y = orders_data[,.(order,customer)], by.x = "reference", by.y = "order", all.x = TRUE)
orders_table <- data.table(orders_table)

#Createt df with single relation btw reference and customer
sin_relation <- orders_table[,.(customer.x,customer.y)]
sin_relation <- sin_relation[!duplicated(customer.x)]

sin_channel <- nsubset[,.(customer, firstChannel)]
sin_channel <- sin_channel[!duplicated(customer)]


sin_relation <- merge(x = sin_relation, y = sin_channel, by.x = "customer.x", by.y = "customer", all.x = TRUE)


#Bring to minimax
minmax <- merge(x = minmax, y = sin_relation, by.x = "customer", by.y = "customer.y", all.x = TRUE)


##########################
## Aggregate on unique customer basis
##########################
unique_cust <- minmax[,list(location[1],sum(totalValidOrders),stages[1], firstChannel[1],head(firstOrder,1),tail(lastOrder,1)),by=customer]
colnames(unique_cust) <- c("customer","location","totalOrders","stage","firstChannel","firstOrder","lastOrder")


unique_cust$lifespan_last_minus_last <- difftime(time1 = unique_cust$lastOrder,
                                 time2 = unique_cust$firstOrder,
                                 units = 'days')


##########################
## Compute average lifespan
##########################
average_lifespan <- unique_cust[totalOrders > 1, median(lifespan_last_minus_last),]

lifespanbycity = unique_cust[totalOrders >1 & stage == "Stage 3",median(lifespan_last_minus_last),by=location]

colnames(lifespanbycity) <- c("location","lifespan")
lifespanbycity$lifespan <- as.numeric(lifespanbycity$lifespan)

##########################
## Get orders and customers per month
##########################
sum(orders_data$isvalid)

orders_data$createdAtYM <- as.yearmon(orders_data$createdAt)


##########################
## Get it by first marketing channel
##########################

orders_data <- merge(x = orders_data, y = sin_relation, by.x = "customer", by.y = "customer.y", all.x = TRUE)


orders_per_user <- orders_data[isvalid==1,list(length(unique(customer)),sum(isvalid)),by=.(createdAtYM,location,firstChannel)]

colnames(orders_per_user) <- c("monthYear","location","firstChannel","customers","orders")
sum(orders_per_user$orders)

orders_per_user$orderperuser <- orders_per_user$orders/orders_per_user$customers

##########################
## Get revenue per order
##########################
aov <- orders_data[!is.na(GrossTotalBeforeDiscount),list(sum(GrossTotalBeforeDiscount),sum(isvalid)),by=.(createdAtYM,location,firstChannel)]
colnames(aov) <- c("monthYear","location","firstChannel","grossBeforeEur","orders")
aov$aov <- aov$grossBeforeEur/aov$orders

##########################
## Merge three points alltogether
##########################

orders_per_user <- merge(x = orders_per_user, y = aov, by = c("monthYear","location","firstChannel"),all.x = TRUE )
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
#orders_per_user$clv[which(is.na(orders_per_user$clv))] <- 0
orders_per_user[is.na(orders_per_user)] <- 0


write.csv(orders_per_user,"/home/dima/powerbi-share/R_outputs/clv.csv",row.names = F)
