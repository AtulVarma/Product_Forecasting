library(forecast)
library(lubridate)
library(DataCombine)
library(imputeTS)
library(plyr)
library(dplyr)
library(TTR)
library(graphics)
library(data.table)
#library(Quandl)
library(DMwR)
#####Reading CSV#####
setwd("C:\\Users\\atul\\Desktop")

d_tr<-read.csv('ExistingCustomersTransactionsData.csv')
d_demo<-read.csv('ExisitngCustomerDemographics.csv')

t_tr<-read.csv('NewCustomerTransactions_Segmentation.csv')
t_demo<-read.csv('NewCustomerDemographics_Segmentation.csv')
str(t_tr)
##### Report check on data #####
#####dimention check#####

dim(t_tr)#6220/7
dim(d_tr)#29982/7
dim(d_demo)#2379/5
dim(d_tr)#29982/7

#col/row check

colnames(d_tr)
colnames(t_tr)
colnames(d_demo)
colnames(t_demo)


str(d_tr)##cust2379,,product1862,productcat3
str(d_demo)##cusID2379,,DOB2251,,fam_size=4
summary(d_demo)
summary(d_tr)##sales(max=22638,min=.44)

unique(d_tr$Quantity)
unique(d_demo$Familysize)


##### now making factors #####

#d_tr$Quantity<-as.factor(d_tr$Quantity)#14 but not do facterize for further process
d_demo$Familysize<-as.factor(d_demo$Familysize)##4
t_demo$Familysize<-as.factor(t_demo$Familysize)

#####cheacking NAs#####
sum(is.na(d_demo))#0
sum(is.na(d_tr))##0

#####Merging Both DATA#####


data <- merge(d_tr,d_demo,by = 'CustomerID')
t_data <- merge(t_tr,t_demo,by = 'CustomerID')
ncol(data)##11
ncol(t_data)

#####Ploting for check outliers#####

par(mar=c(1,1,1,1))
#par(mfrow = c(1,1))
plot(d_tr$Sales)




data[data$Sales>10000,]## I asume these 14 pints are outliers
data[data$Sales<.9,]
data[data$ProductCategory=='Technology',]

odata<-data  
#data<-odata
data<-data[-c(data$Sales>10000),]#remove sles outliers
data<-data[-c(data$Sales<.9),]
sum(is.na(data))

####################################################

#####changing in date format and exrtracct #####
data$BillDate<-as.Date(data$BillDate,'%m/%d/%Y')
data$DOB<-as.Date(data$DOB,'%m/%d/%Y')

t_data$BillDate<-as.Date(t_data$BillDate,'%m/%d/%Y')
t_data$DOB<-as.Date(t_data$DOB,'%m/%d/%Y')


data$week <- week(as.Date(data$BillDate))
t_data$week <- week(as.Date(t_data$BillDate))
#data$week<-as.factor(data$week)

data$day <- weekdays(as.Date(data$BillDate))
t_data$day <- weekdays(as.Date(t_data$BillDate))
#data$day<-as.factor(data$day)


data$month <- month(as.Date(data$BillDate))
t_data$month <- month(as.Date(t_data$BillDate))
#data$month<-as.factor(data$month)

data$year <- year(as.Date(data$BillDate))
t_data$year <- year(as.Date(t_data$BillDate))
#data$year <-as.factor(data$year)

#data$birth <- year(as.Date(data$DOB))##we can also crate month and day col for coutmer to 
#give them theor birthday offer but here it no much require


##### Coverting DOB to AGE#####

age <- function(dob, age.day = today(), units = "years", floor = TRUE) {
  calc.age = interval(dob, age.day) / duration(num = 1, units = units)
  if (floor) return(as.integer(floor(calc.age)))
  return(calc.age)
}
data$age<-age(data$DOB)
t_data$age<-age(t_data$DOB)



str(t_data)
##### removing Unwanted Columns#####
#data$DOB<-NULL

colnames(data)
s<-c('CustomerID','BillNumber','BillDate','ProductID','ProductCategory','Sales','Quantity','gender','MaritalStatus','Familysize','week','day','month','year','age')

data1<-data[,s]
t_data1<-t_data[,s]


##### removing duplicates#####
data1 <- data1[!duplicated(data1),]
t_data1 <- t_data1[!duplicated(t_data1),]



library(data.table)
library(dplyr)
library(ggplot2)
#library(stringr)
#library(DT)
library(tidyr)
library(knitr)
library(rmarkdown)



######RFM stands for the three dimensions:#####
  
  # Recency - How recently did the customer purchase?
  # Frequency - How often do they purchase?
  # Monetary Value - How much do they spend?





# df_data <- tempdata %>% 
#   mutate(Quantity = replace(Quantity, Quantity<=0, NA),
#          UnitPrice = replace(UnitPrice, UnitPrice<=0, NA))
df_data<-data1
tdf_data<-t_data1

  
#####Doing Mutation on existing coustemer data#####

df_data <- df_data %>% 
  mutate(BillNumber=as.factor(BillNumber), ProductID=as.factor(ProductID), 
         BillDate=as.Date(BillDate, '%m/%d/%Y %H:%M'), CustomerID=as.factor(CustomerID))


#####Doing mutation on new customer data#####

tdf_data <- tdf_data %>% 
  mutate(BillNumber=as.factor(BillNumber), ProductID=as.factor(ProductID), 
         BillDate=as.Date(BillDate, '%m/%d/%Y %H:%M'), CustomerID=as.factor(CustomerID))






library(bindrcpp)

#####RFM on data extraction#####

df_RFM <- df_data %>% 
  group_by(CustomerID) %>% 
  summarise(recency=as.numeric(as.Date("2018-01-01")-max(BillDate)),
            frequenci=n_distinct(BillDate), monitery= sum(Sales)/n_distinct(BillDate)) 



tdf_RFM <- tdf_data %>% 
  group_by(CustomerID) %>% 
  summarise(recency=as.numeric(as.Date("2018-01-01")-max(BillDate)),
            frequenci=n_distinct(BillDate), monitery= sum(Sales)/n_distinct(BillDate))





kable(head(df_RFM))
#

#####Tenure calculation#####
df_RFM$Tenure <- data.frame(aggregate(year~CustomerID,data = data,FUN = "max"))$year+1 - data.frame(aggregate(year~CustomerID,data = data,FUN = "min"))$year 

tdf_RFM$Tenure <- data.frame(aggregate(year~CustomerID,data = t_data,FUN = "max"))$year+1 - data.frame(aggregate(year~CustomerID,data = t_data,FUN = "min"))$year 


#####now merging df_RFM and df_data#####

str(df_RFM)
rfmdata <- merge(x = df_RFM,y = df_data,by = 'CustomerID')

trfmdata <- merge(x = tdf_RFM,y = tdf_data,by = 'CustomerID')




##### Recency - How recently did the customer purchase#####
#it give ranking to customer here i seen highr rank less frequency

  
hist(df_RFM$recency,main = 'Customer Recency')

hist(tdf_RFM$recency,main = 'Customer Recency')

##### Frequency - How often do they purchase?#####
#####MAking some histogram#####

hist(df_RFM$frequenci, breaks =20 ,xlim = c(1,17))

hist(tdf_RFM$frequenci, breaks =20 ,xlim = c(1,17))

df_RFM[df_RFM$frequenci <5,]

par(mfrow = c(1,1))
df_RFM[df_RFM$frequenci <1,'frequenci']


par(mar=c(1,1,1,1))
#####Monetary Value - How much do they spend#####
# gives rank to cust high value means cust have good rank for revenue

#####Cheacking Skewness #####

hist(rfmdata$monitery, breaks = 50)##right skewed

hist(trfmdata$monitery, breaks = 50)##right skewed

#####filtering not required custmer where retail not making money much#####
rmm<-rfmdata[rfmdata$monitery <70 & rfmdata$frequenci<5,'Sales'] #below 70 olny 1 and 2 frequency have 
summary(rfmdata$monitery)

#####remove those customes#####
rfmdata<-rfmdata[-rmm,]

#####the data is right  skewed so  I am taking log of monitery #####

rfmdata$monitery <- log(rfmdata$monitery)
hist(df_RFM$monitery)







#####Calculating rfm Scores #####


# Install rfm from CRAN
#install.packages("rfm")

#Or the development version from GitHub
#install.packages("devtools")
devtools::install_github("rsquaredacademy/rfm")
# colnames(data1)

#
library(rfm)

#############################
colnames(rfmdata)
min(rfmdata$BillDate)##2014-01-03

min(trfmdata$BillDate)##2014-01-03

#####Taking ANalyss data limit#####

analysis_date <- lubridate::as_date('2017-12-31', tz = 'UTC')


analysis_tdate <- lubridate::as_date('2017-12-31', tz = 'UTC')


str(rfmdata)
str(trfmdata)


df_RFM$latest_date<- as.Date(data.frame(aggregate(BillDate~CustomerID,data = rfmdata,FUN = "max"))$BillDate)

tdf_RFM$latest_date<- as.Date(data.frame(aggregate(BillDate~CustomerID,data = trfmdata,FUN = "max"))$BillDate)


#####making CustomerID,BillDate and  Sales new df#### 

Sal_cId<-rfmdata[,c('CustomerID','Sales','BillDate')]
df_RFM<-merge(x = Sal_cId,y=df_RFM,by ='CustomerID' )
colnames(df_RFM)


tSal_cId<-trfmdata[,c('CustomerID','Sales','BillDate')]
tdf_RFM<-merge(x = tSal_cId,y=tdf_RFM,by ='CustomerID' )


library(sqldf)
#####RUNING sql Coamand for group by CustomerID #####

df_RFM <- sqldf('select CustomerID,recency,frequenci,monitery,Tenure,latest_date,sum(Sales) as Sales from df_RFM group by CustomerID')
rfm_result <-(rfm_table_customer_2(data = df_RFM,customer_id= CustomerID,n_transactions = frequenci,
                                    total_revenue = Sales, 
                                    analysis_date =analysis_date,latest_visit_date = latest_date))
View(rfm_result$rfm)




tdf_RFM <- sqldf('select CustomerID,recency,frequenci,monitery,Tenure,latest_date,sum(Sales) as Sales from tdf_RFM group by CustomerID')
trfm_result <-(rfm_table_customer_2(data = tdf_RFM,customer_id= CustomerID,n_transactions = frequenci,
                                   total_revenue = Sales, 
                                   analysis_date =analysis_tdate,latest_visit_date = latest_date))
str(trfm_result$rfm)


#View(trfm_result$rfm)


#####droping Sales and Recency col from df_rfm and tdf_rfm#####



df_RFM$Sales<-NULL
df_RFM$recency<-NULL

tdf_RFM$Sales<-NULL

tdf_RFM$recency<-NULL

colnames(tdf_RFM)

rfmresult<-rfm_result$rfm
str(rfmresult)
str(df_RFM)

rfmresult<-as.data.frame(rfmresult)

trfmresult<-trfm_result$rfm
str(trfmresult)

trfmresult<-as.data.frame(trfmresult)





colnames(rfmresult)

colnames(df_RFM)



colnames(rfmresult)[1]<-'CustomerID'

colnames(trfmresult)[1]<-'CustomerID'

str(df_RFM$CustomerID)
str(df_RFM)
goal<-merge(rfmresult,df_RFM,by = 'CustomerID')
colnames(goal)

str(tdf_RFM$CustomerID)

#tdf_RFM$CustomerID<-as.factor(tdf_RFM$CustomerID)




t_goal<-merge(trfmresult,tdf_RFM,by = "CustomerID")

colnames(t_goal)


str(trfmresult)
str(tdf_RFM)
###########################################################################################
#####Now working on original features#####
###########################################################################################


colnames(data1)

##### groupby the imp columns #####

group <- data1 %>% select(CustomerID, age, gender, MaritalStatus, Familysize, ProductCategory,
         Sales, Quantity,week,day,month,year)

t_group <- t_data1 %>%
  select(CustomerID, age, gender, MaritalStatus, Familysize, ProductCategory,
         Sales, Quantity,week,day,month,year)


dim(group)

#####changing qty col to num for aggregation process#####

group$Quantity<-as.numeric(as.character(group$Quantity))

t_group$Quantity<-as.numeric(as.character(t_group$Quantity))


#####tronsforming product category col to respective columns#####

group_cat <- group %>% group_by(CustomerID, age, gender, MaritalStatus,Familysize, ProductCategory,Sales, Quantity,week,day,month,year) %>% 
  summarise(sum_qty = sum(Quantity)) %>%
  spread(ProductCategory, sum_qty)

group_cat<-data.frame(group_cat)

colnames(group_cat)



t_group_cat <- t_group %>% group_by(CustomerID, age, gender, MaritalStatus,Familysize, ProductCategory,Sales, Quantity,week,day,month,year) %>% 
  summarise(sum_qty = sum(Quantity)) %>%
  spread(ProductCategory, sum_qty)

t_group_cat<-data.frame(t_group_cat)


#####now furniture ,technology,and office_supply have NA's , bcoz of transformation#####

group[is.na(group)] <- 0

t_group[is.na(t_group)] <- 0



#####now working on Grouping with sales#####


group_rev <- group %>% group_by(CustomerID, age, gender, MaritalStatus,ProductCategory,Familysize,week,day,month,year) %>% 
  summarise(sum_qty = sum(Quantity), Revenue = sum(Sales)) %>% spread(ProductCategory, sum_qty)

str(group_rev)
group_rev<-data.frame(group_rev)
colnames(group_rev)

group_rev[is.na(group_rev)] <- 0

sum(is.na(group_rev))


t_group_rev <- t_group %>% group_by(CustomerID, age, gender, MaritalStatus,ProductCategory,Familysize,week,day,month,year) %>% 
  summarise(sum_qty = sum(Quantity), Revenue = sum(Sales)) %>% spread(ProductCategory, sum_qty)

t_group_rev<-data.frame(t_group_rev)
colnames(t_group_rev)

group_rev[is.na(group_rev)] <- 0



#####For Final transform working with sql cammand ######

library(sqldf)

######Summerise #####
colnames(group_rev)[12]<-'office'
colnames(group_rev)
finalgroup <- sqldf('select CustomerID, age, gender, MaritalStatus, Familysize,week,day,month,year,
                    sum(Revenue) as Revenue, 
                    sum(Furniture) as Furniture, sum(Office) as Office, 
                    sum(Technology) as Technology from group_rev group by CustomerID')


colnames(t_group_rev)[12]<-'office'
colnames(t_group_rev)
finalgroup_t <- sqldf('select CustomerID, age, gender, MaritalStatus, Familysize,week,day,month,year,
                    sum(Revenue) as Revenue, 
                    sum(Furniture) as Furniture, sum(Office) as Office, 
                    sum(Technology) as Technology from t_group_rev group by CustomerID')


dim(finalgroup)
dim(finalgroup_t)
#####Basic and RFM done #####

#####Now Merging The goal and finalgroup######
colnames(goal)
colnames(finalgroup)


#####Now removing '(amount and transaction) and arranging the columns#####

finalgoal <- sqldf('select CustomerID,latest_date,recency_score,frequency_score,monetary_score,rfm_score,recency_days,frequenci,monitery,Tenure  from goal')
colnames(finalgoal)
colnames(finalgroup)

finalgoal_t <- sqldf('select CustomerID,latest_date,recency_score,frequency_score,monetary_score,rfm_score,recency_days,frequenci,monitery,Tenure  from t_goal')
colnames(finalgoal_t)
colnames(finalgroup_t)

#####Merging Finalgroup and FInalgoal#####
finaldata<-merge(x = finalgroup, y = finalgoal, by = "CustomerID", all.x = TRUE)

finaldata_t<-merge(x = finalgroup_t, y = finalgoal_t, by = "CustomerID", all.x = TRUE)


str(finaldata)
finaldata$day<-as.factor(finaldata$day)
finaldata_t$day<-as.factor(finaldata_t$day)

######Now making CUSTOMERID as RowNames#####

row.names(finaldata) <- finaldata$CustomerID
row.names(finaldata_t) <- finaldata_t$CustomerID

train<-finaldata##Keep it safe
test<-finaldata_t

finaldata$CustomerID<-NULL
finaldata_t$CustomerID<-NULL
################################################################
#####Final feature data#####


# finaldata
# finaldata_t

#####DUMMYFICATION#####


library(dummies)
clust_<-finaldata
tclust_<-finaldata_t

clust_ <- cbind(clust_, dummy(clust_$gender))
clust_<-cbind(clust_,dummy(clust_$day))
clust_<-cbind(clust_,dummy(clust_$MaritalStatus))



tclust_ <- cbind(tclust_, dummy(tclust_$gender))
tclust_<-cbind(tclust_,dummy(tclust_$day))
tclust_<-cbind(tclust_,dummy(tclust_$MaritalStatus))


#####Removing Unwanted Columns #####

clust_$gender <- NULL
clust_$day<-NULL
clust_$MaritalStatus <- NULL
clust_$latest_date <- NULL ##removing date for clustering

tclust_$gender <- NULL
tclust_$day<-NULL
tclust_$MaritalStatus <- NULL
tclust_$latest_date <- NULL ##removing date for clustering

#####converting familysize in numeric for scaling #####

clust_$Familysize<-as.numeric(clust_$Familysize)
tclust_$Familysize<-as.numeric(tclust_$Familysize)

#row.names(clust_)<-NULL
##################################################################################
##################################################################################


##############
##### 80/20 Rule #####
##############
practo<-clust_

t_practo<-tclust_


customers <- practo[order(-practo$monitery),]## we taking order with monitery
tcustomers<- practo[order(-t_practo$monitery),]## we taking order with monitery

# Apply Pareto Principle (80/20 Rule)

pareto.cutoff <- 0.8 * sum(customers$monitery)#fix cutoff refrence on monitery
customers$pareto <- ifelse(cumsum(customers$monetary) <= pareto.cutoff, "Top 20%", "Bottom 80%")##catogerize top 20 and 80%
customers$pareto <- factor(customers$pareto, levels=c("Top 20%", "Bottom 80%"), ordered=TRUE)
levels(customers$pareto)

round(prop.table(table(customers$pareto)), 2)#1>- Top 20%  and 0<-Bottom 80% 

remove(pareto.cutoff)



##### we taking order with monitery#####

##### Apply Pareto Principle (80/20 Rule) on new coustmer data#####


pareto.cutoff <- 0.8 * sum(tcustomers$monitery)#fix cutoff refrence on monitery
tcustomers$pareto <- ifelse(cumsum(tcustomers$monetary) <= pareto.cutoff, "Top 20%", "Bottom 80%")##catogerize top 20 and 80%
tcustomers$pareto <- factor(tcustomers$pareto, levels=c("Top 20%", "Bottom 80%"), ordered=TRUE)
levels(tcustomers$pareto)

round(prop.table(table(tcustomers$pareto)), 2)#1>- Top 20%  and 0<-Bottom 80% 

remove(pareto.cutoff)

# #now data in CID order
# customers <- customers[order(customers[CustomerID,]),]
# tcustomers <- tcustomers[order(tcustomers$CustomerID),]

View(customers)

###################
##### Preprocess data #####
###################
colnames(customers)

##### transforming log for sparse columns#####

customers$recency.log <- log(customers$recency_days)
customers$frequency.log <- log(customers$frequenci)
customers$monetary.log <- customers$monitery + 0.1 # can't take log(0), so add a small value to remove zeros
customers$monetary.log <- log(customers$monetary.log)
dim(customers)
names(customers)

tcustomers$recency.log <- log(tcustomers$recency_days)
tcustomers$frequency.log <- log(tcustomers$frequenci)
tcustomers$monetary.log <- tcustomers$monitery + 0.1 # can't take log(0), so add a small value to remove zeros
tcustomers$monetary.log <- log(tcustomers$monetary.log)

##### Z-Scaling#####

customers$recency.z <- scale(customers$recency.log, center=TRUE, scale=TRUE)
customers$frequency.z <- scale(customers$frequency.log, center=TRUE, scale=TRUE)
customers$monetary.z <- scale(customers$monetary.log, center=TRUE, scale=TRUE)
dim(customers)
names(customers)

tcustomers$recency.z <- scale(tcustomers$recency.log, center=TRUE, scale=TRUE)
tcustomers$frequency.z <- scale(tcustomers$frequency.log, center=TRUE, scale=TRUE)
tcustomers$monetary.z <- scale(tcustomers$monetary.log, center=TRUE, scale=TRUE)
dim(tcustomers)
names(tcustomers)





##################
##### Visualize data #####
##################

#install.packages("ggplot2")
#dev.off()

library(ggplot2)
library(scales)
colnames(customers)
# Original scale

par(mar=c(1,1,1,1))
scatter.1 <- ggplot(customers, aes(x = frequenci, y = monitery))
scatter.1 <- scatter.1 + geom_point(aes(colour = recency_days, shape = 'pareto'))
scatter.1 <- scatter.1 + scale_shape_manual(name = "80/20 Designation", values=c(17,16))
scatter.1 <- scatter.1 + scale_colour_gradient(name="Recency\n(Days since Last Purchase))")
scatter.1 <- scatter.1 + scale_y_continuous(label=dollar)
scatter.1 <- scatter.1 + xlab("Frequency ")
scatter.1 <- scatter.1 + ylab("Monetary Value of Customer")
scatter.1

##### Log-transformed#####
scatter.2 <- ggplot(customers, aes(x = frequency.log, y = monetary.log))
scatter.2 <- scatter.2 + geom_point(aes(colour = recency.log, shape = 'pareto'))
scatter.2 <- scatter.2 + scale_shape_manual(name = "80/20 Designation", values=c(17,16))
scatter.2 <- scatter.2 + scale_colour_gradient(name="Log-transformed Recency")
scatter.2 <- scatter.2 + xlab("Log-transformed Frequency")
scatter.2 <- scatter.2 + ylab("Log-transformed Monetary Value of Customer")
scatter.2


# How many customers are represented by the two data points in the lower left-hand corner of the plot? 18
# delete <- subset(customers, monetary.log < 0)
# no.value.custs <- unique(customers$CustomerID)
# delete2 <- subset(finalgoal, CustomerID %in% no.value.custs)
# delete2 <- delete2[order(delete2$CustomerID, delete2$BillDate),]
# remove(delete, delete2, no.value.custs)

##### Scaled variables#####
scatter.3 <- ggplot(customers, aes(x = frequenci, y = monitery))
scatter.3 <- scatter.3 + geom_point(aes(colour = recency_days, shape = 'pareto'))
scatter.3 <- scatter.3 + scale_shape_manual(name = "80/20 Designation", values=c(17,16))
scatter.3 <- scatter.3 + scale_colour_gradient(name="Z-scored Recency")
scatter.3 <- scatter.3 + xlab("Z-scored Frequency")
scatter.3 <- scatter.3 + ylab("Z-scored Monetary ")


scatter.3

#remove(scatter.1, scatter.2, scatter.3)

#####now taking onlu log transform columnsonly#####
colnames(customersall)
customersall<-customers##keep safe all tansform
t_customersall<-tcustomers

customers[,c(10:12,33:35)]<-NULL
tcustomers[,c(10:12,33:35)]<-NULL


###################################################################################
##################################################################################
#####CLUSTERING#####

colnames(customers)
row.names(customers)


customers$pareto<-NULL
tcustomers$pareto<-NULL##not keeping the pareto score

# preprocessed<-scale(customers)
# tpreprocessed<-scale(tcustomers)

#####Scalling data#####

customers<-scale(customers)
tcustomers<-scale(tcustomers)


preprocessed<-customers
tpreprocessed<-tcustomers

#Changing customers o data frame for loop run

# customers<-as.data.frame(customers)


colnames(preprocessed)

j <-12# specify the maximum number of clusters you want to try out

models <- data.frame(k=integer(),
                     tot.withinss=numeric(),
                     betweenss=numeric(),
                     totss=numeric(),
                     rsquared=numeric())

for (k in 5:j) {
  
  print(k)
  
  ##### Run kmeans#####
  # nstart = number of initial configurations; the best one is used
  # $iter will return the iteration used for the final model
  output <- kmeans(preprocessed, centers = k, nstart = 20)
  
  # Add cluster membership to customers dataset
  var.name <- paste("cluster", k, sep="_")
  customers[,(var.name)] <- output$cluster
  customers[,(var.name)] <- factor(customers[,(var.name)], levels = c(1:k))
  
  ##### Graph clusters#####
  cluster_graph <- ggplot(customers, aes(x = frequency.log, y = monetary.log))
  cluster_graph <- cluster_graph + geom_point(aes(colour = customers[,(var.name)]))
  colors <- c('red','orange','green3','deepskyblue','blue','darkorchid4','violet','pink1','tan3','black','purple','pink')
  cluster_graph <- cluster_graph + scale_colour_manual(name = "Cluster Group", values=colors)
  cluster_graph <- cluster_graph + xlab("Log-transformed Frequency")
  cluster_graph <- cluster_graph + ylab("Log-transformed Monetary Value of Customer")
  title <- paste("k-means Solution with", k, sep=" ")
  title <- paste(title, "Clusters", sep=" ")
  cluster_graph <- cluster_graph + ggtitle(title)
  print(cluster_graph)
  
  # Cluster centers in original metrics
  library(plyr)
  print(title)
  cluster_centers <- ddply(customers, .(customers[,(var.name)]), summarize,  
                           monetary=round(median(monitery),2),  # use median b/c this is the raw, heavily-skewed data
                           frequency=round(median(frequenci),1), 
                           recency=round(median(recency_days), 0),
                           Revenue=round(median(Revenue), 0))
  names(cluster_centers)[names(cluster_centers)=="customers[, (var.name)]"] <- "Cluster"
  print(cluster_centers)
  cat("\n")
  cat("\n")
  
  # Collect model information
  models[k,("k")] <- k
  models[k,("tot.withinss")] <- output$tot.withinss # the sum of all within sum of squares
  models[k,("betweenss")] <- output$betweenss
  models[k,("totss")] <- output$totss # betweenss + tot.withinss
  models[k,("rsquared")] <- round(output$betweenss/output$totss, 3) # percentage of variance explained by cluster membership
  assign("models", models, envir = .GlobalEnv) 
  
  remove(output, var.name, cluster_graph, cluster_centers, title, colors)
  
}


#Analysys on cluster  Eroers for any decidsion

# models$k #  1 2 3 4 5 6 7
# models$tot.withinss
# #1] 16997845245  6940066172  3904063091  2387625007 [5]  1742569941  1441585147  1249805802
# 
# models$betweenss
# #[1] 9.902954e-03 1.005778e+10 1.309378e+10 1.461022e+10[5] 1.525528e+10 1.555626e+10 1.574804e+10
# 
# models$totss
# models$rsquared # 0.000 0.592 0.770 0.860 0.897 0.915 0.926
# 
# 
# 
# 
# 
# models$k# 8 9 10 11 12 13 14
# models$tot.withinss
# #1098325112  806198871  589033415  566537584 [13]  544866499  466743941
# 
# models$betweenss
# #15851175226 15899520134 16191646374 16408811830 16431307662   16452978746 16531101305
# 
# models$totss
# models$rsquared # 0.933 0.935 [10] 0.953 0.965 0.967 0.968 0.973



# train_clust_6<-customers[,1:29]
# train_clust_7<-customers[,c(1:28,30)]
# train_clust_8<-customers[,c(1:28,31)]
# train_clust_9<-customers[,c(1:28,32)]
# train_clust_10<-customers[,c(1:28,33)]##this is the best cluster according to uper errore analysys
# train_clust_6<-customers[,c(1:28,34)]

aggregate(preprocessed,by=list(train_clust_10$cluster_10),FUN=mean)

# preprocessed<-as.data.frame(preprocessed)
# tpreprocessed<-as.data.frame(tpreprocessed)
# 
# train_clust_6<-customers[,1:29]
# train_clust_7<-customers[,c(1:29,30)]
# train_clust_8<-customers[,c(1:29,31)]
# train_clust_9<-customers[,c(1:29,32)]
# train_clust_10<-customers[,c(1:29,33)]
# train_clust_6<-customers[,c(1:29,34)]


# colnames(customers)
# train_clust_10<-customers[,c(1:28,36)]
# colnames(tcustomers)
# test<-tpreprocessed
# colnames(test)
# colnames(train_clust_10)
# remove(k)
# 
# # Checking the cluster stability
# set.seed(013)
# # Getting the cluster numbers
# x <- train_clust_10[,29]
# # Building the clusters on 90 % of data
# fit2 <- kmeans(preprocessed[1:2140,], 10,nstart = 20)
# 
# y <- fit2$cluster
# x1<-cbind(row.names(preprocessed), train_clust_10[,29])
# 
# x1<- as.data.frame(x1)
# colnames(x1)
# rownames(x1)<-x1$V1
# x1$V1<-NULL
# x1$V2<-as.numeric(x1$V2)
# 
# 
# #RandIndex Stablity
# #install.packages("fossil")
# 
# library(fossil)
# rand.index(x1[1:2140,], y)###96%
# 
# 
# # Checking whether the same data points are falling into same cluster 
# # when we cluster on all the data or 90% of data.
# 
# #adjustedRandIndex
# library(mclust)
# adjustedRandIndex(x1[1:2140,], y)##72%

# 
# #Cluster predicyion Newcustomer data
# 
# preprocessed<-as.data.frame(preprocessed)
# 
# # library(factoextra)
# # # Visualize the clisters based on first 2 principal components
# # fviz_cluster(x1, data = train_clust_10[1:28])





#############cluster Manual##################################
#################################################################


#############################[K==10]##############################
set.seed(35453)

fit10<-kmeans(mydata,centers = 10,nstart = 20)


#study the model
fit10

fit10$cluster
fit10$withinss
fit10$betweenss
fit10$cluster
fit10$tot.withinss
fit10$centers

##### get cluster means#####
aggregate(preprocessed,by=list(fit10$cluster),FUN=mean)



################################

#####3D Clusters#####
###############################
#fit10$cluster<-as.factor(fit10$cluster)
unique(fit10$cluster)
colors <- c('red','orange','green3','deepskyblue','blue','darkorchid4','violet','pink1','tan3','black','blue')

library(car)
library(rgl)


scatter3d(x = customers$frequency.log,
          y = customers$monetary.log,
          z = customers$recency.log,
          groups = fit10$cluster,
          xlab = "Frequency (Log-transformed)",
          ylab = "Monetary Value (log-transformed)",
          zlab = "Recency (Log-transformed)",
          surface.col = colors,
          axis.scales = FALSE,
          surface = TRUE, # produces the horizonal planes through the graph at each level of monetary value
          fit = "smooth",
          #     ellipsoid = TRUE, # to graph ellipses uses this command and set "surface = " to FALSE
          grid = TRUE,
          axis.col = c("black", "black", "black"))

remove(colors)











##### append cluster label to the actual data frame#####


#####predicting cluster to newcustomer data#####

library(flexclust)
a23 <- as.kcca(fit10, data = preprocessed)

testpreds <- predict(a23, tpreprocessed)

testpreprocessed<- data.frame(tpreprocessed, testpreds)
trainpreprocessed<- data.frame(preprocessed,fit10$cluster)
nrow(testpreprocessed)


#####aapend target on non scale data#####
colnames(preprocessed)
colnames(customersall)
colnames(t_customersall)
mydata<-customersall[,c(1:9,13:26,27:28,30:32)]
mydatatest<-t_customersall[,c(1:9,13:26,27:28,30:32)]

colnames(mydata)
colnames(mydatatest)
library(flexclust)
b23 <- as.kcca(fit10, data = mydata)

testpreds2 <- predict(b23, mydatatest)



mydatatest<- data.frame(mydatatest, testpreds2)
mydata<- data.frame(mydata,fit10$cluster)
nrow(mydatatest)





#####writing files#####

write.csv(trainpreprocessed,"preprocessed_10.csv")
write.csv(testpreprocessed,"testpreprocessed_10.csv")


write.csv(mydata,"mydata.csv")
write.csv(mydatatest,"mydatatest.csv")
# K-means:  Determine number of clusters by considering the withinness measure
wss <- 0
for (i in 1:15) {
  wss[i] <- sum(kmeans(preprocessed,centers=i)$withinss)
}

# Ploting the within sum of square error for different clusters
plot(1:15, wss,
     type="b",
     xlab="Number of Clusters",
     ylab="Within groups sum of squares")


test_datapoint <- preprocessed[sample(1:nrow(preprocessed),1),]

closest.cluster <- function(x) {
  cluster.dist <- apply(fit10$centers, 1, function(y) sqrt(sum((x-y)^2)))
  print(cluster.dist)
  return(which.min(cluster.dist)[1])
}

# Predicting which cluster the new data point belongs to based on the distance.
closest.cluster(test_datapoint)



######################################################################

library(ggplot2)
library(scales)

# Graph variance explained by number of clusters
r2_graph <- ggplot(models, aes(x = k, y = rsquared))
r2_graph <- r2_graph + geom_point() + geom_line()
r2_graph <- r2_graph + scale_y_continuous(labels = scales::percent)
r2_graph <- r2_graph + scale_x_continuous(breaks = 1:j)
r2_graph <- r2_graph + xlab("k (Number of Clusters)")
r2_graph <- r2_graph + ylab("Variance Explained")
r2_graph

# Graph within sums of squares by number of clusters
# Look for a "bend" in the graph, as with a scree plot
ss_graph <- ggplot(models, aes(x = k, y = tot.withinss))
ss_graph <- ss_graph + geom_point() + geom_line()
ss_graph <- ss_graph + scale_x_continuous(breaks = 1:j)
ss_graph <- ss_graph + scale_y_continuous(labels = scales::comma)
ss_graph <- ss_graph + xlab("k (Number of Clusters)")
ss_graph <- ss_graph + ylab("Total Within SS")
ss_graph

remove(j, r2_graph, ss_graph)


#########################################################################



# #######################
##### Plot clusters in 3D #####
# #######################
# 
colors <- c('red','orange','green3','deepskyblue','blue','darkorchid4','violet','pink1','tan3','black','blue')
str(customers)
str(fit10$cluster)

library(car)
library(rgl)
fit10$cluster<-as.factor(fit10$cluster)
colnames(customers)
scatter3d(x = customers$frequency.log,
          y = customers$monetary.log,
          z = customers$recency.log,
          groups = fit10$cluster,
          xlab = "Frequency (Log-transformed)",
          ylab = "Monetary Value (log-transformed)",
          zlab = "Recency (Log-transformed)",
          surface.col = colors,
          axis.scales = FALSE,
          surface = TRUE, # produces the horizonal planes through the graph at each level of monetary value
          fit = "smooth",
          #     ellipsoid = TRUE, # to graph ellipses uses this command and set "surface = " to FALSE
          grid = TRUE,
          axis.col = c("black", "black", "black"))

remove(colors)

names(customers)
#########################################################






#####  Other Clustrs #####
#1

library(fpc)
pamk.best <- pamk(preprocessed)
cat("number of clusters estimated by optimum average silhouette width:", pamk.best$nc, "\n")
plot(pam(preprocessed, pamk.best$nc))
# number of clusters estimated by optimum average silhouette width: 2

# 2################################
# # we could also do:
# library(fpc)
# asw <- numeric(20)
# for (k in 2:15)
#   asw[[k]] <- pam(preprocessed, k) $ silinfo $ avg.width
# k.best <- which.max(asw)
# cat("silhouette-optimal number of clusters:", k.best, "\n")
# # 10 clusters
########################################
# 
# require(vegan)
# fit <- cascadeKM(scale(preprocessed, center = TRUE,  scale = TRUE), 1, 10, iter = 100)
# plot(fit, sortg = TRUE, grpmts.plot = TRUE)
# calinski.best <- as.numeric(which.max(fit$results[2,]))
# cat("Calinski criterion optimal number of clusters:", calinski.best, "\n")
# #  clusters!=2

###############################################

# #
# library(mclust)
# # Run the function to see how many clusters
# # it finds to be optimal, set it to search for
# # at least 1 model and up 20.
# m_clust <- Mclust(as.matrix(sclust), G=1:20)
# m.best <- dim(m_clust$z)[2]
# cat("model-based optimal number of clusters:", m.best, "\n")
# # model-based optimal number of clusters: 1
# plot(m_clust)

#####################################################################
# 
# install.packages("apcluster")
# library(apcluster)
# ##it use montycharlo optimization
# ap_clust <- apcluster(negDistMat(r=2), preprocessed)
# cat("optimal number of clusters:", length(ap_clust@clusters), "\n")
# # 25 clusters
# heatmap(ap_clust)
# plot(ap_clust, sclust)
#######################################################
#takes long time
# library(cluster)
# fit_C<-clusGap(preprocessed, kmeans, 10, B = 100, verbose = interactive())
# 
# 
# # S3 method for clusGap
# print(preprocessed, method = "firstSEmax", SE.factor = 1)#SE.Favtor=Determining the optimal number of clusters,
# 
# # S3 method for clusGap
# plot(sclust, type = "b", xlab = "k", ylab = expression(Gap[k]),
#      main = NULL, do.arrows = TRUE,
#      arrowArgs = list(col="red3", length=1/16, angle=90, code=3))

#####NB CLUST#####

ibrary(NbClust)
colnames(customers)


NB<-customers
NB[,c(35:44,18,21,32:34,27)]<-NULL
colnames(NB)


NB_best <- NbClust(NB, diss = NULL,  distance = "euclidean", min.nc=2, max.nc=10, 
                  method = "kmeans", index = "all")
hist(NB_best$Best.nc[1,], breaks = max(na.omit(NB_best$Best.nc[1,])))

#####NB_CLUST gives 2 and 10 is the best cluster size#####

# 
# fit3 <- kmeans(preprocessed, 2,nstart = 20)
# 
# fit4 <- kmeans(preprocessed[1:2140,], 2,nstart = 20)
# #
#  x<- fit3$cluster
#  y<- fit4$cluster
#  
# x1<-cbind(row.names(preprocessed), train_clust_10[,29])
#
# x1<- as.data.frame(x1)
# colnames(x1)
# rownames(x1)<-x1$V1
# x1$V1<-NULL
# x1$V2<-as.numeric(x1$V2)
#
#
# #RandIndex Stablity
# #install.packages("fossil")
#
# library(fossil)
 #rand.index(x[1:2140], y)###90%
#
#
# # Checking whether the same data points are falling into same cluster
# # when we cluster on all the data or 90% of data.
#
# #adjustedRandIndex
#  library(mclust)
# adjustedRandIndex(x[1:2140], y)##81%

 
 
 
 #######################################################################################
 
#####Now working on Decisio Tree And XGB#####
 
 ######################################################################################

str(trainpreprocessed)
str(terrain.colors)

d<-trainpreprocessed
v<-testpreprocessed



# d$Familysize<-as.factor(d$Familysize)
# v$Familysize<-as.factor(v$Familysize)

d$week<-NULL
v$week<-NULL

d$fit10.cluster<-as.factor(d$fit10.cluster)
v$testpreds<-as.factor(v$testpreds)

str(d)

colnames(v)[28]<-'fit10.cluster'

###Use caret package to build XGboost

xgb_grid_1 = expand.grid(
  nrounds = 100,
  eta = c(0.01, 0.001, 0.0001),
  max_depth = c(2, 4, 6, 8, 10),
  gamma = 1,
  colsample_bytree=0.7,
  min_child_weight=2,
  subsample=0.9
)

# pack the training control parameters
xgb_trcontrol_1 = trainControl(
  method = "cv",
  number = 3
)

# train the model for each parameter combination in the grid, 
#   using CV to evaluate
xgb_train_1 = train(
  fit10.cluster~., data=d,
  trControl = xgb_trcontrol_1,
  tuneGrid = xgb_grid_1,
  method = "xgbTree"
)


xgb_train_1
summary(xgb_train_1)

xgb_train_1$finalModel
plot(xgb_train_1)##gives the max dpth vs accuracy plot
library(xgboost)
xgb.importance(colnames(train),xgb_train_1$finalModel)#it gives the gain,cover and how much frequently usees of a feature in data 


pred_Train1 = predict(xgb_train_1,newdata=d)
pred_Test1 = predict(xgb_train_1, newdata=v)

confusionMatrix(d$fit10.cluster,pred_Train1)
confusionMatrix(v$testpreds,pred_Test1)

#####RandomFOrest#####

control <- trainControl(method="cv", number=3)
set.seed(1235869)
tunegrid <- expand.grid(mtry=c(1:15))
rf_gridsearch <- train(fit10.cluster~ ., data=d, method = "rf",
                       trControl=control,
                       tuneGrid = tunegrid)


summary(rf_gridsearch)
rf_gridsearch$finalModel
# Predict on the train data
preds_train_rf1 <- predict(rf_gridsearch)
confusionMatrix(preds_train_rf1, d$fit10.cluster)


# Store predictions from the model
preds_rf1 <- predict(rf_gridsearch, v)

confusionMatrix(preds_rf1, v$testpreds)






#########Decision Tree#########################################################

#####On  Scaled data#####


#install.packages("C50")
library(C50)
set.seed(0035)


dtC50= C5.0(fit10.cluster ~ ., data = d, rules=T)##rules T for gives the rules of model
summary(dtC50)
C5imp(dtC50, pct=T)
pred_Train1 = predict(dtC50,newdata=d)#here type="raw" it will give prediction in 1 or 0
pred_Test1 = predict(train.rpart, newdata=v)#
C5.0Control(dtC50)

confusionMatrix(d$fit10.cluster,pred_Train1)
confusionMatrix(v$fit10.cluster,pred_Test1)

plot(dtC50,main="Classification Tree for Customers Class",margin=0.15,uniform=TRUE)
###############################


#####Now on UNSCALED DATA#####
set.seed(0035)


dtC50= C5.0(fit10.cluster ~ ., data = mydata, rules=T)##rules T for gives the rules of model
summary(dtC50)
C5imp(dtC50, pct=T)
pred_Train1 = predict(dtC50,newdata=mydata)#here type="raw" it will give prediction in 1 or 0
pred_Test1 = predict(train.rpart, newdata=mydatatest)#
C5.0Control(dtC50)

confusionMatrix(d$fit10.cluster,pred_Train1)
confusionMatrix(v$fit10.cluster,pred_Test1)

plot(dtC50,main="Classification Tree for Customers Class",margin=0.15,uniform=TRUE)




###############################C%.0 On unscale data############

colnames(mydatatest)[29]<-'fit10.cluster'

mydata$fit10.cluster<-as.factor(mydata$fit10.cluster)
mydatatest$fit10.cluster<-as.factor(mydatatest$fit10.cluster)

str(mydata)

colnames(mydata)
#Grid Search using c5.0
tc <- trainControl("cv",10)
c50Grid <- expand.grid(.trials = c(1:3, (1:10)*10),
                       .model = c("tree", "rules"),
                       .winnow = c(TRUE, FALSE))
train.c50 <- train(fit10.cluster~.,mydata, method="C5.0",trControl=tc,tuneGrid=c50Grid)

summary(train.c50)
pred_Train1_tc50 = predict(train.c50,newdata=mydata, type="raw")
pred_Test1_tc50 = predict(train.c50, newdata=mydatatest, type="raw")

confusionMatrix(mydata$fit10.cluster,pred_Train1_tc50)
confusionMatrix(mydatatest$fit10.cluster,pred_Test1_tc50)
str(mydatatest)


###
#####################################################
#####Now filters the clusters####
nrow(mydata[mydata$fit10.cluster==6,])
C_6<-mydata[mydata$fit10.cluster==6,]
C_7<-mydata[mydata$fit10.cluster==7,]
C_8<-mydata[mydata$fit10.cluster==8,]
C_9<-mydata[mydata$fit10.cluster==9,]
C_10<-mydata[mydata$fit10.cluster==10,]
C_5<-mydata[mydata$fit10.cluster==5,]
C_4<-mydata[mydata$fit10.cluster==4,]
C_3<-mydata[mydata$fit10.cluster==3,]
C_2<-mydata[mydata$fit10.cluster==2,]
C_1<-mydata[mydata$fit10.cluster==1,]

unique(mydata$Tenure)
