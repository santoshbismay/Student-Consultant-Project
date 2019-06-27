tstart.time <- Sys.time()

# Packages required -------------------------------------------------------

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(data.table)
library(forecast)
library(tsintermittent)


# Importing sales data ----------------------------------------------------------


sales18 <- read_excel("2018 Sales.xlsx") %>% as_tibble()
 sales18$year <- "2018"
# 
# 
# #Sales17

sales17 <- read_excel("2017 Sales.xlsx") %>% as_tibble()
 sales17$year <- "2017"
# 
# 
# #Sales16

sales16 <- read_excel("2016 Sales.xlsx") %>% as_tibble()
 sales16$year <- "2016"

# path_sales15 <- "C:\\Coursework\\Sem 2\\Grad Case Study\\Milacron Data Files\\S&Q\\sales15.rds"
# sales15 <- readRDS(path_sales15) %>% as_tibble()
# 
# path_sales14 <- "C:\\Coursework\\Sem 2\\Grad Case Study\\Milacron Data Files\\S&Q\\sales14.rds"
# sales14 <- readRDS(path_sales14) %>% as_tibble()

#Combining  all the sales data into one data frame
sales_t <- rbind(sales16, sales17, sales18)


##---------------Importing other data------------------------
itemrecords <- read_excel("10040 Item Records 01.29.2019.xlsx")
inventory <- read_excel("All Inventory 29 January 2019.xlsx")

#Variable conversions ------------------------------------------------------

for(i in seq(1,9,1)) {
  sales_t[[i]] <- as.factor(sales_t[[i]])
}

for(i in seq(13,21,1)) {
  sales_t[[i]] <- as.factor(sales_t[[i]])
}

for(i in seq(26,28,1)) {
  sales_t[[i]] <- as.factor(sales_t[[i]])
}


# glimpse(sales_t)

sales <- sales_t
sales_t <- sales %>% select(OrTy, Order_Number, year, Order_Date, Actual_Ship_Date,
                            Legacy_Part_Number, Sls_Cd2, Sls_Cd5, Sales_Cat7,
                            Quantity_Ordered, Unit_Price, Unit_Cost)

#Removing parts where order quantity is 0 in all 3 years
sales_t <- sales_t %>%
  filter(Quantity_Ordered > 0)
summary(sales_t)




# Calculating Monthly Co-eff of Cariation in Sales ------------------------

sales_t$month <- months.Date(sales_t$Order_Date)
sales_t$quarter <- quarters.Date(sales_t$Order_Date)

#Monthly
sales_lpn_month <- sales_t %>% 
  group_by(Legacy_Part_Number, month, year) %>% 
  summarise(quantity_m = sum(Quantity_Ordered))

sales_lpn_month$month <- paste(sales_lpn_month$month, sales_lpn_month$year, sep = "_")

level_month <- c('January_2016','February_2016','March_2016','April_2016','May_2016','June_2016','July_2016','August_2016','September_2016','October_2016','November_2016','December_2016','January_2017','February_2017','March_2017','April_2017','May_2017','June_2017','July_2017','August_2017','September_2017','October_2017','November_2017','December_2017','January_2018','February_2018','March_2018','April_2018','May_2018','June_2018','July_2018','August_2018','September_2018','October_2018','November_2018','December_2018')
sales_lpn_month$month<- ordered(sales_lpn_month$month, levels = level_month)

#spreading the data to get 0s in quarters where the part was not ordered
sales_lpn_month2 <- sales_lpn_month %>% 
  select(Legacy_Part_Number, month, quantity_m) %>% 
  tidyr::spread(month, quantity_m)

sum(is.na(sales_lpn_month2))
sales_lpn_month2[is.na(sales_lpn_month2)] <- 0 #replacing all NAs with 0

#gather the columns to bring it back to original form
#Now each part has an entry in every quarter
sales_lpn_month <- gather(sales_lpn_month2, key = month, value = quantity, -Legacy_Part_Number)
sales_lpn_month$month <- ordered(sales_lpn_month$month, levels = level_month)

sales_lpn_covs_m <-  sales_lpn_month %>% 
  group_by(Legacy_Part_Number) %>% 
  summarise(covs_m = (sd(quantity, na.rm = T) / mean(quantity, na.rm = T)))



#Selecting the parts that have Cv in monthly sales <= 1.0
lpn_cvm_lt1 <- sales_lpn_covs_m %>% filter(covs_m <= 1.0)



sales_cvm_lt1 <- sales_lpn_month %>% 
                 inner_join(lpn_cvm_lt1) %>% 
                 select(Legacy_Part_Number, month, quantity) %>%
                 tidyr::spread(key = Legacy_Part_Number, value = quantity) %>%
                 filter(!(month=="December_2018")) %>%
                 arrange(month)

sales_cvm_lt1_test <-sales_lpn_month %>% 
                inner_join(lpn_cvm_lt1) %>% 
                select(Legacy_Part_Number, month, quantity) %>%
                tidyr::spread(key = Legacy_Part_Number, value = quantity) %>%
                filter(month=="December_2018") %>%
                arrange(month)


#Selecting the parts that have Cv in monthly sales between 1.0 & 4.5
lpn_cvm_lt4.5 <- sales_lpn_covs_m %>% filter(covs_m > 1.0 & covs_m <= 4.5)

sales_cvm_lt4.5 <- sales_lpn_month %>% 
                   inner_join(lpn_cvm_lt4.5) %>% 
                   select(Legacy_Part_Number, month, quantity) %>%
                   tidyr::spread(key = Legacy_Part_Number, value = quantity) %>% 
                   arrange(month)

# sales_cvm_lt4.5$month <- ordered(sales_cvm_lt4.5$month, levels = level_month)
# sales_cvm_lt4.5 <- sales_cvm_lt4.5 %>%  tidyr::spread(key = month, value = quantity)




# Calculating quarterly Co-eff of Variation in Sales ----------------------

sales_lpn_quarter <- sales_t %>% 
  group_by(Legacy_Part_Number, quarter, year) %>% 
  summarise(quantity_q = sum(Quantity_Ordered))
sales_lpn_quarter$quarter <- paste(sales_lpn_quarter$quarter, sales_lpn_quarter$year, sep = "-")


#spreading the data to get 0s in quarters where the part was not ordered
sales_lpn_quarter2 <- sales_lpn_quarter %>% 
  select(Legacy_Part_Number, quarter, quantity_q) %>% 
  tidyr::spread(quarter, quantity_q)

sum(is.na(sales_lpn_quarter2))
sales_lpn_quarter2[is.na(sales_lpn_quarter2)] <- 0

#gather the columns to bring it back to original form
#Now each part has an entry in every quarter
sales_lpn_quarter <- gather(sales_lpn_quarter2, key = quarter, value = quantity, -Legacy_Part_Number)
level_quarter <- c('Q1-2016','Q2-2016','Q3-2016','Q4-2016','Q1-2017','Q2-2017','Q3-2017','Q4-2017','Q1-2018','Q2-2018','Q3-2018','Q4-2018')
sales_lpn_quarter$quarter<- ordered(sales_lpn_quarter$quarter, levels = level_quarter)
sales_lpn_covs_q <-  sales_lpn_quarter %>% 
                     group_by(Legacy_Part_Number) %>% 
                     summarise(covs_q = (sd(quantity, na.rm = T) / mean(quantity, na.rm = T)))

#Selecting the parts that have Cv in quarterly sales <= 1.0
lpn_cvq_lt1 <- sales_lpn_covs_q %>% filter(covs_q <= 1.0)
sales_cvq_lt1 <- sales_lpn_quarter %>% 
                 inner_join(lpn_cvq_lt1) %>%
                 select(Legacy_Part_Number, quarter, quantity) %>%
  filter(!(quarter=='Q4-2018')) %>%
  #filter(Legacy_Part_Number=='10922587') %>%
                 tidyr::spread(key = Legacy_Part_Number, value = quantity) %>% 
                 arrange(quarter)
sales_cvq_lt1_test <- sales_lpn_quarter %>% 
  inner_join(lpn_cvq_lt1) %>%
  select(Legacy_Part_Number, quarter, quantity) %>%
  filter(quarter=='Q4-2018') %>%
  tidyr::spread(key = Legacy_Part_Number, value = quantity) %>% 
  arrange(quarter)


#Selecting the parts that have Cv in quarterly sales between 1.0 & 2.5
lpn_cvq_lt2.5 <- sales_lpn_covs_q %>% filter(covs_q > 1.0 & covs_q <= 2.5)
sales_cvq_lt2.5 <- sales_lpn_quarter %>% 
                   inner_join(lpn_cvq_lt2.5) %>%
                   select(Legacy_Part_Number, quarter, quantity) %>%
                   tidyr::spread(key = Legacy_Part_Number, value = quantity) %>% 
                   arrange(quarter)




# Final Models ------------------------------------------------------------


# ARIMA for parts with CV <= 1 --------------------------------------------
#Monthly forecast using ARIMA Model for parts with CV < 1
arima_input_m <- sales_cvm_lt1[-1]
arima_output_m <- cbind(lpn=c(), Forecast = c(), `Lo 80` = c(), `Hi 80` = c(), `Lo 95` = c(), `Hi 95` = c())

start.time <- Sys.time()
for(i in seq_along(arima_input_m)){
  t_ar <-forecast(auto.arima(arima_input_m[i]), 1) %>% as.data.frame()
  lpn <- colnames(arima_input_m)[i]
  forecast_row <- cbind(lpn, t_ar)
  arima_output_m <- rbind(arima_output_m, forecast_row)
}
end.time <- Sys.time()
(time.taken <- end.time - start.time)

testcheck_m<-cbind(arima_output_m$lpn,arima_output_m$`Point Forecast`,
                   t(sales_cvm_lt1_test[,-1]))


  


#Quarterly forecast using ARIMA Model  for parts with CV < 1
arima_input_q <- sales_cvq_lt1[-1]
#ts.plot(sales_cvq_lt1[-1])
arima_output_q <- cbind(lpn=c(), Forecast = c(), `Lo 80` = c(), `Hi 80` = c(), `Lo 95` = c(), `Hi 95` = c())
count_arima000=0
start.time <- Sys.time()
for(i in seq_along(arima_input_q)){
  arima_model<-auto.arima(arima_input_q[i])
  t_ar <-forecast(arima_model, 1) %>% as.data.frame()
  if (all(arima_model$arma==c(0,0,0,0,1,0,0),TRUE)==TRUE) {
    count_arima000=count_arima000+1
  }
  lpn <- colnames(arima_input_q)[i]
  forecast_row <- cbind(lpn, t_ar)
  arima_output_q <- rbind(arima_output_q, forecast_row)
}
end.time <- Sys.time()
(time.taken <- end.time - start.time)
result <- mapply(FUN=accuracy,f=arima_output_q$`Point Forecast`,x=sales_cvq_lt1_test,SIMPLIFY=FALSE)
result
sapply(result,"[","Test set","RMSE")
testcheck<-cbind(arima_output_q$lpn,arima_output_q$`Point Forecast`,t(sales_cvq_lt1_test[,-1]))


### Calculating Safety Stock with new method

colnames(croston_output_m_1)<- c("Legacy_Part_Number","forecast","forecast_int")
sales_cvm_lt1<-sales_lpn_month %>% 
  inner_join(arima_output_m) %>% 
  select(Legacy_Part_Number, month, quantity) %>%
  tidyr::spread(key = Legacy_Part_Number, value = quantity) %>%
  filter(!(month=="November_2018")) %>%
  arrange(month)



sales_cvm_lt1_test<-sales_lpn_month %>% 
  inner_join(arima_output_m) %>% 
  select(Legacy_Part_Number, month, quantity) %>%
  tidyr::spread(key = Legacy_Part_Number, value = quantity) %>%
  filter(month=="November_2018") %>%
  arrange(month)



Safetystock_old<-sales_cvm_lt1 %>%
  transpose() %>%
  select(c(24:35))
Safetystock_old$Legacy_Part_Number<-colnames(sales_cvm_lt1)
cols = c(1:12);    
Safetystock_old[,cols] = apply(Safetystock_old[,cols], 2, function(x) as.numeric(as.character(x)));
Safetystock_old<-as.data.frame(Safetystock_old)

rowmeans <- Safetystock_old %>%
  select(contains("V")) %>%
  rowMeans(na.rm = TRUE)
Safetystock_old<-cbind(Safetystock_old,rowmeans)

Safetystock_old<-Safetystock_old[-1,]
Safetystock_old<-Safetystock_old%>%
  select(c(13,14))
### Calculating Safety Stock with new method
arima_output_m<-arima_output_m %>%
  select(c(1,2))
colnames(arima_output_m)<-c("Legacy_Part_Number","forecast")
         
accuracy_check <-  arima_output_m %>%
  inner_join(Safetystock_old) %>%
  cbind(t(sales_cvm_lt1_test[,-1])) 



RMSE_Old_Method<-sqrt(sum((accuracy_check$rowmeans-
                             accuracy_check$`t(sales_cvm_lt1_test[, -1])`)^2))

RMSE_New_Method<-sqrt(sum((accuracy_check$forecast-
                             accuracy_check$`t(sales_cvm_lt1_test[, -1])`)^2))

RMSE_Old_Methodx<- mean((accuracy_check$rowmeans- accuracy_check$`t(sales_cvm_lt1_test[, -1])`)^2)

RMSE_New_Methodx<-mean((accuracy_check$forecast-
                          accuracy_check$`t(sales_cvm_lt1_test[, -1])`)^2)

                             










itemrecords_f <- itemrecords %>%
  select(Legacy_Part_Number,Leadtime_Level)

safety_stock<- lpn_cvm_lt1 %>%
  left_join(arima_output_m,by=c("Legacy_Part_Number"="lpn")) %>%
  left_join(itemrecords_f,by="Legacy_Part_Number")
               
 
safety_stock$Leadtime_Level <- ifelse(is.na(safety_stock$Leadtime_Level),14,
                                   safety_stock$Leadtime_Level)

safety_stock$Leadtime_months<-  safety_stock$Leadtime_Level/30
safety_stock$service_level<- 0.95

safety_stock_calculations<-safety_stock%>%
  mutate(Lead_Time_Demand=`Point Forecast`*Leadtime_months) %>%
  mutate(sd=(`Hi 95`-`Point Forecast`)/2) %>%
  mutate(service_factor=qnorm(service_level)) %>%
  mutate(Lead_Time_Factor=sqrt(Leadtime_months)) %>%
  mutate(safety_stock_arima=round(sd*service_factor*Lead_Time_Factor) )


### Calculating Safety Stock with old method for monthly arima

Safetystock_old<-sales_cvm_lt1 %>%
  transpose() %>%
  select(c(24:35))
Safetystock_old$Legacy_Part_Number<-colnames(sales_cvm_lt1)

Safetystock_old<- lpn_cvm_lt1 %>%
  full_join(Safetystock_old,by="Legacy_Part_Number") %>%
  filter(!(row_number()==n())) 

Safetystock_old$month_count<-rowSums(!(Safetystock_old == 0))-1

Safetystock_old<- Safetystock_old %>%
  mutate(plan_code=ifelse(month_count>5,"A",ifelse(month_count>3,"B",
                          ifelse(month_count>1,"C","D")))) 
Safetystock_old[,3:14]<- apply(Safetystock_old[,3:14]
                               ,2,as.numeric)
Safetystock_old$sd<- apply(Safetystock_old[,3:11],1,sd)

sd(Safetystock_old[2,3:11])

Safetystock_old$avg_month_use<- (apply(Safetystock_old[,3:14]
                               ,1,sum)/12)
Safetystock_old <-Safetystock_old %>%
  left_join(itemrecords_f,by="Legacy_Part_Number") 


Safetystock_old$Leadtime_Level <- ifelse(is.na(Safetystock_old$Leadtime_Level),14,
                                         Safetystock_old$Leadtime_Level)

Safetystock_old <- Safetystock_old %>%
  mutate(LT_in_Months=30/Leadtime_Level) %>%
  mutate(Usage_per_LT=avg_month_use/LT_in_Months) %>%
  mutate(multiplier=ifelse(plan_code=="A",2.054,ifelse(plan_code=="B"
                      ,1.282,ifelse(plan_code=="C",0.675,0)))) %>%
  mutate(safety_factor=sd*multiplier) %>%
  mutate(safety_stock_old=round(Usage_per_LT+safety_factor))




safety_stock_arima <- safety_stock_calculations %>%
  select(Legacy_Part_Number,safety_stock_arima)

accuracy_check <-  Safetystock_old %>%
  select(Legacy_Part_Number,safety_stock_old) %>%
  inner_join(safety_stock_arima,by="Legacy_Part_Number") %>%
  cbind( t(sales_cvm_lt1_test[,-1]))


RMSE_Old_Method<-sqrt(sum((accuracy_check$safety_stock_old-
       accuracy_check$`t(sales_cvm_lt1_test[, -1])`)^2))

RMSE_New_Method<-sqrt(sum((accuracy_check$safety_stock_arima-
            accuracy_check$`t(sales_cvm_lt1_test[, -1])`)^2))



### Calculating Safety Stock with old method for quarterly arima


sales_cvm_lt1<-sales_lpn_month %>% 
  inner_join(lpn_cvq_lt1) %>% 
  select(Legacy_Part_Number, month, quantity) %>%
  tidyr::spread(key = Legacy_Part_Number, value = quantity) %>%
  arrange(month)



Safetystock_old<-sales_cvm_lt1 %>%
  transpose() %>%
  select(c(24:35))

Safetystock_old$Legacy_Part_Number<-colnames(sales_cvm_lt1)
cols = c(1:12);    
Safetystock_old[,cols] = apply(Safetystock_old[,cols], 2, function(x) as.numeric(as.character(x)));
Safetystock_old<-as.data.frame(Safetystock_old)

rowmeans <- Safetystock_old %>%
  select(contains("V")) %>%
  rowMeans(na.rm = TRUE) 
  
Safetystock_old<-cbind(Safetystock_old,rowmeans)
Safetystock_old$rowmeans=3*Safetystock_old$rowmeans
Safetystock_old<-Safetystock_old[-1,]
Safetystock_old<-Safetystock_old%>%
  select(c(13,14)) 
### Calculating Safety Stock with new method


colnames(arima_output_q)<-c("Legacy_Part_Number","forecast","V1","V2","V3","V4")
arima_output_q<-arima_output_q %>%
  select(c(1:2))
arima_output_q %>%
  filter(lpn=="3994425")
arima_output_q<- arima_output_q %>%
  distinct(Legacy_Part_Number,.keep_all = TRUE)
Safetystock_old<- Safetystock_old %>%
  distinct(Legacy_Part_Number,.keep_all = TRUE)

accuracy_check <-  arima_output_q %>%
  inner_join(Safetystock_old) %>%
  cbind(t(sales_cvq_lt1_test[,-1])) 



RMSE_Old_Method<-sqrt(sum((accuracy_check$rowmeans-
                             accuracy_check$`t(sales_cvq_lt1_test[, -1])`)^2))

RMSE_New_Method<-sqrt(sum((accuracy_check$forecast-
                             accuracy_check$`t(sales_cvq_lt1_test[, -1])`)^2))

RMSE_Old_Methodx<- mean((accuracy_check$rowmeans- accuracy_check$`t(sales_cvq_lt1_test[, -1])`)^2)

RMSE_New_Methodx<-mean((accuracy_check$forecast-
                          accuracy_check$`t(sales_cvq_lt1_test[, -1])`)^2)

test_variable<-accuracy_check$rowmeans-
  accuracy_check$`t(sales_cvq_lt1_test[, -1])`

head(accuracy_check,n=23)




## Old method
sales_cvm_lt1<-sales_lpn_month %>% 
  inner_join(lpn_cvq_lt1) %>% 
  select(Legacy_Part_Number, month, quantity) %>%
  tidyr::spread(key = Legacy_Part_Number, value = quantity) %>%
  arrange(month)



Safetystock_old<-sales_cvm_lt1 %>%
  transpose() %>%
  select(c(24:35))
Safetystock_old$Legacy_Part_Number<-colnames(sales_cvq_lt1)

Safetystock_old<- lpn_cvq_lt1 %>%
  full_join(Safetystock_old,by="Legacy_Part_Number") %>%
  filter(!(row_number()==n())) 

Safetystock_old$month_count<-rowSums(!(Safetystock_old == 0))-1

Safetystock_old<- Safetystock_old %>%
  mutate(plan_code=ifelse(month_count>5,"A",ifelse(month_count>3,"B",
                                                   ifelse(month_count>1,"C","D")))) 
Safetystock_old[,3:14]<- apply(Safetystock_old[,3:14]
                               ,2,as.numeric)
Safetystock_old$sd<- apply(Safetystock_old[,3:11],1,sd)

Safetystock_old$avg_month_use<- (apply(Safetystock_old[,3:14]
                                       ,1,sum)/12)
Safetystock_old <-Safetystock_old %>%
  left_join(itemrecords_f,by="Legacy_Part_Number") 


Safetystock_old$Leadtime_Level <- ifelse(is.na(Safetystock_old$Leadtime_Level),14,
                                         Safetystock_old$Leadtime_Level)

Safetystock_old <- Safetystock_old %>%
  mutate(LT_in_Months=30/Leadtime_Level) %>%
  mutate(Usage_per_LT=avg_month_use/LT_in_Months) %>%
  mutate(multiplier=ifelse(plan_code=="A",2.054,ifelse(plan_code=="B"
                                                       ,1.282,ifelse(plan_code=="C",0.675,0)))) %>%
  mutate(safety_factor=sd*multiplier) %>%
  mutate(safety_stock_old=3*round(Usage_per_LT+safety_factor))


### Calculating Safety Stock with new method

safety_stock<- lpn_cvq_lt1 %>%
  left_join(arima_output_q,by=c("Legacy_Part_Number"="lpn")) %>%
  left_join(itemrecords_f,by="Legacy_Part_Number")


safety_stock$Leadtime_Level <- ifelse(is.na(safety_stock$Leadtime_Level),14,
                                      safety_stock$Leadtime_Level)

safety_stock$Leadtime_months<-  safety_stock$Leadtime_Level/30
safety_stock$service_level<- 0.95

safety_stock_calculations<-safety_stock%>%
  mutate(Lead_Time_Demand=`Point Forecast`*Leadtime_months) %>%
  mutate(sd=(`Hi 95`-`Point Forecast`)/2) %>%
  mutate(service_factor=qnorm(service_level)) %>%
  mutate(Lead_Time_Factor=sqrt(Leadtime_months)) %>%
  mutate(safety_stock_arima=round(sd*service_factor*Lead_Time_Factor) )

safety_stock_arima <- safety_stock_calculations %>%
  select(Legacy_Part_Number,safety_stock_arima)

## Accuracy check for both methods


safety_stock_arima<- safety_stock_arima %>%
  distinct(Legacy_Part_Number,.keep_all = TRUE)
Safetystock_old<- Safetystock_old %>%
  distinct(Legacy_Part_Number,.keep_all = TRUE)
accuracy_check <-  Safetystock_old %>%
  select(Legacy_Part_Number,safety_stock_old) %>%
  left_join(safety_stock_arima,by="Legacy_Part_Number") %>%
  cbind(t(sales_cvq_lt1_test[,-1]))



RMSE_Old_Method<-sqrt(sum((accuracy_check$safety_stock_old-
                             accuracy_check$`t(sales_cvq_lt1_test[, -1])`)^2))

RMSE_New_Method<-sqrt(sum((accuracy_check$safety_stock_arima-
                             accuracy_check$`t(sales_cvq_lt1_test[, -1])`)^2))


# Croston's Method for parts with CV > 1 ----------------------------------

#Monthly forecast Using Croston's Method  for parts with CV > 1 & <= 2.5

croston_input_m <- as.data.frame(sales_cvm_lt4.5[-1])

lpn <- c()
forecast <- c()
forecast_int <- c()
croston_output_m <- cbind(lpn, forecast, forecast_int)

# i = 10
start.time <- Sys.time()
for(i in seq_along(croston_input_m)){
  lpn <- append(lpn, colnames(croston_input_m)[i])
  croston_lpn <- crost(data=croston_input_m[,i],h=1,type="sbj",cost="mar")
  forecast <- append(forecast, croston_lpn$components$c.out[1,1])
  forecast_int <- append(forecast_int, croston_lpn$components$c.out[1,2])
}

croston_output_m <- data.frame(lpn, forecast, forecast_int)
end.time <- Sys.time()
(time.taken <- end.time - start.time)

croston_output_m %>% 
  arrange(desc(forecast)) %>% head(n = 30)

# For parts with demand interval between 1 and 1.2 and demand>1
croston_output_m_1<-croston_output_m %>%
  filter(forecast_int<1.2) %>%
  filter(forecast>1)


#######Checking model accuracy
### Calculating Safety Stock with old method for quarterly arima

## Old method
colnames(croston_output_m_1)<- c("Legacy_Part_Number","forecast","forecast_int")
sales_cvm_lt1<-sales_lpn_month %>% 
  inner_join(croston_output_m_1) %>% 
  select(Legacy_Part_Number, month, quantity) %>%
  tidyr::spread(key = Legacy_Part_Number, value = quantity) %>%
  filter(!(month=="December_2018")) %>%
  arrange(month)

sales_cvm_lt1_test<-sales_lpn_month %>% 
  inner_join(croston_output_m_1) %>% 
  select(Legacy_Part_Number, month, quantity) %>%
  tidyr::spread(key = Legacy_Part_Number, value = quantity) %>%
  filter(month=="December_2018") %>%
  arrange(month)



Safetystock_old<-sales_cvm_lt1 %>%
  transpose() %>%
  select(c(24:35))
Safetystock_old$Legacy_Part_Number<-colnames(sales_cvm_lt1)
cols = c(1:12);    
Safetystock_old[,cols] = apply(Safetystock_old[,cols], 2, function(x) as.numeric(as.character(x)));
Safetystock_old<-as.data.frame(Safetystock_old)

rowmeans <- Safetystock_old %>%
  select(contains("V")) %>%
  rowMeans(na.rm = TRUE)
Safetystock_old<-cbind(Safetystock_old,rowmeans)

Safetystock_old<-Safetystock_old[-1,]
Safetystock_old<-Safetystock_old%>%
select(c(13,14))
### Calculating Safety Stock with new method
 testdata<-croston_output_m_1 %>%
   filter(Legacy_Part_Number=="3963929132")



accuracy_check <-  croston_output_m_1 %>%
  mutate(newforecast=forecast/forecast_int) %>%
  select(Legacy_Part_Number,newforecast) %>%
  inner_join(Safetystock_old) %>%
  cbind(t(sales_cvm_lt1_test[,-1])) 



RMSE_Old_Method<-sqrt(sum((accuracy_check$rowmeans-
                             accuracy_check$`t(sales_cvm_lt1_test[, -1])`)^2))

RMSE_New_Method<-sqrt(sum((accuracy_check$newforecast-
                             accuracy_check$`t(sales_cvm_lt1_test[, -1])`)^2))




###Safety Stock calculations

safety_stock<- croston_output_m_1 %>%
  left_join(itemrecords_f,by="Legacy_Part_Number")

safety_stock$Leadtime_Level <- ifelse(is.na(safety_stock$Leadtime_Level),14,
                                      safety_stock$Leadtime_Level)

safety_stock$Leadtime_months<-  safety_stock$Leadtime_Level/30
safety_stock$service_level<- 0.95
# standard deviation for monthly sales
mean_monthly <- sales_lpn_month %>% 
  group_by(Legacy_Part_Number) %>% 
  summarise(std_dev = mean(quantity))

colnames(mean_monthly)<- c("Legacy_Part_Number","sd")
safety_stock<- safety_stock %>%
  left_join(mean_monthly)

# std_dev_monthly%>%
#   filter(Legacy_Part_Number=="3963929132")

safety_stock_calculations<-safety_stock%>%
  mutate(Lead_Time_Demand=`forecast`*Leadtime_months) %>%
  mutate(service_factor=qnorm(service_level)) %>%
  mutate(Lead_Time_Factor=sqrt(Leadtime_months)) %>%
  mutate(safety_stock_croston=round(sd*service_factor*Lead_Time_Factor) )

safety_stock_croston <- safety_stock_calculations %>%
  select(Legacy_Part_Number,safety_stock_croston)

## Accuracy check for both methods





safety_stock_croston<- safety_stock_croston %>%
  distinct(Legacy_Part_Number,.keep_all = TRUE)
Safetystock_old<- Safetystock_old %>%
  distinct(Legacy_Part_Number,.keep_all = TRUE)
accuracy_check <-  Safetystock_old %>%
  select(Legacy_Part_Number,safety_stock_old) %>%
  left_join(safety_stock_croston,by="Legacy_Part_Number") %>%
  cbind(t(sales_cvm_lt1_test[,-1]))



RMSE_Old_Method<-sqrt(sum((accuracy_check$safety_stock_old-
                             accuracy_check$`t(sales_cvm_lt1_test[, -1])`)^2))

RMSE_New_Method<-sqrt(sum((accuracy_check$safety_stock_croston-
                             accuracy_check$`t(sales_cvm_lt1_test[, -1])`)^2))



# For parts with demand interval between 2 and 2.2 and demand>2
croston_output_m_2<-croston_output_m %>%
  filter(forecast_int>2 & forecast_int<2.2) %>%
  filter(forecast>2)






# filter out pars with forecasting interval > 3 or 6 months
# filter out parts that have less than 1 demand in 3 months


#Quarterly forecast Using Croston's Method  for parts with CV > 1 & <= 2.5
croston_input_q <- as.data.frame(sales_cvq_lt2.5[-1])
lpn <- c()
forecast <- c()
forecast_int <- c()
croston_output_q <- cbind(lpn, forecast, forecast_int)

start.time <- Sys.time()
for(i in seq_along(croston_input_q)){
  lpn <- append(lpn, colnames(croston_input_q)[i])
  croston_lpn <- crost(data=croston_input_q[,i],h=1,type="croston",cost="mar")
  forecast <- append(forecast, croston_lpn$components$c.out[1,1])
  forecast_int <- append(forecast_int, croston_lpn$components$c.out[1,2])
}

croston_output_q <- data.frame(lpn, forecast, forecast_int)
end.time <- Sys.time()
(time.taken <- end.time - start.time)

croston_output_q %>%
  arrange(desc(forecast)) %>% head(n = 30)

tend.time <- Sys.time()
(ttime.taken <- tend.time - tstart.time)






