#libaray commands
library(tidyverse)
library(dplyr)
library(knitr)
library(MASS)
library(forcats)
library(ggplot2)
library(mice)
library("car") 
library(outliers)
library(caret)
library(AppliedPredictiveModeling)
library(Metrics)
library(pls)
library(earth)

#Load data
sup_raw = read.csv('US Superstore data.csv')
sup<-sup_raw
view(sup_raw)

#Data Preparation

#set date as date
sup$Order.Date <- as.Date(sup$Order.Date, "%m/%d/%Y")
#get number of days since 1/1/1970
sup$Order.Date <- as.numeric(sup$Order.Date)
largest_date_order<-max(sup$Order.Date)
smallest_date_order<-min(sup$Order.Date)

#set date as date
sup$Ship.Date <- as.Date(sup$Ship.Date, "%m/%d/%Y")
#get number of days since 1/1/1970
sup$Ship.Date <- as.numeric(sup$Ship.Date)
largest_date_ship<-max(sup$Ship.Date)
smallest_date_ship<-min(sup$Ship.Date)

#make as numeric or as factor and lump all factors except Product.ID
sup<-sup %>%
mutate(Order.ID = fct_lump(as.factor(Order.ID), n =500))%>% 
mutate(Ship.Mode = fct_lump(as.factor(Ship.Mode), n =4))%>%
mutate(Customer.ID = fct_lump(as.factor(Customer.ID), n =500))%>% 
mutate(Customer.Name = fct_lump(as.factor(Customer.Name), n =500))%>%
mutate(Segment = fct_lump(as.factor(Segment), n =3))%>%
mutate(Country = fct_lump(as.factor(Country), n =20))%>%
mutate(City = fct_lump(as.factor(City), n =531))%>%
mutate(State = fct_lump(as.factor(State), n =49))%>%
mutate(Postal.Code = fct_lump(as.factor(Postal.Code), n =631))%>%
mutate(Region = fct_lump(as.factor(Region), n =4))%>%
mutate(Category = fct_lump(as.factor(Category), n =3))%>%
mutate(Sub.Category = fct_lump(as.factor(Sub.Category), n =17))%>%
mutate(Sales=as.numeric(Sales)) %>%
mutate(Quantity=as.numeric(Quantity)) %>%
mutate(Discount=as.numeric(Discount)) %>%
mutate(Profit=as.numeric(Profit))
  
#drop country which only has 1 unique value
sup=subset(sup,select=-(Country))
sup=subset(sup,select=-(Customer.Name))
sup=subset(sup,select=-(Product.Name))

#aggregate train data based off product
agg_sup<-sup %>% group_by(Product.ID) %>% 
summarize(order_date_mean=mean(Order.Date, na.rm=TRUE),
order_date_from_start=min(Order.Date, na.rm=TRUE)-smallest_date_order, 
order_date_from_end=largest_date_order-max(Order.Date, na.rm=TRUE), 
order_date_range=max(Order.Date)-min(Order.Date), 
ship_date_mean=mean(Ship.Date, na.rm=TRUE), 
average_shipping_delay=mean(Ship.Date, na.rm=TRUE)-mean(Order.Date, na.rm=TRUE),
shipping_mode=as.factor(first(ifelse(is.na(Ship.Mode), 'NA', Ship.Mode))), 
segment=as.factor(first(ifelse(is.na(Segment),NA, Segment))),
segment_count=length(unique(factor(Segment))),
city=as.factor(first(ifelse(is.na(City), 'NA', City))), 
city_count=length(unique(factor(City))),
state=as.factor(first(ifelse(is.na(State), 'NA', State))),
state_count=length(unique(factor(State))),
postal_code=as.factor(first(ifelse(is.na(Postal.Code), 'NA', Postal.Code))), 
postal_code_count=length(unique(factor(Postal.Code))),
region=as.factor(first(ifelse(is.na(Region), 'NA', Region))),  
region_count=length(unique(factor(Region))),
customer_ID=as.factor(first(ifelse(is.na(Customer.ID), 'NA', Customer.ID))),
customer_count=length(unique(factor(Customer.ID))),
category=as.factor(first(ifelse(is.na(Category), 'NA', Category))),
sub_category=as.factor(first(ifelse(is.na(Sub.Category), 'NA', Sub.Category))),
mean_of_sales=mean(Sales, na.rm=TRUE),  
sales_count=sum(ifelse(Sales>0,1,0)), 
sales_max=max(Sales, na.rm=TRUE), 
sales_median=median(Sales, na.rm=TRUE), 
quantity_mean=mean(Quantity, na.rm=TRUE),
quantity_max=max(Quantity, na.rm=TRUE),
quantity_median=median(Quantity, na.rm=TRUE),
discount_max=max(Discount, na.rm=TRUE),
discount_mean=mean(Discount, na.rm=TRUE),
discount_count=sum(ifelse(Discount>0,1,0)), 
discount_median=median(Discount, na.rm=TRUE),
profit=sum(Profit, na.rm=TRUE),
profit_per_product=profit/quantity_max)


##Decision tree regression
dt.fit1<-train(profit_per_product ~.,agg_sup, method ='rpart2')  #Rsq=0.412, #RMSE=123.382
dtprofit<-train(profit~.,agg_sup, method ='rpart2') #rsq=0.496, #RMSE=616.062
dt.fitnumeric<-train(Profit ~ Sales+Quantity+Discount,sup,method='rpart2', preProcess = c("center", "scale"))#RMSE=165.98 #rsq=0.528
dt.fit<- train(profit_per_product ~mean_of_sales+quantity_mean+ discount_mean+ category, 
               agg_sup, method='rpart2') #Rsq=0.142, #RMSE=163.19
dt.fitsup<-train(Profit ~Sales+Ship.Mode,sup,method='rpart2', preProcess = c("center", "scale"))
dt#RMSE 205.4364 # Rsq=0.251
#tried few combinations
dt1<-train(profit ~ shipping_mode + mean_of_sales,agg_sup,method='rpart2', preProcess = c("center", "scale"))
#RMSE 846.813  #Rsq= 0.0594
dt2<-train(profit ~ shipping_mode + mean_of_sales+discount_mean+category,agg_sup,method='rpart2', preProcess = c("center", "scale"))
#RMSE 801.8339  #Rsq=0.157
dt3<-train(profit ~ shipping_mode + mean_of_sales+discount_max+category,agg_sup,method='rpart2', preProcess = c("center", "scale"))
#RMSE 784.38  #Rsq=0.061

#Mars model on data aggregated by product ID
grid <- expand.grid(degree = 1:2, nprune=c(10,20,30))
mars.fit<- train(profit ~customer_count+city+state_count+customer_ID+category+sub_category+mean_of_sales+sales_max+sales_median, data=agg_sup, method = "earth",metric = "RMSE",  trControl = trainControl(method = "cv", number = 5), tuneGrid = grid)
mars.fit$bestTune #10, 2
summary(mars.fit) #0.80288
predictions<-predict(mars.fit,agg_sup)
actuals<-agg_sup$profit
RMSE<-rmse(actuals,predictions)
RMSE #359.1415

#SVM model on data aggregated by product ID
grid <- expand.grid(sigma = seq(0.0000000001,0.0001,length=5),
                    C = c(100, 200, 300, 400))
svm.fit <- train(profit ~customer_count+city+state_count+customer_ID+category+sub_category+mean_of_sales+sales_max+sales_median, data=agg_sup,tuneGrid = grid,method="svmRadial")
svm.fit
plot(svm.fit)
summary(svm.fit) #
predictions<-predict(svm.fit,agg_sup)
actuals<-agg_sup$profit
RMSE<-rmse(actuals,predictions)
RMSE #642.8587


#evaluating each variable against
fit_lm<-lm(profit ~ order_date_mean, data=agg_sup)
summary(fit_lm) #Rsq=-0.0004
fit_lm<-lm(profit ~ order_date_from_start, data=agg_sup)
summary(fit_lm) #Rsq=-0.0009
fit_lm<-lm(profit ~ order_date_from_end, data=agg_sup)
summary(fit_lm) #Rsq=-0.0003
fit_lm<-lm(profit ~ ship_date_mean, data=agg_sup)
summary(fit_lm) #Rsq=-0.00004
fit_lm<-lm(profit ~ average_shipping_delay, data=agg_sup)
summary(fit_lm) #Rsq=-0.0005
fit_lm<-lm(profit ~ shipping_mode, data=agg_sup)
summary(fit_lm) #Rsq=0.001
fit_lm<-lm(profit ~ customer_count, data=agg_sup)
summary(fit_lm) #Rsq=0.09
fit_lm<-lm(profit ~ segment, data=agg_sup)
summary(fit_lm) #Rsq=-0.001
fit_lm<-lm(profit ~ segment_count, data=agg_sup)
summary(fit_lm) #Rsq=0.004
fit_lm<-lm(profit ~ city, data=agg_sup)
summary(fit_lm) #Rsq=-0.118
fit_lm<-lm(profit ~ city_count, data=agg_sup)
summary(fit_lm) #Rsq=0.06
fit_lm<-lm(profit ~ state, data=agg_sup)
summary(fit_lm) #Rsq=0.006
fit_lm<-lm(profit ~ state_count, data=agg_sup)
summary(fit_lm) #Rsq=0.0088
fit_lm<-lm(profit ~ postal_code, data=agg_sup)
summary(fit_lm) #Rsq=-0.1222
fit_lm<-lm(profit ~ postal_code_count, data=agg_sup)
summary(fit_lm) #Rsq=0.007
fit_lm<-lm(profit ~ region, data=agg_sup)
summary(fit_lm) #Rsq=-0.0007
fit_lm<-lm(profit ~ region_count, data=agg_sup)
summary(fit_lm) #Rsq=-0.0028
fit_lm<-lm(profit ~ customer_ID, data=agg_sup)
summary(fit_lm) #Rsq=0.1536
fit_lm<-lm(profit ~ category, data=agg_sup)
summary(fit_lm) #Rsq=-0.01786
fit_lm<-lm(profit ~ sub_category, data=agg_sup)
summary(fit_lm) #Rsq=0.1971
fit_lm<-lm(profit ~ mean_of_sales, data=agg_sup)
summary(fit_lm) #Rsq=-0.1184
fit_lm<-lm(profit ~ sales_count, data=agg_sup)
summary(fit_lm) #Rsq=0.0077
fit_lm<-lm(profit ~ sales_max, data=agg_sup)
summary(fit_lm) #Rsq=0.177
fit_lm<-lm(profit ~ sales_median, data=agg_sup)
summary(fit_lm) #Rsq=-0.09423
fit_lm<-lm(profit ~ quantity_mean, data=agg_sup)
summary(fit_lm) #Rsq=-0.0008
fit_lm<-lm(profit ~quantity_max, data=agg_sup)
summary(fit_lm) #Rsq=0.005
fit_lm<-lm(profit ~ quantity_median, data=agg_sup)
summary(fit_lm) #Rsq=-0.00008
fit_lm<-lm(profit ~ discount_mean, data=agg_sup)
summary(fit_lm) #Rsq=-0.018
fit_lm<-lm(profit ~ discount_count, data=agg_sup)
summary(fit_lm) #Rsq=-0.0005
fit_lm<-lm(profit ~ discount_median, data=agg_sup)
summary(fit_lm) #Rsq=-0.02


#evaluating each variable against
fit_lm<-lm(profit_per_product ~ order_date_mean, data=agg_sup)
summary(fit_lm) #Rsq=-0.0004
fit_lm<-lm(profit_per_product ~ order_date_from_start, data=agg_sup)
summary(fit_lm) #Rsq=-0.0005
fit_lm<-lm(profit_per_product ~ order_date_from_end, data=agg_sup)
summary(fit_lm) #Rsq=-0.0005
fit_lm<-lm(profit_per_product ~ ship_date_mean, data=agg_sup)
summary(fit_lm) #Rsq=-0.00004
fit_lm<-lm(profit_per_product ~ average_shipping_delay, data=agg_sup)
summary(fit_lm) #Rsq=-0.0005
fit_lm<-lm(profit_per_product ~ shipping_mode, data=agg_sup)
summary(fit_lm) #Rsq=0.001
fit_lm<-lm(profit_per_product ~ customer_count, data=agg_sup)
summary(fit_lm) #Rsq=0.003
fit_lm<-lm(profit_per_product ~ segment, data=agg_sup)
summary(fit_lm) #Rsq=-0.002
fit_lm<-lm(profit_per_product ~ segment_count, data=agg_sup)
summary(fit_lm) #Rsq=0.002
fit_lm<-lm(profit_per_product ~ city, data=agg_sup)
summary(fit_lm) #Rsq=-0.1298
fit_lm<-lm(profit_per_product ~ city_count, data=agg_sup)
summary(fit_lm) #Rsq=0.002
fit_lm<-lm(profit_per_product ~ state, data=agg_sup)
summary(fit_lm) #Rsq=0.01
fit_lm<-lm(profit_per_product ~ state_count, data=agg_sup)
summary(fit_lm) #Rsq=0.004
fit_lm<-lm(profit_per_product ~ postal_code, data=agg_sup)
summary(fit_lm) #Rsq=-0.1374
fit_lm<-lm(profit_per_product ~ postal_code_count, data=agg_sup)
summary(fit_lm) #Rsq=0.002
fit_lm<-lm(profit_per_product ~ region, data=agg_sup)
summary(fit_lm) #Rsq=-0.0012
fit_lm<-lm(profit_per_product ~ region_count, data=agg_sup)
summary(fit_lm) #Rsq=-0.0006
fit_lm<-lm(profit_per_product ~ customer_ID, data=agg_sup)
summary(fit_lm) #Rsq=0.1869
fit_lm<-lm(profit_per_product ~ category, data=agg_sup)
summary(fit_lm) #Rsq=-0.015
fit_lm<-lm(profit_per_product ~ sub_category, data=agg_sup)
summary(fit_lm) #Rsq=0.1878
fit_lm<-lm(profit_per_product ~ mean_of_sales, data=agg_sup)
summary(fit_lm) #Rsq=-0.122
fit_lm<-lm(profit_per_product ~ sales_count, data=agg_sup)
summary(fit_lm) #Rsq=0.0027
fit_lm<-lm(profit_per_product ~ sales_max, data=agg_sup)
summary(fit_lm) #Rsq=0.169
fit_lm<-lm(profit_per_product ~ sales_median, data=agg_sup)
summary(fit_lm) #Rsq=-0.1025
fit_lm<-lm(profit_per_product ~ quantity_mean, data=agg_sup)
summary(fit_lm) #Rsq=-0.0005
fit_lm<-lm(profit_per_product ~quantity_max, data=agg_sup)
summary(fit_lm) #Rsq=0.0004
fit_lm<-lm(profit_per_product ~ quantity_median, data=agg_sup)
summary(fit_lm) #Rsq=-0.0004
fit_lm<-lm(profit_per_product ~ discount_mean, data=agg_sup)
summary(fit_lm) #Rsq=-0.017
fit_lm<-lm(profit_per_product ~ discount_count, data=agg_sup)
summary(fit_lm) #Rsq=-0.0002
fit_lm<-lm(profit ~ discount_median, data=agg_sup)
summary(fit_lm) #Rsq=-0.022
fit_lm<-lm(profit_per_product ~ discount_median, data=agg_sup)
summary(fit_lm) #Rsq=-0.02




#modeling on  set aggregated by product.ID (all variables  causes 0 residuals so I took all rsq>0.1)
fit_lm<-lm(profit ~customer_count+city+state_count+customer_ID+category+sub_category+mean_of_sales+sales_max+sales_median, data=agg_sup)
summary(fit_lm) #Rsq=0.3323

predictions_lm<-predict(fit_lm,agg_sup)
actuals<-agg_sup$profit
RMSE<-rmse(actuals,predictions_lm)
RMSE #505
plot(fit_lm)





#modeling per transaction linear
fit_lm<-lm(Profit ~., data=sup)
summary(fit_lm) #Rsq=0.6522
predictions_lm<-predict(fit_lm,sup)
actuals<-sup$Profit
RMSE<-rmse(actuals,predictions_lm)
RMSE # 121.6146
plot(fit_lm)

fit_lm_stepped<-stepAIC(fit_lm, direction="both")
fit_lm_stepped$anova #final model same as original
summary(fit_lm_stepped)
#Final model
#Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region
fit_lm<-lm(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup)
summary(fit_lm) #Rsq=0.6633
predictions_lm<-predict(fit_lm,sup)
actuals<-sup$Profit
RMSE<-rmse(actuals,predictions_lm)
RMSE # 122.553


ggplot(sup)

cor(as.numeric(sup$Sub.Category), as.numeric(sup$Profit), method="spearman")

sup_acc<-data.frame(sup[sup$Sub.Category=='Accessories',])
sup_app<-data.frame(sup[sup$Sub.Category=='Appliances',])
sup_art<-data.frame(sup[sup$Sub.Category=='Art',])
sup_bind<-data.frame(sup[sup$Sub.Category=='Binders',])
sup_book<-data.frame(sup[sup$Sub.Category=='Bookcases',])
sup_cha<-data.frame(sup[sup$Sub.Category=='Chairs',])
sup_cop<-data.frame(sup[sup$Sub.Category=='Copiers',])
sup_env<-data.frame(sup[sup$Sub.Category=='Envelopes',])
sup_fas<-data.frame(sup[sup$Sub.Category=='Fasteners',])
sup_fur<-data.frame(sup[sup$Sub.Category=='Furnishings',])
sup_lab<-data.frame(sup[sup$Sub.Category=='Labels',])
sup_mac<-data.frame(sup[sup$Sub.Category=='Machines',])
sup_pap<-data.frame(sup[sup$Sub.Category=='Paper',])
sup_pho<-data.frame(sup[sup$Sub.Category=='Phones',])
sup_sto<-data.frame(sup[sup$Sub.Category=='Storage',])
sup_sup<-data.frame(sup[sup$Sub.Category=='Supplies',])
sup_tab<-data.frame(sup[sup$Sub.Category=='Tables',])






#modeling per transaction for each category
#category Accessories
fit_lm_acc<-lm(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_acc)
summary(fit_lm_acc) #Rsq=0.8953
predictions_lm_acc<-predict(fit_lm_acc,sup_acc)
actuals<-sup_acc$Profit
RMSE<-rmse(actuals,predictions_lm_acc)
RMSE # 30.77537
sup_acc$Predict_Profit<-predictions_lm_acc


#category Appliances
fit_lm_app<-lm(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_app)
summary(fit_lm_app) #Rsq=0.7699
predictions_lm_app<-predict(fit_lm_app,sup_app)
actuals<-sup_app$Profit
RMSE<-rmse(actuals,predictions_lm_app)
RMSE # 62.45189
sup_app$Predict_Profit<-predictions_lm_app

#category Art
fit_lm_art<-lm(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_art)
summary(fit_lm_art) #Rsq=0.9381
predictions_lm_art<-predict(fit_lm_art,sup_art)
actuals<-sup_art$Profit
RMSE<-rmse(actuals,predictions_lm_art)
RMSE # 2.949
sup_art$Predict_Profit<-predictions_lm_art

#category Binders
fit_lm_bind<-lm(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_bind)
summary(fit_lm_bind) #Rsq=0.7134
predictions_lm_bind<-predict(fit_lm_bind,sup_bind)
actuals<-sup_bind$Profit
RMSE<-rmse(actuals,predictions_lm_bind)
RMSE # 151.67
sup_bind$Predict_Profit<-predictions_lm_bind

#category Bookcases
fit_lm_book<-lm(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_book)
summary(fit_lm_book) #Rsq=0.314
predictions_lm_book<-predict(fit_lm_book,sup_book)
actuals<-sup_book$Profit
RMSE<-rmse(actuals,predictions_lm_book)
RMSE # 129.67
sup_book$Predict_Profit<-predictions_lm_book

#category Chairs
fit_lm_cha<-lm(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_cha)
summary(fit_lm_cha) #Rsq=0.7156
predictions_lm_cha<-predict(fit_lm_cha,sup_cha)
actuals<-sup_cha$Profit
RMSE<-rmse(actuals,predictions_lm_cha)
RMSE # 62.286
sup_cha$Predict_Profit<-predictions_lm_cha

#category Copiers
fit_lm_cop<-lm(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_cop)
summary(fit_lm_cop) #Rsq=0.9763
predictions_lm_cop<-predict(fit_lm_cop,sup_cop)
actuals<-sup_cop$Profit
RMSE<-rmse(actuals,predictions_lm_cop)
RMSE # 185.112
sup_cop$Predict_Profit<-predictions_lm_cop

#category Envelopes
fit_lm_env<-lm(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_env)
summary(fit_lm_env) #Rsq=0.9776
predictions_lm_env<-predict(fit_lm_env,sup_env)
actuals<-sup_env$Profit
RMSE<-rmse(actuals,predictions_lm_env)
RMSE # 4.585
sup_env$Predict_Profit<-predictions_lm_env

#category Fasteners
fit_lm_fas<-lm(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_fas)
summary(fit_lm_fas) #Rsq=0.8734
predictions_lm_fas<-predict(fit_lm_fas,sup_fas)
actuals<-sup_fas$Profit
RMSE<-rmse(actuals,predictions_lm_fas)
RMSE # 1.56859
sup_fas$Predict_Profit<-predictions_lm_fas

#category Furnishings
fit_lm_fur<-lm(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_fur)
summary(fit_lm_fur) #Rsq=0.6879
predictions_lm_fur<-predict(fit_lm_fur,sup_fur)
actuals<-sup_fur$Profit
RMSE<-rmse(actuals,predictions_lm_fur)
RMSE # 26.36655
sup_fur$Predict_Profit<-predictions_lm_fur

#category Labels
fit_lm_lab<-lm(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_lab)
summary(fit_lm_lab) #Rsq=0.9834
predictions_lm_lab<-predict(fit_lm_lab,sup_lab)
actuals<-sup_lab$Profit
RMSE<-rmse(actuals,predictions_lm_lab)
RMSE # 3.882573
sup_lab$Predict_Profit<-predictions_lm_lab

#category Machines
fit_lm_mac<-lm(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_mac)
summary(fit_lm_mac) #Rsq=0.5388
predictions_lm_mac<-predict(fit_lm_mac,sup_mac)
actuals<-sup_mac$Profit
RMSE<-rmse(actuals,predictions_lm_mac)
RMSE # 456.4
sup_mac$Predict_Profit<-predictions_lm_mac

#category Paper
fit_lm_pap<-lm(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_pap)
summary(fit_lm_pap) #Rsq=0.9816
predictions_lm_pap<-predict(fit_lm_pap,sup_pap)
actuals<-sup_pap$Profit
RMSE<-rmse(actuals,predictions_lm_pap)
RMSE # 4.25
sup_pap$Predict_Profit<-predictions_lm_pap

#category Phones
fit_lm_pho<-lm(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_pho)
summary(fit_lm_pho) #Rsq=0.6823
predictions_lm_pho<-predict(fit_lm_pho,sup_pho)
actuals<-sup_pho$Profit
RMSE<-rmse(actuals,predictions_lm_pho)
RMSE # 56.48
sup_pho$Predict_Profit<-predictions_lm_pho

#category Storage
fit_lm_sto<-lm(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_sto)
summary(fit_lm_sto) #Rsq=0.723
predictions_lm_sto<-predict(fit_lm_sto,sup_sto)
actuals<-sup_sto$Profit
RMSE<-rmse(actuals,predictions_lm_sto)
RMSE # 29.38682
sup_sto$Predict_Profit<-predictions_lm_sto

#category Supplies
fit_lm_sup<-lm(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_sup)
summary(fit_lm_sup) #Rsq=0.01252
predictions_lm_sup<-predict(fit_lm_sup,sup_sup)
actuals<-sup_sup$Profit
RMSE<-rmse(actuals,predictions_lm_sup)
RMSE # 90.57
sup_sup$Predict_Profit<-predictions_lm_sup

#category Tables
fit_lm_tab<-lm(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_tab)
summary(fit_lm_tab) #Rsq=0.5859
predictions_lm_tab<-predict(fit_lm_tab,sup_tab)
actuals<-sup_tab$Profit
RMSE<-rmse(actuals,predictions_lm_tab)
RMSE # 132.1283
sup_tab$Predict_Profit<-predictions_lm_tab


#To get the overall rmse for the linear models
#put all predictions into a dataframe
sup_final<-rbind(sup_acc, sup_app, sup_art, sup_bind, sup_book, sup_cha, sup_cop, sup_env, sup_fas, sup_fur, sup_lab, sup_mac, sup_pap, sup_pho, sup_sto, sup_sup, sup_tab)
view(sup_final)

#evaluate model on a per transaction level
RMSE<-rmse(sup_final$Profit,sup_final$Predict_Profit)
RMSE #90.63011



#combine data for a per product model and compare resulting avg profit with agg by prod id
agg_sup_final<-sup_final %>% group_by(Product.ID) %>% 
  summarize(order_date_mean=mean(Order.Date, na.rm=TRUE),
            order_date_from_start=min(Order.Date, na.rm=TRUE)-smallest_date_order, 
            order_date_from_end=largest_date_order-max(Order.Date, na.rm=TRUE), 
            order_date_range=max(Order.Date)-min(Order.Date), 
            ship_date_mean=mean(Ship.Date, na.rm=TRUE), 
            average_shipping_delay=mean(Ship.Date, na.rm=TRUE)-mean(Order.Date, na.rm=TRUE),
            shipping_mode=as.factor(first(ifelse(is.na(Ship.Mode), 'NA', Ship.Mode))), 
            segment=as.factor(first(ifelse(is.na(Segment),NA, Segment))),
            segment_count=length(unique(factor(Segment))),
            city=as.factor(first(ifelse(is.na(City), 'NA', City))), 
            city_count=length(unique(factor(City))),
            state=as.factor(first(ifelse(is.na(State), 'NA', State))),
            state_count=length(unique(factor(State))),
            postal_code=as.factor(first(ifelse(is.na(Postal.Code), 'NA', Postal.Code))), 
            postal_code_count=length(unique(factor(Postal.Code))),
            region=as.factor(first(ifelse(is.na(Region), 'NA', Region))),  
            region_count=length(unique(factor(Region))),
            customer_ID=as.factor(first(ifelse(is.na(Customer.ID), 'NA', Customer.ID))),
            customer_count=length(unique(factor(Customer.ID))),
            category=as.factor(first(ifelse(is.na(Category), 'NA', Category))),
            sub_category=as.factor(first(ifelse(is.na(Sub.Category), 'NA', Sub.Category))),
            mean_of_sales=mean(Sales, na.rm=TRUE),  
            sales_count=sum(ifelse(Sales>0,1,0)), 
            sales_max=max(Sales, na.rm=TRUE), 
            sales_median=median(Sales, na.rm=TRUE), 
            quantity_mean=mean(Quantity, na.rm=TRUE),
            quantity_max=max(Quantity, na.rm=TRUE),
            quantity_median=median(Quantity, na.rm=TRUE),
            discount_max=max(Discount, na.rm=TRUE),
            discount_mean=mean(Discount, na.rm=TRUE),
            discount_count=sum(ifelse(Discount>0,1,0)), 
            discount_median=median(Discount, na.rm=TRUE),
            profit=sum(Profit, na.rm=TRUE),
            profit_per_product=profit/quantity_max,
            predict_profit=sum(Predict_Profit, na.rm=TRUE),
            predict_profit_per_product=Predict_Profit/quantity_max)

agg_sup_final<-agg_sup_final[!duplicated(agg_sup_final$Product.ID),]
view(agg_sup_final)

RMSE<-rmse(agg_sup_final$profit,agg_sup_final$predict_profit)
RMSE #324.667





#modeling per transaction mars

#category accessories
grid <- expand.grid(degree = 1:2, nprune=c(20,25,30))
mars.fit_acc<- train(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_acc, method = "earth",metric = "RMSE",  trControl = trainControl(method = "cv", number = 5), tuneGrid = grid)
mars.fit_acc$bestTune
summary(mars.fit_acc)#0.9739

#category appliances
mars.fit_app<- train(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_app, method = "earth",metric = "RMSE")
mars.fit_app$bestTune
summary(mars.fit_app) #0.79245

#category art
grid <- expand.grid(degree = 1:2, nprune=c(15,20,25))
mars.fit_art<- train(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_art, method = "earth",metric = "RMSE",  trControl = trainControl(method = "cv", number = 5), tuneGrid = grid)
mars.fit_art$bestTune
summary(mars.fit_art)#0.98935

#category binders
grid <- expand.grid(degree = 1:2, nprune=c(20,25,30))
mars.fit_bind<- train(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_bind, method = "earth",metric = "RMSE",  trControl = trainControl(method = "cv", number = 5), tuneGrid = grid)
mars.fit_bind$bestTune #0.99918
summary(mars.fit_bind)

#category bookcases
grid <- expand.grid(degree = 1:2, nprune=c(20,25,30))
mars.fit_book<- train(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_book, method = "earth",metric = "RMSE",  trControl = trainControl(method = "cv", number = 5), tuneGrid = grid)
mars.fit_book$bestTune# 0.98476
summary(mars.fit_book)

#category chairs
grid <- expand.grid(degree = 1:2, nprune=c(20,25,30))
mars.fit_cha<- train(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_cha, method = "earth",metric = "RMSE",  trControl = trainControl(method = "cv", number = 5), tuneGrid = grid)
mars.fit_cha$bestTune
summary(mars.fit_cha) #0.96987

#category copiers
grid <- expand.grid(degree = 1:2, nprune=c(20,25,30))
mars.fit_cop<- train(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_cop, method = "earth",metric = "RMSE",  trControl = trainControl(method = "cv", number = 5), tuneGrid = grid)
mars.fit_cop$bestTune
summary(mars.fit_cop) #0.9977

#category envelopes
grid <- expand.grid(degree = 1:2, nprune=c(20,25,30))
mars.fit_env<- train(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_env, method = "earth",metric = "RMSE",  trControl = trainControl(method = "cv", number = 5), tuneGrid = grid)
mars.fit_env$bestTune
summary(mars.fit_env) #0.9986

#category fasteners
grid <- expand.grid(degree = 1:2, nprune=c(20,25,30))
mars.fit_fas<- train(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_fas, method = "earth",metric = "RMSE",  trControl = trainControl(method = "cv", number = 5), tuneGrid = grid)
mars.fit_fas$bestTune
summary(mars.fit_fas) #0.99128

#category furnishings
grid <- expand.grid(degree = 1:2, nprune=c(25,30, 35))
mars.fit_fur<- train(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_fur, method = "earth",metric = "RMSE",  trControl = trainControl(method = "cv", number = 5), tuneGrid = grid)
mars.fit_fur$bestTune
summary(mars.fit_fur) #0.9619

#category labels
grid <- expand.grid(degree = 1:2, nprune=c(20,25, 30))
mars.fit_lab<- train(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_lab, method = "earth",metric = "RMSE",  trControl = trainControl(method = "cv", number = 5), tuneGrid = grid)
mars.fit_lab$bestTune
summary(mars.fit_lab) #0.999929

#category machines
grid <- expand.grid(degree = 1:2, nprune=c(3,5,8))
mars.fit_mac<- train(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_mac, method = "earth",metric = "RMSE",  trControl = trainControl(method = "cv", number = 5), tuneGrid = grid)
mars.fit_mac$bestTune
summary(mars.fit_mac) #0.95545

#category paper
grid <- expand.grid(degree = 1:2, nprune=c(3,5,8))
mars.fit_pap<- train(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_pap, method = "earth",metric = "RMSE",  trControl = trainControl(method = "cv", number = 5), tuneGrid = grid)
mars.fit_pap$bestTune
summary(mars.fit_pap) #0.9987745

#category phones
grid <- expand.grid(degree = 1:2, nprune=c(5,10,15))
mars.fit_pho<- train(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_pho, method = "earth",metric = "RMSE",  trControl = trainControl(method = "cv", number = 5), tuneGrid = grid)
mars.fit_pho$bestTune
summary(mars.fit_pho) #0.9831546

#category storage
grid <- expand.grid(degree = 1:2, nprune=c(25, 30, 35))
mars.fit_sto<- train(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_sto, method = "earth",metric = "RMSE",  trControl = trainControl(method = "cv", number = 5), tuneGrid = grid)
mars.fit_sto$bestTune
summary(mars.fit_sto) #0.9452

#category supplies
grid <- expand.grid(degree = 1:2, nprune=c(1, 3, 5))
mars.fit_sup<- train(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_sup, method = "earth",metric = "RMSE", tuneGrid = grid,trControl = trainControl(method = "cv", number = 5))
mars.fit_sup$bestTune
summary(mars.fit_sup) #0.99641

#category tables
grid <- expand.grid(degree = 1:2, nprune=c(20,25,30))
mars.fit_tab<- train(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_tab, method = "earth",metric = "RMSE",  trControl = trainControl(method = "cv", number = 5), tuneGrid = grid)
mars.fit_tab$bestTune 
summary(mars.fit_tab) #0.9740







#modeling per transactions svm

#category accessories
grid <- expand.grid(sigma = seq(0.0000001,0.0000002,length=5),
                    C = c(200, 225, 250, 275, 300))
svm.fit_acc <- train(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_acc,tuneGrid = grid,method="svmRadial")
svm.fit_acc
plot(svm.fit_acc)
summary(svm.fit_acc) #0.7774
predictions_acc<-predict(svm.fit_acc,sup_acc)
actuals_acc<-sup_acc$Profit
RMSE<-rmse(actuals_acc,predictions_acc)
RMSE #104.7708
sup_acc$Predict_Profit<-predictions_acc


#category appliances
grid <- expand.grid(sigma = seq(0.000000005,0.00000002,length=5),
                    C = c(400, 425, 450, 475, 500))
svm.fit_app <- train(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_app,tuneGrid = grid,method="svmRadial")
svm.fit_app
plot(svm.fit_app)
summary(svm.fit_app) #0.5662
predictions_app<-predict(svm.fit_app,sup_app)
actuals_app<-sup_app$Profit
RMSE<-rmse(actuals_app,predictions_app)
RMSE #148.733
sup_app$Predict_Profit<-predictions_app

#category art
grid <- expand.grid(sigma = seq(0.0000001,0.00001,length=5),
                    C = c(150,200, 250, 300, 350))
svm.fit_art <- train(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_art,tuneGrid = grid,method="svmRadial")
svm.fit_art
plot(svm.fit_art)
summary(svm.fit_art) #0.87115
predictions_art<-predict(svm.fit_art,sup_art)
actuals_art<-sup_art$Profit
RMSE<-rmse(actuals_art,predictions_art)
RMSE #7.6215
sup_art$Predict_Profit<-predictions_art

#category binders
grid <- expand.grid(sigma = seq(0.000000001,0.0000001,length=5),
                    C = c(300, 350, 400))
svm.fit_bind <- train(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_bind,tuneGrid = grid,method="svmRadial")
svm.fit_bind
plot(svm.fit_bind)
summary(svm.fit_bind) #0.3503
predictions_bind<-predict(svm.fit_bind,sup_bind)
actuals_bind<-sup_bind$Profit
RMSE<-rmse(actuals_bind,predictions_bind)
RMSE #304.5365
sup_bind$Predict_Profit<-predictions_bind


#category bookcases
grid <- expand.grid(sigma = seq(0.0000000001,0.00000001,length=5),
                    C = c(300,350,400))
svm.fit_book <- train(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_book,tuneGrid = grid,method="svmRadial")
svm.fit_book
plot(svm.fit_book)
summary(svm.fit_book) #0.1811
predictions_book<-predict(svm.fit_book,sup_book)
actuals_book<-sup_book$Profit
RMSE<-rmse(actuals_book,predictions_book)
RMSE #182.3816
sup_book$Predict_Profit<-predictions_book

#category chairs
grid <- expand.grid(sigma = seq(0.0000000001,0.001,length=5),
                    C = c(300,350,400))
svm.fit_cha <- train(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_cha,tuneGrid = grid,method="svmRadial")
svm.fit_cha
plot(svm.fit_cha)
summary(svm.fit_cha) #0.3632
predictions_cha<-predict(svm.fit_cha,sup_cha)
actuals_cha<-sup_cha$Profit
RMSE<-rmse(actuals_cha,predictions_cha)
RMSE #34.60156
sup_cha$Predict_Profit<-predictions_cha

#category copiers
grid <- expand.grid(sigma = seq(0.0001,0.01,length=5),
                    C = c(200,300,400, 500))
svm.fit_cop <- train(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_cop,tuneGrid = grid,method="svmRadial")
svm.fit_cop
plot(svm.fit_cop)
summary(svm.fit_cop) #0.7731
predictions_cop<-predict(svm.fit_cop,sup_cop)
actuals_cop<-sup_cop$Profit
RMSE<-rmse(actuals_cop,predictions_cop)
RMSE #111.3483
sup_cop$Predict_Profit<-predictions_cop

#category envelopes
grid <- expand.grid(sigma = seq(0.0000005,0.00005,length=5),
                    C = c(200,300,400))
svm.fit_env <- train(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_env,tuneGrid = grid,method="svmRadial")
svm.fit_env
plot(svm.fit_env)
summary(svm.fit_env) #0.96
predictions_env<-predict(svm.fit_env,sup_env)
actuals_env<-sup_env$Profit
RMSE<-rmse(actuals_env,predictions_env)
RMSE #8.67845
sup_env$Predict_Profit<-predictions_env

#category fasteners
grid <- expand.grid(sigma = seq(0.0001,0.01,length=5),
                    C = c(200,300,400))
svm.fit_fas <- train(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_fas,tuneGrid = grid,method="svmRadial")
svm.fit_fas
plot(svm.fit_fas)
summary(svm.fit_fas) #0.63
predictions_fas<-predict(svm.fit_fas,sup_fas)
actuals_fas<-sup_fas$Profit
RMSE<-rmse(actuals_fas,predictions_fas)
RMSE #0.440335
sup_fas$Predict_Profit<-predictions_fas

#category furnishings
grid <- expand.grid(sigma = seq(0.0001,0.001,length=3),
                    C = c(300,350,400))
svm.fit_fur <- train(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_fur,tuneGrid = grid,method="svmRadial")
svm.fit_fur
plot(svm.fit_fur)
summary(svm.fit_fur) #0.235
predictions_fur<-predict(svm.fit_fur,sup_fur)
actuals_fur<-sup_fur$Profit
RMSE<-rmse(actuals_fur,predictions_fur)
RMSE #20.21547
sup_fur$Predict_Profit<-predictions_fur

#category labels
grid <- expand.grid(sigma = seq(0.00000001,0.000001,length=5),
                    C = c(200, 300,400))
svm.fit_lab <- train(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_lab,tuneGrid = grid,method="svmRadial")
svm.fit_lab
plot(svm.fit_lab)
summary(svm.fit_lab) #0.9302
predictions_lab<-predict(svm.fit_lab,sup_lab)
actuals_lab<-sup_lab$Profit
RMSE<-rmse(actuals_lab,predictions_lab)
RMSE #132.88757
sup_lab$Predict_Profit<-predictions_lab

#category machines
grid <- expand.grid(sigma = seq(0.0000000000000000000001,0.0000000000000001,length=5),
                    C = c(100, 200, 300, 400))
svm.fit_mac <- train(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_mac,tuneGrid = grid,method="svmRadial")
svm.fit_mac
plot(svm.fit_mac)
summary(svm.fit_mac) #0.10137
predictions_mac<-predict(svm.fit_mac,sup_mac)
actuals_mac<-sup_mac$Profit
RMSE<-rmse(actuals_mac,predictions_mac)
RMSE #1095.229
sup_mac$Predict_Profit<-predictions_mac

#category paper
grid <- expand.grid(sigma = seq(0.00000000001,0.00001,length=5),
                    C = c(100, 200, 300, 400))
svm.fit_pap <- train(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_pap,tuneGrid = grid,method="svmRadial")
svm.fit_pap
plot(svm.fit_pap)
summary(svm.fit_pap)  #0.971515
predictions_pap<-predict(svm.fit_pap,sup_pap)
actuals_pap<-sup_pap$Profit
RMSE<-rmse(actuals_pap,predictions_pap)
RMSE #7.351244
sup_pap$Predict_Profit<-predictions_pap

#category phones
grid <- expand.grid(sigma = seq(0.00000000001,0.00001,length=5),
                    C = c(100, 200, 300, 400))
svm.fit_pho <- train(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_pho,tuneGrid = grid,method="svmRadial")
svm.fit_pho
plot(svm.fit_pho)
summary(svm.fit_pho) #0.41108
predictions_pho<-predict(svm.fit_pho,sup_pho)
actuals_pho<-sup_pho$Profit
RMSE<-rmse(actuals_pho,predictions_pho)
RMSE #91.02773
sup_pho$Predict_Profit<-predictions_pho

#category storage
grid <- expand.grid(sigma = seq(0.00000000001,0.00001,length=5),
                    C = c(100, 200, 300, 400))
svm.fit_sto <- train(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_sto,tuneGrid = grid,method="svmRadial")
svm.fit_sto
plot(svm.fit_sto)
summary(svm.fit_sto) #0.37038
predictions_sto<-predict(svm.fit_sto,sup_sto)
actuals_sto<-sup_sto$Profit
RMSE<-rmse(actuals_sto,predictions_sto)
RMSE #62.45118
sup_sto$Predict_Profit<-predictions_sto

#category supplies
grid <- expand.grid(sigma = seq(0.00000000001,0.00001,length=5),
                    C = c(100, 200, 300, 400))
svm.fit_sup <- train(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_sup,tuneGrid = grid,method="svmRadial")
svm.fit_sup
plot(svm.fit_sup)
summary(svm.fit_sup) #0.2342
predictions_sup<-predict(svm.fit_sup,sup_sup)
actuals_sup<-sup_sup$Profit
RMSE<-rmse(actuals_sup,predictions_sup)
RMSE #105.6458
sup_sup$Predict_Profit<-predictions_sup

#category tables
grid <- expand.grid(sigma = seq(0.000000000000001,0.000000001,length=5),
                    C = c(100, 200, 300, 400))
svm.fit_tab <- train(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_tab,tuneGrid = grid,method="svmRadial")
svm.fit_tab
plot(svm.fit_tab)
summary(svm.fit_tab) #0.0754
predictions_tab<-predict(svm.fit_tab,sup_tab)
actuals_tab<-sup_tab$Profit
RMSE<-rmse(actuals_tab,predictions_tab)
RMSE #230.9523
sup_tab$Predict_Profit<-predictions_tab

#To get the overall rmse for the svm models
#put all predictions into a dataframe
sup_final<-rbind(sup_acc, sup_app, sup_art, sup_bind, sup_book, sup_cha, sup_cop, sup_env, sup_fas, sup_fur, sup_lab, sup_mac, sup_pap, sup_pho, sup_sto, sup_sup, sup_tab)
view(sup_final)

#evaluate model on a per transaction level
RMSE<-rmse(sup_final$Profit,sup_final$Predict_Profit)
RMSE #183.8757



#combine data for a per product model and compare resulting avg profit with agg by prod id
agg_sup_final<-sup_final %>% group_by(Product.ID) %>% 
  summarize(order_date_mean=mean(Order.Date, na.rm=TRUE),
            order_date_from_start=min(Order.Date, na.rm=TRUE)-smallest_date_order, 
            order_date_from_end=largest_date_order-max(Order.Date, na.rm=TRUE), 
            order_date_range=max(Order.Date)-min(Order.Date), 
            ship_date_mean=mean(Ship.Date, na.rm=TRUE), 
            average_shipping_delay=mean(Ship.Date, na.rm=TRUE)-mean(Order.Date, na.rm=TRUE),
            shipping_mode=as.factor(first(ifelse(is.na(Ship.Mode), 'NA', Ship.Mode))), 
            segment=as.factor(first(ifelse(is.na(Segment),NA, Segment))),
            segment_count=length(unique(factor(Segment))),
            city=as.factor(first(ifelse(is.na(City), 'NA', City))), 
            city_count=length(unique(factor(City))),
            state=as.factor(first(ifelse(is.na(State), 'NA', State))),
            state_count=length(unique(factor(State))),
            postal_code=as.factor(first(ifelse(is.na(Postal.Code), 'NA', Postal.Code))), 
            postal_code_count=length(unique(factor(Postal.Code))),
            region=as.factor(first(ifelse(is.na(Region), 'NA', Region))),  
            region_count=length(unique(factor(Region))),
            customer_ID=as.factor(first(ifelse(is.na(Customer.ID), 'NA', Customer.ID))),
            customer_count=length(unique(factor(Customer.ID))),
            category=as.factor(first(ifelse(is.na(Category), 'NA', Category))),
            sub_category=as.factor(first(ifelse(is.na(Sub.Category), 'NA', Sub.Category))),
            mean_of_sales=mean(Sales, na.rm=TRUE),  
            sales_count=sum(ifelse(Sales>0,1,0)), 
            sales_max=max(Sales, na.rm=TRUE), 
            sales_median=median(Sales, na.rm=TRUE), 
            quantity_mean=mean(Quantity, na.rm=TRUE),
            quantity_max=max(Quantity, na.rm=TRUE),
            quantity_median=median(Quantity, na.rm=TRUE),
            discount_max=max(Discount, na.rm=TRUE),
            discount_mean=mean(Discount, na.rm=TRUE),
            discount_count=sum(ifelse(Discount>0,1,0)), 
            discount_median=median(Discount, na.rm=TRUE),
            profit=sum(Profit, na.rm=TRUE),
            profit_per_product=profit/quantity_max,
            predict_profit=sum(Predict_Profit, na.rm=TRUE),
            predict_profit_per_product=Predict_Profit/quantity_max)

agg_sup_final<-agg_sup_final[!duplicated(agg_sup_final$Product.ID),]
view(agg_sup_final)

RMSE<-rmse(agg_sup_final$profit,agg_sup_final$predict_profit)
RMSE #460.7212








#Tune Best model choices for each category (or if they are similar the faster model) with 5 fold cross validation
#Mars was better than SVM in all cases and better than linear in most cases. 
#When Mars and Linear are withing 0.002 I chose linear for the speed
#Best model for accessories
grid <- expand.grid(degree = 1:2, nprune=c(24,26,28))
mars.fit_acc<- train(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_acc, method = "earth",metric = "RMSE",  trControl = trainControl(method = "cv", number = 5), tuneGrid = grid)
mars.fit_acc$bestTune
summary(mars.fit_acc) #0.97348

#Best model for appliances
grid <- expand.grid(degree = 1:2, nprune=c(20,25,30))
mars.fit_app<- train(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_app, method = "earth",metric = "RMSE",  trControl = trainControl(method = "cv", number = 5), tuneGrid = grid)
mars.fit_app$bestTune
summary(mars.fit_app) #0.9938447

#Best model for art
grid <- expand.grid(degree = 1:2, nprune=c(15,20,25))
mars.fit_art<- train(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_art, method = "earth",metric = "RMSE",  trControl = trainControl(method = "cv", number = 5), tuneGrid = grid)
mars.fit_art$bestTune
summary(mars.fit_art)#0.98935

#Best model for binders
grid <- expand.grid(degree = 1:2, nprune=c(20,25,30))
mars.fit_bind<- train(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_bind, method = "earth",metric = "RMSE",  trControl = trainControl(method = "cv", number = 5), tuneGrid = grid)
mars.fit_bind$bestTune #0.99918
summary(mars.fit_bind)

#Best model for bookcases
grid <- expand.grid(degree = 1:2, nprune=c(23, 24, 25,26, 27, 28))
mars.fit_book<- train(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_book, method = "earth",metric = "RMSE",  trControl = trainControl(method = "cv", number = 5), tuneGrid = grid)
mars.fit_book$bestTune# 0.9841
summary(mars.fit_book)

#Best model for chairs
grid <- expand.grid(degree = 1:2, nprune=c(36, 37, 38, 39, 40, 41))
mars.fit_cha<- train(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_cha, method = "earth",metric = "RMSE",  trControl = trainControl(method = "cv", number = 5), tuneGrid = grid)
mars.fit_cha$bestTune
summary(mars.fit_cha) #0.97702

#Best model for copiers
grid <- expand.grid(degree = 1:2, nprune=c(6, 7, 8,  9, 10, 11))
mars.fit_cop<- train(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_cop, method = "earth",metric = "RMSE",  trControl = trainControl(method = "cv", number = 5), tuneGrid = grid)
mars.fit_cop$bestTune
summary(mars.fit_cop) #0.9972

#Best model for envelopes
grid <- expand.grid(degree = 1:2, nprune=c(20,25,30))
mars.fit_env<- train(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_env, method = "earth",metric = "RMSE",  trControl = trainControl(method = "cv", number = 5), tuneGrid = grid)
mars.fit_env$bestTune
summary(mars.fit_env) #0.9986

#Best model for fasteners
grid <- expand.grid(degree = 1:2, nprune=c(22, 23, 24, 25, 26, 27))
mars.fit_fas<- train(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_fas, method = "earth",metric = "RMSE",  trControl = trainControl(method = "cv", number = 5), tuneGrid = grid)
mars.fit_fas$bestTune
summary(mars.fit_fas) #0.979867

#Best model for furnishings
grid <- expand.grid(degree = 1:2, nprune=c(27,28, 29, 30, 31))
mars.fit_fur<- train(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_fur, method = "earth",metric = "RMSE",  trControl = trainControl(method = "cv", number = 5), tuneGrid = grid)
mars.fit_fur$bestTune
summary(mars.fit_fur) #0.9699

#Best model for labels
grid <- expand.grid(degree = 1:2, nprune=c(20,25, 30))
mars.fit_lab<- train(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_lab, method = "earth",metric = "RMSE",  trControl = trainControl(method = "cv", number = 5), tuneGrid = grid)
mars.fit_lab$bestTune
summary(mars.fit_lab) #0.999929

#Best model for machines
grid <- expand.grid(degree = 1:2, nprune=c( 7, 8, 9, 10, 11, 12))
mars.fit_mac<- train(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_mac, method = "earth",metric = "RMSE",  trControl = trainControl(method = "cv", number = 5), tuneGrid = grid)
mars.fit_mac$bestTune
summary(mars.fit_mac) #0.9852

#Best model for paper
grid <- expand.grid(degree = 1:2, nprune=c(3,5,8))
mars.fit_pap<- train(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_pap, method = "earth",metric = "RMSE",  trControl = trainControl(method = "cv", number = 5), tuneGrid = grid)
mars.fit_pap$bestTune
summary(mars.fit_pap) #0.9987745

#Best model for phones
grid <- expand.grid(degree = 1:2, nprune=c(5,10,15))
mars.fit_pho<- train(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_pho, method = "earth",metric = "RMSE",  trControl = trainControl(method = "cv", number = 5), tuneGrid = grid)
mars.fit_pho$bestTune
summary(mars.fit_pho) #0.9831546

#Best model for storage
grid <- expand.grid(degree = 1:2, nprune=c(50, 55, 60, 65))
mars.fit_sto<- train(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_sto, method = "earth",metric = "RMSE",  trControl = trainControl(method = "cv", number = 5), tuneGrid = grid)
mars.fit_sto$bestTune
summary(mars.fit_sto) #0.974365

#Best model for supplies
grid <- expand.grid(degree = 1:2, nprune=c(1, 3, 5))
mars.fit_sup<- train(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_sup, method = "earth",metric = "RMSE", tuneGrid = grid,trControl = trainControl(method = "cv", number = 5))
mars.fit_sup$bestTune
summary(mars.fit_sup) #0.99641

#Best model for tables
grid <- expand.grid(degree = 1:2, nprune=c( 20, 21, 22, 23, 24, 25,26, 27, 28))
mars.fit_tab<- train(Profit ~ Ship.Mode + Product.ID + Sales + Quantity + Discount + Region, data=sup_tab, method = "earth",metric = "RMSE",  trControl = trainControl(method = "cv", number = 5), tuneGrid = grid)
mars.fit_tab$bestTune 
summary(mars.fit_tab) #0.98147


#predictions and rmse for each model
#Best model for accessories
summary(mars.fit_acc)
predictions_acc<-predict(mars.fit_acc,sup_acc)
actuals_acc<-sup_acc$Profit
RMSE<-rmse(actuals_acc,predictions_acc)
RMSE #17.8335
sup_acc$Predict_Profit<-predictions_acc[,"y"]

#Best model for appliances
summary(mars.fit_app)
predictions_app<-predict(mars.fit_app,sup_app)
actuals_app<-sup_app$Profit
RMSE<-rmse(actuals_app,predictions_app)
RMSE #11.58036
sup_app$Predict_Profit<-predictions_app[,"y"]

#Best model for art
summary(mars.fit_art)
predictions_art<-predict(mars.fit_art,sup_art)
actuals_art<-sup_art$Profit
RMSE<-rmse(actuals_art,predictions_art)
RMSE #1.546684
sup_art$Predict_Profit<-predictions_art[,"y"]

#Best model for binders
summary(mars.fit_bind)
predictions_bind<-predict(mars.fit_bind,sup_bind)
actuals_bind<-sup_bind$Profit
RMSE<-rmse(actuals_bind,predictions_bind)
RMSE #12.36394
sup_bind$Predict_Profit<-predictions_bind[,"y"]

#Best model for bookcases
summary(mars.fit_book)
predictions_book<-predict(mars.fit_book,sup_book)
actuals_book<-sup_book$Profit
RMSE<-rmse(actuals_book,predictions_book)
RMSE #22.8748
sup_book$Predict_Profit<-predictions_book[,"y"]

#Best model for chairs
summary(mars.fit_cha)
predictions_cha<-predict(mars.fit_cha,sup_cha)
actuals_cha<-sup_cha$Profit
RMSE<-rmse(actuals_cha,predictions_cha)
RMSE #19.25112
sup_cha$Predict_Profit<-predictions_cha[,"y"]

#Best model for copiers
summary(mars.fit_cop)
predictions_cop<-predict(mars.fit_cop,sup_cop)
actuals_cop<-sup_cop$Profit
RMSE<-rmse(actuals_cop,predictions_cop)
RMSE #76.21223
sup_cop$Predict_Profit<-predictions_cop[,"y"]

#Best model for envelopes
summary(mars.fit_env)
predictions_env<-predict(mars.fit_env,sup_env)
actuals_env<-sup_env$Profit
RMSE<-rmse(actuals_env,predictions_env)
RMSE #1.302717
sup_env$Predict_Profit<-predictions_env[,"y"]

#Best model for fasteners
summary(mars.fit_fas)
predictions_fas<-predict(mars.fit_fas,sup_fas)
actuals_fas<-sup_fas$Profit
RMSE<-rmse(actuals_fas,predictions_fas)
RMSE #0.7156
sup_fas$Predict_Profit<-predictions_fas[,"y"]

#Best model for furnishings
summary(mars.fit_fur)
predictions_fur<-predict(mars.fit_fur,sup_fur)
actuals_fur<-sup_fur$Profit
RMSE<-rmse(actuals_fur,predictions_fur)
RMSE #9.146086
sup_fur$Predict_Profit<-predictions_fur[,"y"]

#Best model for labels
summary(mars.fit_lab)
predictions_lab<-predict(mars.fit_lab,sup_lab)
actuals_lab<-sup_lab$Profit
RMSE<-rmse(actuals_lab,predictions_lab)
RMSE #0.9047785
sup_lab$Predict_Profit<-predictions_lab[,"y"]

#Best model for machines
summary(mars.fit_mac)
predictions_mac<-predict(mars.fit_mac,sup_mac)
actuals_mac<-sup_mac$Profit
RMSE<-rmse(actuals_mac,predictions_mac)
RMSE #133.1609
sup_mac$Predict_Profit<-predictions_mac[,"y"]

#Best model for paper
summary(mars.fit_pap)
predictions_pap<-predict(mars.fit_pap,sup_pap)
actuals_pap<-sup_pap$Profit
RMSE<-rmse(actuals_pap,predictions_pap)
RMSE #1.23269
sup_pap$Predict_Profit<-predictions_pap[,"y"]

#Best model for phones
summary(mars.fit_pho)
predictions_pho<-predict(mars.fit_pho,sup_pho)
actuals_pho<-sup_pho$Profit
RMSE<-rmse(actuals_pho,predictions_pho)
RMSE #14.69062
sup_pho$Predict_Profit<-predictions_pho[,"y"]

#Best model for storage
summary(mars.fit_sto)
predictions_sto<-predict(mars.fit_sto,sup_sto)
actuals_sto<-sup_sto$Profit
RMSE<-rmse(actuals_sto,predictions_sto)
RMSE #13.10899
sup_sto$Predict_Profit<-predictions_sto[,"y"]

#Best model for supplies
summary(mars.fit_sup)
predictions_sup<-predict(mars.fit_sup,sup_sup)
actuals_sup<-sup_sup$Profit
RMSE<-rmse(actuals_sup,predictions_sup)
RMSE #6.48085
sup_sup$Predict_Profit<-predictions_sup[,"y"]

#Best model for tables
summary(mars.fit_tab)
predictions_tab<-predict(mars.fit_tab,sup_tab)
actuals_tab<-sup_tab$Profit
RMSE<-rmse(actuals_tab,predictions_tab)
RMSE #31.337
sup_tab$Predict_Profit<-predictions_tab[,"y"]

#put all predictions into a dataframe
sup_final<-rbind(sup_acc, sup_app, sup_art, sup_bind, sup_book, sup_cha, sup_cop, sup_env, sup_fas, sup_fur, sup_lab, sup_mac, sup_pap, sup_pho, sup_sto, sup_sup, sup_tab)
view(sup_final)

#evaluate model on a per transaction level
RMSE<-rmse(sup_final$Profit,sup_final$Predict_Profit)
RMSE #20.17256

#combine data for a per product model and compare resulting avg profit with agg by prod id
agg_sup_final<-sup_final %>% group_by(Product.ID) %>% 
            summarize(order_date_mean=mean(Order.Date, na.rm=TRUE),
            order_date_from_start=min(Order.Date, na.rm=TRUE)-smallest_date_order, 
            order_date_from_end=largest_date_order-max(Order.Date, na.rm=TRUE), 
            order_date_range=max(Order.Date)-min(Order.Date), 
            ship_date_mean=mean(Ship.Date, na.rm=TRUE), 
            average_shipping_delay=mean(Ship.Date, na.rm=TRUE)-mean(Order.Date, na.rm=TRUE),
            shipping_mode=as.factor(first(ifelse(is.na(Ship.Mode), 'NA', Ship.Mode))), 
            segment=as.factor(first(ifelse(is.na(Segment),NA, Segment))),
            segment_count=length(unique(factor(Segment))),
            city=as.factor(first(ifelse(is.na(City), 'NA', City))), 
            city_count=length(unique(factor(City))),
            state=as.factor(first(ifelse(is.na(State), 'NA', State))),
            state_count=length(unique(factor(State))),
            postal_code=as.factor(first(ifelse(is.na(Postal.Code), 'NA', Postal.Code))), 
            postal_code_count=length(unique(factor(Postal.Code))),
            region=as.factor(first(ifelse(is.na(Region), 'NA', Region))),  
            region_count=length(unique(factor(Region))),
            customer_ID=as.factor(first(ifelse(is.na(Customer.ID), 'NA', Customer.ID))),
            customer_count=length(unique(factor(Customer.ID))),
            category=as.factor(first(ifelse(is.na(Category), 'NA', Category))),
            sub_category=as.factor(first(ifelse(is.na(Sub.Category), 'NA', Sub.Category))),
            mean_of_sales=mean(Sales, na.rm=TRUE),  
            sales_count=sum(ifelse(Sales>0,1,0)), 
            sales_max=max(Sales, na.rm=TRUE), 
            sales_median=median(Sales, na.rm=TRUE), 
            quantity_mean=mean(Quantity, na.rm=TRUE),
            quantity_max=max(Quantity, na.rm=TRUE),
            quantity_median=median(Quantity, na.rm=TRUE),
            discount_max=max(Discount, na.rm=TRUE),
            discount_mean=mean(Discount, na.rm=TRUE),
            discount_count=sum(ifelse(Discount>0,1,0)), 
            discount_median=median(Discount, na.rm=TRUE),
            profit=sum(Profit, na.rm=TRUE),
            profit_per_product=profit/quantity_max,
            predict_profit=sum(Predict_Profit, na.rm=TRUE),
            predict_profit_per_product=Predict_Profit/quantity_max)

agg_sup_final<-agg_sup_final[!duplicated(agg_sup_final$Product.ID),]
view(agg_sup_final)

RMSE<-rmse(agg_sup_final$profit,agg_sup_final$predict_profit)
RMSE #70.68937

write.csv(sup_final,"C:\\Users\\Beth\\Desktop\\Final_Project.csv")
write.csv(agg_sup_final,"C:\\Users\\Beth\\Desktop\\Final_Project_Aggregated.csv")

#aggregate train data based off product
length(sup_final$Profit)
length(sup_raw$Profit)

length(agg_sup_final$profit)
length(agg_sup$profit)


















#Lets see the rmse without way worse rmses copiers and machines
#put all predictions into a dataframe except copiers and machines
sup_final_no_cop_mac<-rbind(sup_acc, sup_app, sup_art, sup_bind, sup_book, sup_cha, sup_env, sup_fas, sup_fur, sup_lab, sup_pap, sup_pho, sup_sto, sup_sup, sup_tab)
view(sup_final_no_cop_mac)

#evaluate model on a per transaction level
RMSE<-rmse(sup_final_no_cop_mac$Profit,sup_final_no_cop_mac$Predict_Profit)
RMSE #12.90042

#combine data for a per product model and compare resulting avg profit with agg by prod id
agg_sup_final_no_cop_mac<-sup_final_no_cop_mac %>% group_by(Product.ID) %>% 
  summarize(order_date_mean=mean(Order.Date, na.rm=TRUE),
            order_date_from_start=min(Order.Date, na.rm=TRUE)-smallest_date_order, 
            order_date_from_end=largest_date_order-max(Order.Date, na.rm=TRUE), 
            order_date_range=max(Order.Date)-min(Order.Date), 
            ship_date_mean=mean(Ship.Date, na.rm=TRUE), 
            average_shipping_delay=mean(Ship.Date, na.rm=TRUE)-mean(Order.Date, na.rm=TRUE),
            shipping_mode=as.factor(first(ifelse(is.na(Ship.Mode), 'NA', Ship.Mode))), 
            segment=as.factor(first(ifelse(is.na(Segment),NA, Segment))),
            segment_count=length(unique(factor(Segment))),
            city=as.factor(first(ifelse(is.na(City), 'NA', City))), 
            city_count=length(unique(factor(City))),
            state=as.factor(first(ifelse(is.na(State), 'NA', State))),
            state_count=length(unique(factor(State))),
            postal_code=as.factor(first(ifelse(is.na(Postal.Code), 'NA', Postal.Code))), 
            postal_code_count=length(unique(factor(Postal.Code))),
            region=as.factor(first(ifelse(is.na(Region), 'NA', Region))),  
            region_count=length(unique(factor(Region))),
            customer_ID=as.factor(first(ifelse(is.na(Customer.ID), 'NA', Customer.ID))),
            customer_count=length(unique(factor(Customer.ID))),
            category=as.factor(first(ifelse(is.na(Category), 'NA', Category))),
            sub_category=as.factor(first(ifelse(is.na(Sub.Category), 'NA', Sub.Category))),
            mean_of_sales=mean(Sales, na.rm=TRUE),  
            sales_count=sum(ifelse(Sales>0,1,0)), 
            sales_max=max(Sales, na.rm=TRUE), 
            sales_median=median(Sales, na.rm=TRUE), 
            quantity_mean=mean(Quantity, na.rm=TRUE),
            quantity_max=max(Quantity, na.rm=TRUE),
            quantity_median=median(Quantity, na.rm=TRUE),
            discount_max=max(Discount, na.rm=TRUE),
            discount_mean=mean(Discount, na.rm=TRUE),
            discount_count=sum(ifelse(Discount>0,1,0)), 
            discount_median=median(Discount, na.rm=TRUE),
            profit=sum(Profit, na.rm=TRUE),
            profit_per_product=profit/quantity_max,
            predict_profit=sum(Predict_Profit, na.rm=TRUE),
            predict_profit_per_product=Predict_Profit/quantity_max)

agg_sup_final_no_cop_mac<-agg_sup_final_no_cop_mac[!duplicated(agg_sup_final_no_cop_mac$Product.ID),]
view(agg_sup_final_no_cop_mac)

RMSE<-rmse(agg_sup_final_no_cop_mac$profit,agg_sup_final_no_cop_mac$predict_profit)
RMSE #56.48035
