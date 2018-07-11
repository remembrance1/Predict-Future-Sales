#Forecast the total amount of products sold in every shop for the test set. 
#Note that the list of shops and products slightly changes every month
#Forecast the sales for these shops and products for November 2015.

#loading of libraries
library(ggplot2)
library(gridExtra)
library(strsplit)
library(lubridate)       # For date manipulation
library(magrittr)
library(dplyr)

#loading of dataset
train <- read.csv("sales_train.csv")
shops <- read.csv("shops.csv")
items <- read.csv("items.csv")
itemscat <- read.csv("item_categories.csv")
test <- read.csv("test.csv")

#Join datasets 
#SalesData <- train %>% left_join(items, by='item_id') %>%  left_join(shops, by='shop_id') 
#SalesData <- SalesData %>% left_join(itemscat, by='item_category_id') 

SalesData <- left_join(train, items, by='item_id')
SalesData <- left_join(SalesData, shops, by='shop_id')
SalesData <- left_join(SalesData, itemscat, by='item_category_id')

#transformation of data
SalesData$date <- dmy(SalesData$date)
SalesData$Year <- year(SalesData$date)
SalesData$Month <- month(SalesData$date)
SalesData$Day <- day(SalesData$date)
SalesData$Weekday <- weekdays(SalesData$date)
SalesData$Quarter <- quarter(SalesData$date)

#Changing fields to factor
SalesData$shop_id = as.factor(SalesData$shop_id)
SalesData$item_id = as.factor(SalesData$item_id)
SalesData$item_category_id = as.factor(SalesData$item_category_id)
SalesData$Year = as.factor(SalesData$Year)
SalesData$Month = as.factor(SalesData$Month)
SalesData$Day = as.factor(SalesData$Day)
SalesData$Weekday = as.factor(SalesData$Weekday)
SalesData$Quarter = as.factor(SalesData$Quarter)

####---Manipulation of Dataset---#
#1.How many shops are there? Which shop is most popular?

#a. How many shops are there?
TotalShops <- SalesData %>% select(shop_id) %>% distinct() %>% count()
TotalShops

#b.Which shop is most popular?
SalesData %>% group_by(shop_id,shop_name) %>%  summarise(SalesPerShop=sum(item_cnt_day)) %>%   ungroup() %>%
  arrange(desc(SalesPerShop)) %>% head(20) -> popular_shop

head(popular_shop,1)

#Graph
ggplot(popular_shop, aes(x = reorder(shop_id, SalesPerShop), y = SalesPerShop, fill=shop_id)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Top 20 most poular shops", x= "Shop Numbers", y = "Selling Qty", fill = "Shop id") +
  theme_solarized()

rm (popular_shop)
rm (TotalShops)
########################################################
