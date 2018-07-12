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
library(RColorBrewer)


#loading of dataset
train <- read.csv("sales_train_v2.csv")
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

x<- subset(SalesData, Month == 1 | Month == 2 | Month == 3)
x$Season <- "Spring"
x1<- subset(SalesData, Month == 4 | Month == 5 | Month == 6)
x1$Season <-"Summer"
x2<- subset(SalesData, Month == 7 | Month == 8 | Month == 9)
x2$Season <-"Autumn"
x3<- subset(SalesData, Month == 10 | Month == 11 | Month == 12)
x3$Season <-"Winter"

SalesData <- bind_rows(list(x,x1,x2,x3)) #getting season name


####---Manipulation of Dataset---####
#1.How many shops are there? Which shop is most popular?

#a. How many shops are there?
TotalShops <- SalesData %>% select(shop_id) %>% distinct() %>% count() #pipeoperator format

TotalShops <- count(distinct(select(SalesData, shop_id)))

#b.Which shop is most popular?
SalesData %>% group_by(shop_id,shop_name) %>%  summarise(SalesPerShop=sum(item_cnt_day)) %>%   ungroup() %>% #pipeoperator format
  arrange(desc(SalesPerShop)) %>% head(20) -> popular_shop

Popularshop <- group_by(SalesData, shop_id, shop_name) #group dataset based on shop_id and shop_name
Popularshop <- summarise(Popularshop, SalesPerShop = sum(item_cnt_day))#shows the number of sales per shop per day, in grouped format
Popularshop <- as.data.frame(Popularshop) #ungroup dataset
Popularshop <- arrange(Popularshop, desc(SalesPerShop)) #sorting Salespershop in descending order
Popularshop <- head(Popularshop, 20) #restrict to showing just the top 20
Popularshop[1,]#shows the most popular shop

#Plotting of Top 20 most popular shops
ggplot(Popularshop, aes(x = reorder(shop_id, SalesPerShop), y = SalesPerShop, fill=shop_id)) + 
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Top 20 Most Popular Shops", x= "Shop ID", y = "No. of Sales", fill = "Shop ID") +
  theme_bw()

rm (popular_shop)
rm (TotalShops)
########################################################

#2. How many items are there? Which item is most popular?

#a).How many items are there

TotalItems <- count(distinct(SalesData, SalesData$item_id))

#b)Which item is most popular?

popularitem <- group_by(SalesData, item_id, item_name)
popularitem <- summarize(popularitem, qtysold = sum(item_cnt_day)) #summarize creates a new dataframe
popularitem <- as.data.frame(popularitem)
popularitem <- arrange(popularitem, desc(popularitem$qtysold))
popularitem[1,]

#c)Which item is most popular by shop
popitembyshop <- group_by(SalesData, shop_id, item_id)
popitembyshop <- summarize(popitembyshop, qtysold = sum(item_cnt_day))
popitembyshop <- filter(popitembyshop, qtysold == max(qtysold))
popitembyshop <- arrange(popitembyshop, desc(popitembyshop$qtysold))
popitembyshop <- as.data.frame(popitembyshop)

#graph
ggplot(popitembyshop, aes(x = reorder(shop_id, qtysold), y = qtysold, fill = item_id)) + 
  geom_bar(stat = "identity") +
  labs(title = "Most Popular Items by Shop", x= "Shop ID", y = "Quantity Sold", fill = "Item ID") +
  theme_bw()

rm(popularitem)
rm(Popularshop)
rm(TotalItems)
rm(popitembyshop)

##-----Monthly Sales-----##
#6. Month-wise total sales
MonthlySales <- group_by(SalesData, Year, Month)
MonthlySales <- summarise(MonthlySales, ItemSales=sum(item_cnt_day * item_price)) #Shows the number of sales per month
MonthlySales <- arrange(MonthlySales, Year)
MonthlySales <- as.data.frame(MonthlySales)

MonthlySales

#Graph
ggplot(MonthlySales, aes(x=Month, y= ItemSales, fill=Year)) + 
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Monthly Total Sales", x="Month", y="Item Sales") +
  theme_bw()

##-----Sales Based on Seasonality----##
MonthlySales$Month <- as.numeric(MonthlySales$Month)
x <- subset(MonthlySales, Month > 0 & Month < 4)#manipulate month -> quarter
x$Quarter <- 1
x1 <- subset(MonthlySales, Month > 3 & Month < 7)#manipulate month -> quarter
x1$Quarter <- 2
x2 <- subset(MonthlySales, Month > 6 & Month < 10)#manipulate month -> quarter
x2$Quarter <- 3
x3 <- subset(MonthlySales, Month > 9 & Month < 13)#manipulate month -> quarter
x3$Quarter <- 4

MonthlySales <- bind_rows(list(x, x1, x2, x3))

myPalette <- colorRampPalette(rev(brewer.pal(4, "Spectral")))

ggplot(MonthlySales, aes(x=Month, y= ItemSales, fill=Quarter)) + 
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks=c(1:12)) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_gradientn(colours = myPalette(4)) +
  facet_wrap(~Year) +
  labs(title = "Monthly Total Sales", x="Month", y="Item Sales") +
  theme_bw()

#Model Building - XGBoost
