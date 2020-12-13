library("dplyr")
library("ggplot2")
library("lubridate")

url <- paste( "C:/Users/KANGHEA/OneDrive - Hilti/Study/WQD7001",
              "/Assignment/Group Assignment", 
              "/US Superstore data.csv", sep = "")

superStoreData <- read.csv(url)

str(superStoreData)

#===============================================================================
# Convert order date to correct format
superStoreData <- superStoreData %>%
  rowwise() %>% 
  mutate( Order.Date = strptime(as.character(Order.Date), 
                                "%m/%d/%Y") )

superStoreData <- superStoreData %>%
  rowwise() %>% 
  mutate( Order.Date = format(Order.Date, "%Y-%m-%d"))

#===============================================================================

#===============================================================================
# Populate weekday, month and year
superStoreData <- superStoreData %>%
  rowwise() %>% 
  mutate( weekDay = strftime(Order.Date, format = "%V"))

superStoreData <- superStoreData %>%
  rowwise() %>% 
  mutate( year = strftime(Order.Date, format = "%Y"))

superStoreData <- superStoreData %>%
  rowwise() %>% 
  mutate( month = strftime(Order.Date, format = "%m"))
#===============================================================================

#===============================================================================
#Convert delivery date to correct format
superStoreData <- superStoreData %>%
  rowwise() %>% 
  mutate( Ship.Date = strptime(as.character(Ship.Date), 
                                "%m/%d/%Y") )

superStoreData <- superStoreData %>%
  rowwise() %>% 
  mutate( Ship.Date = format(Ship.Date, "%Y-%m-%d"))

#Find the different between delivery and order date
superStoreData <- superStoreData %>%
  rowwise() %>% 
  mutate(shipDays = as.numeric(difftime(Ship.Date, Order.Date, units = "days")))

superStoreData$shipDays <- cut(superStoreData$shipDays, 
                               breaks=c(-1, 2, 4, 6, 8), 
                               c("0 - 2", "3 - 4", "5 - 6", "7 - 8"))
#===============================================================================


#===============================================================================
# Save dataframe to csv file for sharing :)
write.csv(superStoreData, file="~/superStoreData.csv",
          row.names=FALSE)

#===============================================================================

#===============================================================================
# Analysis Top 10 - Sales Product
#===============================================================================
top10Product <- superStoreData %>%                     # Specify data frame
  group_by(Product.ID) %>%                   # Specify group indicator
  summarise_at(vars(Sales),              # Specify column
               list(totalSales = sum)) 

top10Product <- top10Product %>% 
  arrange(desc(totalSales)) %>%
  top_n(10)

ggplot(data=top10Product, aes(x=Product.ID, y=totalSales, fill=Product.ID)) +
  geom_bar(colour="black", stat="identity")
#  guides(fill=FALSE)

#===============================================================================
# Analysis Bottom 10 - Sales Product
#===============================================================================
bot10Product <- superStoreData %>%                     # Specify data frame
  group_by(Product.ID) %>%                   # Specify group indicator
  summarise_at(vars(Sales),              # Specify column
               list(totalSales = sum)) 

bot10Product <- bot10Product %>% 
  arrange(desc(totalSales)) %>%
  top_n(-10)

ggplot(data=bot10Product, aes(x=Product.ID, y=totalSales, fill=Product.ID)) +
  geom_bar(colour="black", stat="identity")
#  guides(fill=FALSE)

#===============================================================================
# Analysis Top 10 - Sub Category - 20201213
# Because category only have 3 data, dont' need to do top 10
#===============================================================================
top10SubCat <- superStoreData %>%                     # Specify data frame
  group_by(Sub.Category) %>%                   # Specify group indicator
  summarise_at(vars(Sales),              # Specify column
               list(totalSales = sum)) 

top10SubCat <- top10SubCat %>% 
  arrange(desc(totalSales)) %>%
  top_n(10)

ggplot(data=top10SubCat, aes(x=Sub.Category, y=totalSales, fill=Sub.Category)) +
  geom_bar(colour="black", stat="identity") +
  guides(fill=FALSE)

#===============================================================================
# Analysis Bottom 10 - Sub Category - 20201213
# Because category only have 3 data, dont' need to do top 10
#===============================================================================
bot10SubCat <- superStoreData %>%                     # Specify data frame
  group_by(Sub.Category) %>%                   # Specify group indicator
  summarise_at(vars(Sales),              # Specify column
               list(totalSales = sum)) 

bot10SubCat <- bot10SubCat %>% 
  arrange(desc(totalSales)) %>%
  top_n(-10)

ggplot(data=bot10SubCat, aes(x=Sub.Category, y=totalSales, fill=Sub.Category)) +
  geom_bar(colour="black", stat="identity") +
  guides(fill=FALSE)

#===============================================================================
# Trend analysis for month
#===============================================================================
totalSalesYear <- superStoreData %>%                     # Specify data frame
  group_by(month, year) %>%                   # Specify group indicator
  summarise_at(vars(Sales),              # Specify column
               list(totalSales = sum)) 

# Visualization
totalSalesYear %>%
  ggplot( aes(x=month, y=totalSales, group=year, color=year)) +
  geom_line()

#===============================================================================
# Region category sales
#===============================================================================
# can try to filter by year
RegionSales <- superStoreData %>%                     # Specify data frame
  group_by(Region,Category) %>%                   # Specify group indicator
  summarise_at(vars(Sales),              # Specify column
               list(totalSales = sum))

#ggplot(top5RegionSales, aes(x = Region, y = totalSales))+
#  geom_col(aes(fill = Category), width = 0.7)

ggplot(RegionSales, aes(Region,totalSales, fill=Category))+
  geom_bar(stat = "identity", position = 'dodge')
#===============================================================================
# Region category profit  - 20201213
#===============================================================================

# can try to filter by year
regionCatProfit <- superStoreData %>%                     # Specify data frame
  group_by(Region,Category) %>%                   # Specify group indicator
  summarise_at(vars(Profit),              # Specify column
               list(totalProfit = sum))

ggplot(regionCatProfit, aes(x = Region, y = totalProfit, fill=Category))+
#  geom_col(aes(fill = Category), width = 0.7)
  geom_bar(stat = "identity", position = 'dodge')

#ggplot(RegionSales, aes(Region,totalSales, fill=Category))+
#  geom_bar(stat = "identity", position = 'dodge')

#===============================================================================
# Trend analysis for order ID per month
#===============================================================================
totalOrderID <- superStoreData %>%                     # Specify data frame
  group_by(month, year) %>%                   # Specify group indicator
  summarise(uniqueOrderId = n_distinct(Order.ID)) 

# Visualization
totalOrderID %>%
  ggplot( aes(x=month, y=uniqueOrderId, group=year, color=year)) +
  geom_line()

#===============================================================================
# Trend analysis for product cat per month
#===============================================================================
productCatSales <- superStoreData %>%                     # Specify data frame
  group_by(month, Category) %>%                   # Specify group indicator
  summarise_at( vars(Sales),              # Specify column
             list(totalSales = sum)) 

# Visualization
productCatSales %>%
  ggplot( aes(x=month, y=totalSales, group=Category, color=Category)) +
  geom_line()

#===============================================================================
# Chart about number of delivery per duration (delivery date - Order) - 20201213
#===============================================================================
# can try to filter by year
deliveryPerDuration <- superStoreData %>%                     # Specify data frame
  group_by(Region,shipDays) %>%                   # Specify group indicator
  summarise(uniqueOrderId = n_distinct(Order.ID))

ggplot(deliveryPerDuration, aes(x = Region, y = uniqueOrderId, fill = shipDays))+
#  geom_col(aes(fill = shipDays), width = 0.7)
  geom_bar(stat = "identity", position = 'dodge')

#===============================================================================
# Sales per customer group - 20201213
#===============================================================================
# can try to filter by year
totalSalesCustGroup <- superStoreData %>%                     # Specify data frame
  group_by(Segment, year) %>%                   # Specify group indicator
  summarise_at(vars(Sales),              # Specify column
               list(totalSales = sum)) 

# Visualization
ggplot(totalSalesCustGroup, aes(x = Segment, y = totalSales, fill=year))+
  #  geom_col(aes(fill = year), width = 0.7)
  geom_bar(stat = "identity", position = 'dodge')

#===============================================================================
# Chart about ship mode per customer group - 20201213
#===============================================================================
# can try to filter by year
shipModeCust <- superStoreData %>%                     # Specify data frame
  group_by(Segment,Ship.Mode) %>%                   # Specify group indicator
  summarise(uniqueOrderId = n_distinct(Order.ID))

ggplot(shipModeCust, aes(x = Segment, y = uniqueOrderId, fill = Ship.Mode))+
  #  geom_col(aes(fill = shipDays), width = 0.7)
  geom_bar(stat = "identity", position = 'dodge')
 