## Data exploration

getwd()

library(data.table)
library(ggplot2)
library(lubridate)

## Loading the data
filename <- "Online Retail.csv"
retail <- fread(filename)
print("Data loaded")

## Summary statistics
summary(retail)
str(retail)

## Checking for unique countries and stockcodes
print(unique(retail$Country))
print(length(unique(retail$StockCode)))

## Checking where NA values are
sapply(retail, function(x) sum(is.na(x)))

## Creating new revenue variable
retail$Revenue <- retail$Quantity * retail$UnitPrice

## Changing InvoiceDate variable to DateTime
retail$InvoiceDate <- dmy_hm(retail$InvoiceDate)

## Creating subset of data only looking at top and bottom ten countries 
top_countries <- aggregate(retail$Revenue, by=list(Country=retail$Country), FUN = sum)
names(top_countries)[2] <- "Total_Revenue"
top_countries <- top_countries[order(top_countries$Total_Revenue, decreasing = TRUE),]
top_countries

top_ten_countries <- head(top_countries, 10)
top_ten_countries

bottom_ten_countries <- tail(top_countries, 10)
bottom_ten_countries

## Graphing bar plots
p <- ggplot(data=top_ten_countries, aes(x=Country, y=Total_Revenue)) +
  geom_bar(stat="identity")
p

## Creating subset of data only looking at top and bottom ten products 
top_products <- aggregate(retail$Revenue, by=list(Products=retail$Description), FUN = sum)
names(top_products)[2] <- "Total_Revenue"
top_products <- top_products[order(top_products$Total_Revenue, decreasing = TRUE),]
top_products

top_ten_products <- head(top_products, 10)
top_ten_products

bottom_ten_products <- tail(top_products, 10)
bottom_ten_products 

## Summing revenue by month
retail$month <- month(retail$InvoiceDate)
retail$year <- year(retail$InvoiceDate)

## Aggregating by month
top_months <- aggregate(retail$Revenue, by=list(Year=retail$year, Month=retail$month), FUN = sum)
names(top_months)[3] <- "Total_Revenue"
top_months <- top_months[order(top_months$Total_Revenue, decreasing = TRUE),]
top_months

## Looking at it in chronological order
top_months_ordered <- top_months[order(top_months$Year, top_months$Month, decreasing = FALSE),]
top_months_ordered$Month[1] <- 0

t <- ggplot(data=retail, aes(x=InvoiceDate, y=Revenue)) +
  geom_line()
t

u <- ggplot(data=top_months_ordered, aes(x=Month, y=Total_Revenue)) +
  geom_line()
u

## Creating subset of data only looking at top ten customers 
top_customers <- aggregate(retail$Revenue, by=list(CustomerID=retail$CustomerID), FUN = sum)
names(top_customers)[2] <- "Total_Revenue"
top_customers <- top_customers[order(top_customers$Total_Revenue, decreasing = TRUE),]
top_customers

q <- ggplot(data=top_customers, aes(x=Customer, y=Total_Revenue)) +
  geom_bar(stat="identity")
q

r <- ggplot(data=top_customers, aes(x=Total_Revenue)) +
  geom_histogram(binwidth = 100)
r

s <- ggplot(data=top_customers, aes(x=Total_Revenue)) +
  geom_histogram(binwidth = 100, color = "cyan", fill = "purple") + xlim(0,5000)
s

## Top customers by country to determine wholesale
wholesale <- aggregate(Revenue~Country+CustomerID, data = retail, FUN = sum)
names(wholesale)[3] <- "Total_Revenue"
wholesale <- wholesale[order(wholesale$Total_Revenue, decreasing = TRUE),]
wholesale

## See all purchases by specific customer
customer_number <- 18143
customer_purchases <- retail[retail$CustomerID==customer_number]
customer_purchases

## Percentage of income from wholesale (customer spent over 1000)
income_share <- subset(top_customers, Total_Revenue >= 1000)
income_share_sum <- sum(income_share$Total_Revenue)
income_total <- sum(top_customers$Total_Revenue)
income_share_sum/income_total

## Percentage of wholesale by countries
wholesale_countries <- aggregate(wholesale$Total_Revenue, by=list(Country=wholesale$Country), FUN = sum)
names(wholesale_countries)[2] <- "Total_Revenue"
wholesale_countries <- wholesale_countries[order(wholesale_countries$Total_Revenue, decreasing = TRUE),]

wholesale_countries_pct <- wholesale_countries
wholesale_countries_pct <- wholesale_countries_pct[order(wholesale_countries_pct$Total_Revenue, decreasing = TRUE),]
total_wholesale <- sum(wholesale_countries_pct$Total_Revenue)
wholesale_countries_pct$Pct_Revenue <- wholesale_countries_pct$Total_Revenue/total_wholesale
## FIX ABOVE!!!!!!!!!!!!!

## Checking for amazon fee among wholesale
wholesale_customers <- income_share$CustomerID
amazon_fee <- retail[retail$Description == "AMAZON FEE",]
sum(wholesale_customers %in% amazon_fee$CustomerID)

## Graph of wholesale by region
Country <- wholesale_countries$Country[1:10]
c15 <- c(
  "dodgerblue2", 
  "#E31A1C", # red
  "green4",
  "#6A3D9A", # purple
  "#FF7F00", # orange
  "black", 
  "gold1",
  "skyblue2",
  "palegreen2",
  "orchid1", 
  "deeppink1", 
  "blue1", 
  "darkturquoise", 
  "green1", 
  "brown"
)

png("Countries_wholesale_plot.png", width=720, height=480)
v <- ggplot(data=wholesale_countries[1:10,], aes(x=Country, y=Total_Revenue, fill=Country)) +
  geom_bar(stat="identity") + theme(panel.background = element_blank()) +
  scale_fill_manual(values=c15) +
  scale_color_manual(values=c15)
v
dev.off()

## Plotting countries excluding UK
png("Countries_wholesale_EU_plot.png", width=720, height=480)
x <- ggplot(data=wholesale_countries[2:10,], aes(x=Country, y=Total_Revenue, fill=Country)) +
  geom_bar(stat="identity") + theme(panel.background = element_blank()) +
  scale_fill_manual(values=c15) +
  scale_color_manual(values=c15)
x
dev.off()
## Graph wholesale revenue vs retail revenue
wholesale$retail <- "Retail"
wholesale$retail[1:nrow(income_share)] <- "Wholesale"

png("retail_vs_wholesale_plot.png", width=720, height=480)
w <- ggplot(data=wholesale, aes(x=retail, y=Total_Revenue, fill=retail)) +
  geom_bar(stat="identity") + theme(panel.background = element_blank()) +
  coord_flip()
w
dev.off()
