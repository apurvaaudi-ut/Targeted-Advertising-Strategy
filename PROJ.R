library(sqldf)
library(ggplot2)
library(dplyr)
# First, we import data

data = read.csv("data/Online_Retail.csv")

summary(data)




# Add headers and interpret the last column as a date, extract year of purchase

# First format into correct POSIXct 
Invoice_Date_formatted <- strptime(data$InvoiceDate,format="%m/%d/%Y")



data$InvoiceDate = as.Date(Invoice_Date_formatted, "%Y-%m-%d")
data$year_of_purchase = as.numeric(format(data$InvoiceDate, "%Y"))
data$days_since       = as.numeric(difftime(time1 = "2011-12-10",
                                            time2 = data$InvoiceDate,
                                            units = "days"))


# Split data into quarters

q4_date = as.Date("2011-12-10")-90
q3_date = q4_date - 90
q2_date = q3_date - 90
data$quarter_of_purchase = 1
data$quarter_of_purchase[which(data$InvoiceDate>=q4_date)] = 4
data$quarter_of_purchase[which(data$InvoiceDate<q4_date & data$InvoiceDate>=q3_date)] = 3
data$quarter_of_purchase[which(data$InvoiceDate<q3_date & data$InvoiceDate>=q2_date)] = 2


# How many days worth of data do we have?
max(data$InvoiceDate) - min(data$InvoiceDate)


# How many records in just the UK?
nrow(data[data$Country=='United Kingdom',])

# What percentage of records is that?
nrow(data[data$Country=='United Kingdom',])/nrow(data)*100


# Let's only look at records from the UK
data_uk = data[data$Country=='United Kingdom',]
summary(data_uk)



# Let's add a purchase amount column
data_uk$purchase_amount = data_uk$UnitPrice*data_uk$Quantity



# How many records with valid customer IDs?
nrow(data_uk[!is.na(data_uk$CustomerID),])


# What percentage of records is that?
nrow(data_uk[!is.na(data_uk$CustomerID),])/nrow(data_uk)*100


# New dataframe with cust
data_uk_cust = data_uk[!is.na(data_uk$CustomerID),]

# Negative purchase amounts/refunds? Lets check -
nrow(data_uk_cust[data_uk_cust$purchase_amount < 0,])/nrow(data_uk_cust)*100

# Total amount thats refunded?
sum(data_uk_cust[data_uk_cust$purchase_amount < 0,]$purchase_amount)/sum(data_uk_cust[data_uk_cust$purchase_amount >= 0,]$purchase_amount)*100



cust_agg_invoice <- data_uk_cust %>%
  group_by(InvoiceNo) %>%
  summarize(CustomerID = max(CustomerID), purchase_amount = sum(purchase_amount),
            days_since = max(days_since), InvoiceDate = max(InvoiceDate), quarter_of_purchase=max(quarter_of_purchase))





# Compute RFM variables as of today
customers_total = sqldf("SELECT CustomerID,
                               MIN(days_since) AS 'recency',
                               MAX(days_since) AS 'first_purchase',
                               COUNT(*) AS 'frequency',
                               AVG(purchase_amount) AS 'avg_amount',
                               MAX(purchase_amount) AS 'max_amount',
                               SUM(purchase_amount) AS 'revenue'
                        FROM cust_agg_invoice
                        GROUP BY 1")




# How many unique customers does this give us?
nrow(customers_total)
# 3950


# Histogram with density plot of recency for cust
ggplot(customers_total, aes(x=recency)) + 
  geom_histogram(aes(y = ..density..),color="darkblue", fill="lightblue",binwidth = 5)+
  geom_density( alpha=.2, fill="#FF6666")+
  ylab("No. of Customers") +
  xlab("Recency (No. of days since last purchase)")



# Histogram with density plot of frequency for cust
ggplot(customers_total, aes(x=frequency)) + 
  geom_histogram(aes(y = ..density..),color="darkblue", fill="lightblue",binwidth = 5)+
  geom_density( alpha=.2, fill="#FF6666")+
  ylab("No. of Customers") +
  xlab("Recency (No. of days since last purchase)")

# Histogram with density plot of frequency for cust
ggplot(customers_total, aes(x=frequency)) + 
  stat_bin(binwidth=10,color="darkblue", fill="lightblue") +
  stat_bin(binwidth=10, geom="text", aes(label=after_stat(count)), vjust=0) 
  ylab("No. of Customers") +
  xlab("Frequency(No. of purchases")



# Let's look at purchase cycles -
purchase_total = sqldf("SELECT InvoiceDate,
                               MIN(days_since) AS 'recency',
                               MAX(days_since) AS 'first_purchase',
                               COUNT(*) AS 'frequency',
                               SUM(purchase_amount) AS 'purchase_total'
                        FROM cust_agg_invoice
                        GROUP BY 1")




summary(customers_total)

# ----- Deciding segments ------


# Let's choose segments for recency as follows-
# Hot - today to median
# Warm - median to 3rd quantile
# Cold - more than 3rd Quantile

# Let's choose segments for frequency as follows-
# Guest - Lesser than equal to mean = 5
# Recurring - More than mean = 5


# Let's choose segments for monetary value as follows-
# Average Spender - Less than or equal to Mean
# Big spender - More than mean



# Let's create following 6 segments -
# 1) Hot Guest Big (The _somenickname_)- Ideal prospects you want to capture
# 2) Hot Recurring Big (The Golden Apples)- Ideal prospects you want to retain
# 3) Hot Average spender (Bread and butter) - Main consumer base
# 4) Warm Average spender (The masses) - Part of main consumer base who still support
# 5) Warm Big spender (George Clooney from Gravity) - Ideal Customer base thats slipping out of your hands 
# 6) Cold (Jacks) - Little prospect of bringing them back, can try a few hail marys


# Let's first divide up data -

customers_q4 <- customers_total


# ---- More complex 6-segment solution using which ------
customers_q4$segment = "NA"
customers_q4$segment[which(customers_q4$recency > 144)] = "cold"
customers_q4$segment[which(customers_q4$recency <= 144 & customers_q4$recency > 52)] = "warm"
customers_q4$segment[which(customers_q4$recency <= 52)] = "hot"
customers_q4$segment[which(customers_q4$segment == "hot" & customers_q4$avg_amount <= 293)] = "hot average"
customers_q4$segment[which(customers_q4$segment == "hot" & customers_q4$avg_amount > 293)] = "hot big"
customers_q4$segment[which(customers_q4$segment == "hot big" & customers_q4$frequency <= 5)] = "hot guest big"
customers_q4$segment[which(customers_q4$segment == "hot big" & customers_q4$frequency > 5)] = "hot recurring big"


customers_q4$segment[which(customers_q4$segment == "warm" & customers_q4$avg_amount <= 293)] = "warm average"
customers_q4$segment[which(customers_q4$segment == "warm" & customers_q4$avg_amount > 293)] = "warm big"

# ----- Re-order factor in a way that makes sense -----
customers_q4$segment = factor(x = customers_q4$segment, levels = c("cold",
                                                                       "warm big", "warm average",
                                                                       "hot guest big", "hot recurring big", "hot average"))

table(customers_q4$segment)
pie(table(customers_q4$segment), col = rainbow(24))

aggregate(x = customers_q4[, 2:5], by = list(customers_q4$segment), mean)

# --- retrospective segmentation Q3 -----

cust_agg_q3 = cust_agg_invoice[cust_agg_invoice$InvoiceDate < (as.Date("2011-12-10")-90),]
customers_q3 = sqldf("SELECT CustomerID,
                               MIN(days_since) -90 AS 'recency',
                               MAX(days_since) -90 AS 'first_purchase',
                               COUNT(*) AS 'frequency',
                               AVG(purchase_amount) AS 'avg_amount',
                               MAX(purchase_amount) AS 'max_amount',
                               SUM(purchase_amount) AS 'revenue'
                        FROM cust_agg_q3
                        GROUP BY 1")



# ---- Q3 More complex 6-segment solution using which ------
customers_q3$segment = "NA"
customers_q3$segment[which(customers_q3$recency > 144)] = "cold"
customers_q3$segment[which(customers_q3$recency <= 144 & customers_q3$recency > 52)] = "warm"
customers_q3$segment[which(customers_q3$recency <= 52)] = "hot"
customers_q3$segment[which(customers_q3$segment == "hot" & customers_q3$avg_amount <= 293)] = "hot average"
customers_q3$segment[which(customers_q3$segment == "hot" & customers_q3$avg_amount > 293)] = "hot big"
customers_q3$segment[which(customers_q3$segment == "hot big" & customers_q3$frequency <= 5)] = "hot guest big"
customers_q3$segment[which(customers_q3$segment == "hot big" & customers_q3$frequency > 5)] = "hot recurring big"


customers_q3$segment[which(customers_q3$segment == "warm" & customers_q3$avg_amount <= 293)] = "warm average"
customers_q3$segment[which(customers_q3$segment == "warm" & customers_q3$avg_amount > 293)] = "warm big"

# ----- Q3 Re-order factor in a way that makes sense -----
customers_q3$segment = factor(x = customers_q3$segment, levels = c("cold",
                                                                   "warm big", "warm average",
                                                                   "hot guest big", "hot recurring big", "hot average"))

table(customers_q3$segment)
pie(table(customers_q3$segment), col = rainbow(24))

aggregate(x = customers_q3[, 2:5], by = list(customers_q3$segment), mean)





# --- retrospective segmentation Q2 -----

cust_agg_q2 = cust_agg_invoice[cust_agg_invoice$InvoiceDate < (as.Date("2011-12-10")-180),]
customers_q2 = sqldf("SELECT CustomerID,
                               MIN(days_since) -180 AS 'recency',
                               MAX(days_since) -180 AS 'first_purchase',
                               COUNT(*) AS 'frequency',
                               AVG(purchase_amount) AS 'avg_amount',
                               MAX(purchase_amount) AS 'max_amount',
                               SUM(purchase_amount) AS 'revenue'
                        FROM cust_agg_q2
                        GROUP BY 1")

# ---- Q2 More complex 6-segment solution using which ------
customers_q2$segment = "NA"
customers_q2$segment[which(customers_q2$recency > 144)] = "cold"
customers_q2$segment[which(customers_q2$recency <= 144 & customers_q2$recency > 52)] = "warm"
customers_q2$segment[which(customers_q2$recency <= 52)] = "hot"
customers_q2$segment[which(customers_q2$segment == "hot" & customers_q2$avg_amount <= 293)] = "hot average"
customers_q2$segment[which(customers_q2$segment == "hot" & customers_q2$avg_amount > 293)] = "hot big"
customers_q2$segment[which(customers_q2$segment == "hot big" & customers_q2$frequency <= 5)] = "hot guest big"
customers_q2$segment[which(customers_q2$segment == "hot big" & customers_q2$frequency > 5)] = "hot recurring big"


customers_q2$segment[which(customers_q2$segment == "warm" & customers_q2$avg_amount <= 293)] = "warm average"
customers_q2$segment[which(customers_q2$segment == "warm" & customers_q2$avg_amount > 293)] = "warm big"

# ----- Q2 Re-order factor in a way that makes sense -----
customers_q2$segment = factor(x = customers_q2$segment, levels = c("cold",
                                                                   "warm big", "warm average",
                                                                   "hot guest big", "hot recurring big", "hot average"))

table(customers_q2$segment)
pie(table(customers_q2$segment), col = rainbow(24))

aggregate(x = customers_q2[, 2:7], by = list(customers_q2$segment), mean)

# Show average revenue per customer and per segment
r2 = aggregate(x = customers_q2$revenue, by = list(customers_q2$segment), mean)
r3 = aggregate(x = customers_q3$revenue, by = list(customers_q3$segment), mean)
r4 = aggregate(x = customers_q4$revenue, by = list(customers_q4$segment), mean)

# Re-order and display results
r2 = r2[order(r2$x, decreasing = TRUE), ]
r3 = r3[order(r3$x, decreasing = TRUE), ]
r4 = r4[order(r4$x, decreasing = TRUE), ]

barplot(r3$x, names.arg = r3$Group.1)



#----- Transition expectation from Q3 to Q4--------

revenue_q4 = sqldf("SELECT CustomerID, SUM(purchase_amount) AS 'revenue_q4'
                      FROM cust_agg_invoice
                      WHERE quarter_of_purchase = 4
                      GROUP BY 1")
summary(revenue_q4)

# Merge Q3 customers and Q4 revenue
forward = merge(customers_q3, revenue_q4, all.x = TRUE)
forward$revenue_q4[is.na(forward$revenue_q4)] = 0

# Show average revenue per customer and per segment
r3_4 = aggregate(x = forward$revenue_q4, by = list(customers_q3$segment), mean)
print(r3_4)

# Re-order and display results
r3_4 = r3_4[order(r3_4$x, decreasing = TRUE), ]
print(r3_4)

ggplot(r3_4, aes(y=x,x=factor(Group.1, levels = Group.1))) + 
  geom_col(color="darkblue", fill="lightblue")+
  ggtitle("Revenues in Quarter 4 for Quarter 3 Customers")+
  ylab("Average Purchase amount(Sterling Pounds)") +
  xlab("Customer segment")



#----- Transition expectation from Q2 to Q3--------

revenue_q3 = sqldf("SELECT CustomerID, SUM(purchase_amount) AS 'revenue_q3'
                      FROM cust_agg_invoice
                      WHERE quarter_of_purchase = 3
                      GROUP BY 1")
summary(revenue_q3)

# Merge Q2 customers and Q3 revenue
forward = merge(customers_q2, revenue_q3, all.x = TRUE)
forward$revenue_q3[is.na(forward$revenue_q3)] = 0

# Show average revenue per customer and per segment
r2_3 = aggregate(x = forward$revenue_q3, by = list(customers_q2$segment), mean)
print(r2_3)

# Re-order and display results
r2_3 = r2_3[order(r2_3$x, decreasing = TRUE), ]
print(r2_3)


ggplot(r2_3, aes(y=x,x=factor(Group.1, levels = Group.1))) + 
  geom_col(color="darkblue", fill="lightblue")+
  ggtitle("Revenues in Quarter 3 for Quarter 2 Customers")+
  ylab("Average Purchase amount(Sterling Pounds)") +
  xlab("Customer segment")
