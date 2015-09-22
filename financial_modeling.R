rm(list=ls(all=TRUE))

setwd("c:\\Users\\pstessel\\Documents\\repos\\financial_modeling")


# Import data into R
data.amzn <- read.csv("data\\amzn_yahoo.csv", header=TRUE)
head(data.amzn)

# Convert the date variable from a factor to a date
date <- as.Date(data.amzn$Date, format="%Y-%m-%d")
head(date)
tail(date)
class(date)

# Combine date and data.amzn
data.amzn <- cbind(date, data.amzn[,-1])
head(data.amzn)

data.amzn <- data.amzn[order(data.amzn$date), ]
head(data.amzn)

# Convert from data.frame object to xts object for time series analysis (e.g.,
# creating lag variables)
library(xts)
data.amzn <- xts(data.amzn[, 2:7], order.by = data.amzn[, 1])
class(data.amzn)

# Rename variables
names(data.amzn)
names(data.amzn) <- paste(c("amzn.open", "amzn.high", "amzn.low",
                            "amzn.close", "amzn.volume", "amzn.adjusted"))
head(data.amzn)

## Checking the data

# Plotting the data
plot(data.amzn$amzn.close)

data.missing <- data.amzn[-400:-500]
plot(data.missing$amzn.close)

# Checking the dimensions
dim(data.amzn)

# Outputting summary statistics
summary(data.amzn)

# Remember to check the ticker symbol

## Basic data manipulation techniques

# Keeping and deleting one row
amzn.only.first <- data.amzn[1, ]
amzn.only.first


amzn.del.first <- data.amzn[-1, ]
amzn.del.first

# Keeping first and last rows
data.amzn[c(1, nrow(data.amzn)), ]

# Keeping contiguous rows
amzn.first.week <- data.amzn[2:6, ]
amzn.first.week

# Keeping and deleting one column
names(data.amzn)
amzn.only.price <- data.amzn[, 4]
amzn.only.price[c(1:3, nrow(amzn.only.price)), ]

amzn.only.price2 <- data.amzn$amzn.close
amzn.only.price2[c(1:3, nrow(amzn.only.price2)), ]

amzn.del.adjprice <- data.amzn[, -6]
amzn.del.adjprice[c(1:3, nrow(amzn.del.adjprice)), ]

# Keeping non-contiguous columns
amzn.openclose <- data.amzn[, c(1,4)]
amzn.openclose[c(1:3, nrow(amzn.openclose)), ]