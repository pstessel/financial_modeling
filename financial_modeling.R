######################################################################
# Ang, C. S. (2015).
# Analyzing Financial Data and Implementing Financial Models Using R.
# Switzerland: Springer International Publishing.
# Chapter 1: Prices
######################################################################

rm(list=ls(all=TRUE))

setwd("/Volumes/HD2/Users/pstessel/Documents/Git_Repos/financial_modeling")
# setwd("c:/Users/pstessel/Documents/repos/financial_modeling")


# Import data into R
#data.amzn <- read.csv("data/amzn_yahoo.csv", header=TRUE)
data.amzn <- read.csv("data/amzn_yahoo.csv", header=TRUE)
head(data.amzn)

# Create Vector of Symbols
symbols <- c("amzn", "gspc", "ibm", "yhoo")

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

# Keeping contiguous columns
amzn.price.vol <- data.amzn[, 4:5]
amzn.price.vol[c(1:3, nrow(amzn.price.vol)), ]

# Keeping contiguous and non-contiguous columns

amzn.open.close.volume <- data.amzn[, c(1,4:5)]
amzn.open.close.volume[c(1:3, nrow(amzn.open.close.volume)), ]

amzn.open.close.volume <- data.amzn[, c(-2:-3, -6)]
amzn.open.close.volume[c(1:3, nrow(amzn.open.close.volume)), ]

# Subsetting Rows and Columns
data.vwap <- data.amzn[((nrow(data.amzn)-29)):nrow(data.amzn), c(4,5)]
data.vwap[c(1:3, nrow(data.vwap)), ]

## Subsetting Using Dates

# Data is an xts Object
class(data.amzn)
xts.2012 <- subset(data.amzn[, 4],
  index(data.amzn) >= "2012-01-01" &
  index(data.amzn) <= "2012-12-31")
xts.2012[c(1:3, nrow(xts.2012))]

# Data is a data.frame Object
amzn.2012 <- cbind(index(data.amzn),
  data.frame(data.amzn[, 4]))
amzn.2012[c(1:3, nrow(amzn.2012)), ]

names(amzn.2012)[1] <- paste("date")
rownames(amzn.2012) <- seq(1, nrow(amzn.2012),1)
amzn.2012[c(1:3, nrow(amzn.2012)), ]

amzn.2012 <- subset(amzn.2012,
  amzn.2012$date >= "2012-01-01" &
  amzn.2012$date <= "2012-12-31")
amzn.2012[c(1:3, nrow(amzn.2012)), ]

## Converting Daily Prices to Weekly and Monthly Prices

# Converting to Weekly Prices

wk <- data.amzn
data.weekly <- to.weekly(wk)
data.weekly[c(1:3, nrow(data.weekly)), ]

data.amzn[2:6]
sum(data.amzn[2:6, 5])

# Convert to Monthly Prices
mo <- data.amzn
data.monthly <- to.monthly(mo)
data.monthly[c(1:3, nrow(data.monthly)), ]

# Plotting a Candlestick Chart Using Monthly Data
library(quantmod)
ohlc <- data.monthly[-1, -6]
amzn.ohlc <- as.quantmod.OHLC(ohlc,
  col.names = c("Open", "High", "Low", "Close", "Volume"))
class(amzn.ohlc)
amzn.ohlc[c(1:3, nrow(amzn.ohlc)), ]

chartSeries(amzn.ohlc,
  theme = "white.mono",
  name = "AMZN OHLC")

## Comparing Capital Gains of Multiple Securities Over Time
ls()
rm(list=ls())
ls()

# Create character vector of symbols
symbols <- c("amzn", "gspc", "ibm", "yhoo")

# Normalized Price Chart

## Step 1: Import Data for Each of the Four Securities

# AMZN
data.amzn <- read.csv("data/amzn_yahoo.csv", header=TRUE)
date <- as.Date(data.amzn$Date, format = "%Y-%m-%d")
data.amzn <- cbind(date, data.amzn[, -1])
data.amzn <- data.amzn[order(data.amzn$date), ]
data.amzn <- xts(data.amzn[, 2:7], order.by = data.amzn[,1])
names(data.amzn) <-
  paste(c("amzn.Open", "amzn.High", "amzn.Low",
  "amzn.Close", "amzn.Volume", "amzn.Adjusted"))
data.amzn[c(1:3, nrow(data.amzn)), ]

# YHOO
data.yhoo <- read.csv("data/yhoo_yahoo.csv", header=TRUE)
date <- as.Date(data.yhoo$Date, format = "%Y-%m-%d")
data.yhoo <- cbind(date, data.yhoo[, -1])
data.yhoo <- data.yhoo[order(data.yhoo$date), ]
data.yhoo <- xts(data.yhoo[, 2:7], order.by = data.yhoo[,1])
names(data.yhoo) <-
  paste(c("yhoo.Open", "yhoo.High", "yhoo.Low",
  "yhoo.Close", "yhoo.Volume", "yhoo.Adjusted"))
data.yhoo[c(1:3, nrow(data.yhoo)), ]

# IBM
data.ibm <- read.csv("data/ibm_yahoo.csv", header=TRUE)
date <- as.Date(data.ibm$Date, format = "%Y-%m-%d")
data.ibm <- cbind(date, data.ibm[, -1])
data.ibm <- data.ibm[order(data.ibm$date), ]
data.ibm <- xts(data.ibm[, 2:7], order.by = data.ibm[,1])
names(data.ibm) <-
  paste(c("ibm.Open", "ibm.High", "ibm.Low",
  "ibm.Close", "ibm.Volume", "ibm.Adjusted"))
data.ibm[c(1:3, nrow(data.ibm)), ]

# GSPC
data.gspc <- read.csv("data/gspc_yahoo.csv", header=TRUE)
date <- as.Date(data.gspc$Date, format = "%Y-%m-%d")
data.gspc <- cbind(date, data.gspc[, -1])
data.gspc <- data.gspc[order(data.gspc$date), ]
data.gspc <- xts(data.gspc[, 2:7], order.by = data.gspc[,1])
names(data.gspc) <-
  paste(c("gspc.Open", "gspc.High", "gspc.Low",
  "gspc.Close", "gspc.Volume", "gspc.Adjusted"))
data.gspc[c(1:3, nrow(data.gspc)), ]

## Step 2: Combine Data into One Data Object

close.prices <- data.amzn$amzn.Close
close.prices <- cbind(close.prices, data.gspc$gspc.Close,
                      data.yhoo$yhoo.Close, data.ibm$ibm.Close)
close.prices[c(1:3, nrow(close.prices)),]

## Step 3: Convert Data into a data.frame

multi.df <- cbind(index(close.prices),
                  data.frame(close.prices))
names(multi.df) <- paste(c("date", "amzn", "gspc", "yhoo", "ibm"))
rownames(multi.df) <- seq(1, nrow(multi.df),1)
multi.df[c(1:3, nrow(multi.df)),]

## Step 4: Calculate Normalized Values for Each Security

# Function to Write Code
normalize_values <- function(x){
  paste0("multi.df$",(x),".idx <- multi.df$",(x),"/multi.df$",(x),"[1]")
}

# Execute Code Written by Function
eval(parse(text=normalize_values(symbols)))

options(digits = 5)
multi.df[c(1:3, nrow(multi.df)),]
options(digits=7)

## Step 5: Plot the Capital Appreciation of Each Security

y.range <- range(multi.df[, 6:9])
y.range

plot(x = multi.df$date,
     y = multi.df$gspc.idx,
     type = "l",
     xlab = "Date",
     ylim = y.range,
     ylab = "Value of Investment ($)",
     col = "black",
     lty = 1,
     lwd = 2,
     main = "Value of $1 Investment in
     AMZN, IBM, YHOO and the S&P 500 Index
     December 31, 2010 - December 31, 2013")

lines(x = multi.df$date,
     y = multi.df$amzn.idx,
     col = "black",
     lty = 2,
     lwd = 1)

lines(x = multi.df$date,
      y = multi.df$ibm.idx,
      col = "gray",
      lty = 2,
      lwd = 1)

lines(x = multi.df$date,
      y = multi.df$yhoo.idx,
      col = "gray",
      lty = 1,
      lwd = 1)

abline(h=1, lty = 1, col = "black")

legend("topleft",
       c("AMZN", "IBM", "YAHOO", "S&P 500 Index"),
       col = c("black", "gray", "gray", "black"),
       lty = c(2, 2, 1, 1),
       lwd = c(1, 1, 1, 2))

## Alternative Presentation of Normalized Price Chart

# Step 1: Setup Chart Layout
par(oma = c(0, 0, 3, 0))

# Step 2: Let R Know We Will Be Plotting 4 Charts with 2 Charts in Each Column
par(mfrow = c(2, 2))

# Step 3: Create the 4 Plots
plot(x = multi.df$date,
     xlab = "",
     y = multi.df$yhoo.idx
     ylim = y.range,
     ylab = "",
     type = "l",
     col = "gray",
     main = "Amazon Stock")
lines(x = multi.df$date, y = multi.df$gspc.idx, col = "gray")
lines(x = multi.df$date, y = multi.df$ibm.idx, col = "gray")
lines(x = multi.df$date, y = multi.df$amzn.idx, col = "black", lwd = 2)
abline(h = 1)

