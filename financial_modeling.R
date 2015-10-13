######################################################################
# Ang, C. S. (2015).
# Analyzing Financial Data and Implementing Financial Models Using R.
# Switzerland: Springer International Publishing.
# Chapter 1: Prices
######################################################################

rm(list=ls(all=TRUE))

# setwd("/Volumes/HD2/Users/pstessel/Documents/Git_Repos/financial_modeling")
setwd("c:/Users/pstessel/Documents/repos/financial_modeling")


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
     y = multi.df$yhoo.idx,
     ylim = y.range,
     ylab = "",
     type = "l",
     col = "gray",
     main = "Amazon Stock")
lines(x = multi.df$date, y = multi.df$gspc.idx, col = "gray")
lines(x = multi.df$date, y = multi.df$ibm.idx, col = "gray")
lines(x = multi.df$date, y = multi.df$amzn.idx, col = "black", lwd = 2)
abline(h = 1)

plot(x = multi.df$date,
     xlab = "",
     y = multi.df$yhoo.idx,
     ylim = y.range,
     ylab = "",
     type = "l",
     col = "gray",
     main = "IBM Stock")
lines(x = multi.df$date, y = multi.df$amzn.idx, col = "gray")
lines(x = multi.df$date, y = multi.df$gspc.idx, col = "gray")
lines(x = multi.df$date, y = multi.df$ibm.idx, col = "black", lwd = 2)
abline(h = 1)

plot(x = multi.df$date,
     xlab = "",
     y = multi.df$gspc.idx,
     ylim = y.range,
     ylab = "",
     type = "l",
     col = "gray",
     main = "Yahoo Stock")
lines(x = multi.df$date, y = multi.df$amzn.idx, col = "gray")
lines(x = multi.df$date, y = multi.df$ibm.idx, col = "gray")
lines(x = multi.df$date, y = multi.df$yhoo.idx, col = "black", lwd = 2)
abline(h = 1)

plot(x = multi.df$date,
     xlab = "",
     y = multi.df$yhoo.idx,
     ylim = y.range,
     ylab = "",
     type = "l",
     col = "gray",
     main = "S&P 500 Index")
lines(x = multi.df$date, y = multi.df$amzn.idx, col = "gray")
lines(x = multi.df$date, y = multi.df$ibm.idx, col = "gray")
lines(x = multi.df$date, y = multi.df$gspc.idx, col = "black", lwd = 2)
abline(h = 1)

# Step 4: Create a Global Title for the Charts
title1 = "Value of $1 Invested in Amazon, IBM, Yahoo and the Market"
title2 = "December 31, 2010 - December 31, 2013"
title(main = paste(title1, "\n", title2), outer = T)

### Technical Analysis Examples

## Trend: Simple Moving Average (SMA) Crossover

# Step 1: Obtain Closing Prices for Amazon.com Stock
amzn.sma <- data.amzn[, 4]
amzn.sma[c(1:3, nrow(amzn.sma)),]

# Step 2: Calculate the Rolling 50-Day and 200-Day Average Price
amzn.sma$sma50 <- rollmeanr(amzn.sma$amzn.Close, k = 50)
amzn.sma$sma200 <- rollmeanr(amzn.sma$amzn.Close, k = 200)
amzn.sma[c(1:3, nrow(amzn.sma)),]
amzn.sma[48:52, ]

# Step 3: Subset to Only Show 2012 and 2013 Data
amzn.sma[198:201, ]
amzn.sma2012 <- subset(amzn.sma,
                       index(amzn.sma) >= "2012-01-01")
amzn.sma2012[c(1:3, nrow(amzn.sma2012)),]

# Step 4: Plot the SMA
y.range <- range(amzn.sma2012, na.rm = TRUE)
y.range
par(mfrow = c(1, 1))
plot(x = index(amzn.sma2012),
     xlab = "Date",
     y = amzn.sma2012$amzn.Close,
     ylim = y.range,
     ylab = "Price ($)",
     type = "l",
     main = "Amazon - Simple Moving Average
     January 1, 2012 - December 31, 2013")
lines(x = index(amzn.sma2012), y = amzn.sma2012$sma50)
lines(x = index(amzn.sma2012), y = amzn.sma2012$sma200, lty = 2)
legend("topleft",
       c("Amazon Price", "50-Day Moving Average", "200-Day Moving Average"),
       lty = c(1, 1, 2))

## Volatility: Bollinger Bands

# Step 1: Obtain Closing Prices for Amazon.com Stock
amzn.bb <- data.amzn[, 4]
amzn.bb[c(1:3, nrow(amzn.bb)), ]

# Step 2: Calculate Rolling 20-Day Mean and Standard Deviation
amzn.bb$avg <- rollmeanr(amzn.bb$amzn.Close, k = 20)
amzn.bb$sd <- rollapply(amzn.bb$amzn.Close, width = 20, FUN = sd, fill = NA)
amzn.bb[c(1:3, nrow(amzn.bb)), ]
amzn.bb[18:22]

# Step 3: Subset to Only Show 2013 Data
amzn.bb2013 <- subset(amzn.bb,
                      index(amzn.bb) >= "2013-01-01")
amzn.bb2013[c(1:3, nrow(amzn.bb2013)), ]

# Step 4: Calculate the Bollinger Bands
amzn.bb2013$sd2up <- amzn.bb2013$avg + 2 * amzn.bb2013$sd
amzn.bb2013$sd2down <- amzn.bb2013$avg - 2 * amzn.bb2013$sd

#Step 5: Plot the Bollinger Bands
y.range <- range(amzn.bb2013[, -3], na.rm = TRUE)
y.range
plot(x = index(amzn.bb2013),
     xlab = "Date",
     y = amzn.bb2013$amzn.Close,
     ylim=y.range,
     ylab = "Price ($)",
     type = "l",
     lwd = 3,
     main = "Amazon - Bollinger Bands (20 days, 2 deviations)
     January 1, 2013 - December 31, 2013")
lines(x = index(amzn.bb2013), y = amzn.bb2013$avg, lty = 2)
lines(x = index(amzn.bb2013), y = amzn.bb2013$sd2up, col = "gray40")
lines(x = index(amzn.bb2013), y = amzn.bb2013$sd2down, col = "gray40")
legend("topleft",
       c("Amazon Price", "20-Day Moving Average", "Upper Band", "Lower Band"),
       lty = c(1, 2, 1, 1),
       lwd = c(3, 1, 1, 1),
       col = c("black", "black", "gray40", "gray40"))


### 1.6.3 Momentum: Relative Strength Index

## Step 1: Obtain Closing Prices for Amazon.com Stock
amzn.rsi <- data.amzn[,4]
amzn.rsi

# Calculate the diffirences in Amazon.com's price using the diff command. The
# difference between the closing price today and yesterday's closing price is
# reported in the column labeled delta.
amzn.rsi$delta <- diff(amzn.rsi$amzn.Close)
amzn.rsi[c(1:3, nrow(amzn.rsi)), ]

## Step 2: Create Dummy Variables to Indicate Whether Price Went Up or Down
amzn.rsi$up <- ifelse(amzn.rsi$delta > 0, 1, 0)
amzn.rsi$down <- ifelse(amzn.rsi$delta < 0, 1, 0)
amzn.rsi[c(1:3, nrow(amzn.rsi)), ]

## Step 3: Calculate Prices for Up Days and Prices for Down Days

# To construct a series of prices on up days, we multiply amzn.Close with up.
# If it is an up day, up will equal one, so up.val will equal the Amazon.com
# closing price.

amzn.rsi$up.val <- amzn.rsi$delta*amzn.rsi$up
amzn.rsi$down.val <- amzn.rsi$delta*amzn.rsi$down
amzn.rsi <- amzn.rsi[-1,]

# Delete the 12/31/2010 observation -- not needed
amzn.rsi[c(1:15, nrow(amzn.rsi)), ]


## Step 4: Calculate Initial Up and Down 14-Day Averages
amzn.rsi$up.first.avg <- rollapply(amzn.rsi$up.val,
                                width=14,
                                FUN=mean,
                                fill=NA,
                                na.rm=TRUE)
amzn.rsi$down.first.avg <- rollapply(amzn.rsi$down.val,
                                width=14,
                                FUN=mean,
                                fill=NA,
                                na.rm=TRUE)
amzn.rsi[c(1:3, nrow(amzn.rsi)), ]

## Step 5: Calculate the Wilder Exponential Moving Average to Calculate Final Up
## and Down 14-Day Averages.

# This average assumes that the initial average the day before would have a
# wieght of 13 out of 14 days and the current average will have a weight of one
# out of 14 days.

up.val <- as.numeric(amzn.rsi$up.val)
down.val <- as.numeric(amzn.rsi$down.val)

amzn.rsi$down.avg <- amzn.rsi$.first.avg
for (i in 15:nrow(amzn.rsi)){
  amzn.rsi$up.avg[i] <- 
    ((amzn.rsi$up.avg[i-1]*13+up.val[i])/14)
}

amzn.rsi$down.avg <- amzn.rsi$down.first.avg
for (i in 15:nrow(amzn.rsi)){
  amzn.rsi$down.avg[i] <- 
    ((amzn.rsi$down.avg[i-1]*13+down.val[i])/14)
}

amzn.rsi[c(1:20, nrow(amzn.rsi)),]

## Step 6: Calculate the RSI
amzn.rsi$rs <- amzn.rsi$up.avg/amzn.rsi$down.avg
amzn.rsi$rsi <- 100 - (100/(1 + amzn.rsi$rs))
amzn.rsi[c(14:20, nrow(amzn.rsi)),]
