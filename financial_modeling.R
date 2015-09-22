rm(list=ls(all=TRUE))

setwd("/Volumes/HD2/Users/pstessel/Documents/Git_Repos/financial_modeling")


# Import data into R
data.amzn <- read.csv("data/amzn_yahoo.csv", header=TRUE)
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