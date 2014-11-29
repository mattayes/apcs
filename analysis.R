## Air Pollution Case Study

## Packages
library(lubridate)
## Download data
dir.create("./data")
download.file("http://www.epa.gov/ttn/airs/airsaqs/detaildata/501files/Rd_501_88101_1999.Zip",
              "./data/pm99.zip")
download.file("http://www.epa.gov/ttn/airs/airsaqs/detaildata/501files/RD_501_88101_2012.zip",
              "./data/pm12.zip")
## Unzip data
unzip("./data/pm99.zip", exdir = "./data")
unzip("./data/pm12.zip", exdir = "./data")

## Read in data
pm99 <- read.table("./data/RD_501_88101_1999-0.txt", 
                   comment.char = "#",
                   sep = "|",
                   na.strings = "")
pm12 <- read.table("./data/RD_501_88101_2012-0.txt",
                   comment.char = "#",
                   sep = "|",
                   na.strings = "")

## Add column names
cnames <- readLines("./data/RD_501_88101_1999-0.txt", 1)
cnames <- strsplit(cnames, "|", fixed = TRUE)
names(pm99) <- make.names(cnames[[1]])
names(pm12) <- make.names(cnames[[1]])

## Quick look at PM2.5 data for 1999
sv99 <- pm99$Sample.Value
str(sv99)
summary(sv99)
mean(is.na(sv99))

## Now for 2012
sv12 <- pm12$Sample.Value
str(sv12)
summary(sv12)
mean(is.na(sv12))

## And together
boxplot(sv99, sv12)
boxplot(log10(sv99), log10(sv12))

## What's up with those negative values?
negative <- sv12 < 0
sum(negative, na.rm = TRUE)
mean(negative, na.rm = TRUE)

## Are negative values associated with times of the day/year?
dates12 <- pm12$Date
str(dates12)
dates12 <- as.Date(as.character(dates12), "%Y%m%d")
str(dates12)
hist(dates12, "month")
hist(dates12[negative], "month")