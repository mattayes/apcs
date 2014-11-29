## Air Pollution Case Study

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

## Quick look at PM2.5 data
sv <- pm99$Sample.Value
str(sv)
summary(sv)
mean(is.na(sv))
