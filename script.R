## Air Pollution Case Study

## Packages
library(tidyr); library(dplyr); library(lubridate); library(ggplot2)
## Download data
if(!file.exists("./data")){
	dir.create("./data")
	download.file("http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/daily_88101_2012.zip",
              "./data/pm99.zip")
	download.file("http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/daily_88101_1999.zip",
              "./data/pm12.zip")
	unzip("./data/pm99.zip", exdir = "./data")
	unzip("./data/pm12.zip", exdir = "./data")
}

## Read in data
pm99 <- tbl_df(read.csv("./data/daily_88101_1999.csv", 
                   comment.char = "#",
                   na.strings = ""))
pm12 <- tbl_df(read.csv("./data/daily_88101_2012.csv",
                   comment.char = "#",
                   na.strings = ""))

## Quick look at PM2.5 data for 1999
sv99 <- pm99$Arithmetic.Mean
str(sv99)
summary(sv99)

## Now for 2012
sv12 <- pm12$Arithmetic.Mean
str(sv12)
summary(sv12)

## And together
boxplot(sv99, sv12)
boxplot(log10(sv99), log10(sv12))

## What's up with those negative values?
negative <- sv12 < 0
sum(negative)
mean(negative)

## Are negative values associated with time of year?
dates <- pm12$Date.Local
str(dates)
dates <- as.Date(as.character(dates), "%Y%m%d")
str(dates)
hist(dates, "month")
hist(dates[negative], "month")

## Let's follow one monitor over time

## Find monitors for Oregon that are available in both years
site99 <- pm99 %>%
		filter(State.Name == "Oregon") %>%
		select(County.Code, Site.Num) %>%
		distinct() %>%
		mutate(County.Site = paste(County.Code, Site.Num, sep = "."))
site12 <- pm12 %>%
		filter(State.Name == "Oregon") %>%
		select(County.Code, Site.Num) %>%
		distinct() %>%
		mutate(County.Site = paste(County.Code, Site.Num, sep = "."))
sites <- intersect(site99$County.Site, site12$County.Site)

## How many observations are available at each monitor?
or99 <- pm99 %>%
		mutate(County.Site = paste(County.Code, Site.Num, sep = ".")) %>%
		filter(State.Name == "Oregon", County.Site %in% sites) %>%
		group_by(County.Site) %>%
		summarize(count = n())
or12 <- pm12 %>%
		mutate(County.Site = paste(County.Code, Site.Num, sep = ".")) %>%
		filter(State.Name == "Oregon", County.Site %in% sites) %>%
		group_by(County.Site) %>%
		summarize(count = n())
inner_join(or99, or12, by = County.Site)

## Choose county 29 and site number 133; convert dates
sub99 <- pm99 %>%
		filter(County.Code == 29, Site.Num == 133) %>%
		mutate(Date.Local = as.Date(as.character(Date.Local), "%Y-%m-%d"))
sub12 <- pm12 %>%
		filter(County.Code == 29, Site.Num == 133) %>%
		mutate(Date.Local = as.Date(as.character(Date.Local), "%Y-%m-%d"))
dim(sub99)
dim(sub12)

## Plot data for both years in same panel
rng <- range(sub99$Arithmetic.Mean, sub12$Arithmetic.Mean)
a <- ggplot(sub99, aes(Date.Local, Arithmetic.Mean)) +
		geom_point() +
		stat_hline(yintercept = median(sub99$Arithmetic.Mean)) +
		coord_cartesian(ylim = rng)
b <- ggplot(sub12, aes(Date.Local, Arithmetic.Mean)) +
		geom_point() +
		stat_hline(yintercept = median(sub12$Arithmetic.Mean)) +
		coord_cartesian(ylim = rng)
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
## Helper function
vplayout <- function(x, y)
        viewport(layout.pos.row = x, layout.pos.col = y)
print(a, vp = vplayout(1, 1))
print(b, vp = vplayout(1, 2))

## Show state-wide means and make a plot showing trend
state99 <- pm99 %>%
		group_by(State.Name) %>%
		summarize(mean99 = mean(Arithmetic.Mean))
state12 <- pm12 %>%
		group_by(State.Name) %>%
		summarize(mean12 = mean(Arithmetic.Mean))
states <- inner_join(state99, state12)
with(states, {
		plot(rep(1999, 53), mean99, ylab = expression("PM"[2.5]*" (tons)"), 
			xlim = c(1998, 2013), main = "State Variation Over Time")
		points(rep(2012, 53), mean12)
		segments(rep(1999, 53), mean99, rep(2012, 53), mean12)
	})