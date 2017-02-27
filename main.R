#libraries
library(dplyr)
library(ggplot2)

#Read and observe data
# accident <- read.csv("data/original/accident.csv")
# str(accident)

#columns to keep
dat <- select(accident, ST_CASE, COUNTY, DAY, MONTH, YEAR, DAY_WEEK,
              ROUTE, HOUR, MINUTE, LATITUDE, LONGITUD, WEATHER, FATALS, DRUNK_DR)

str(dat)


#Get rid of uknown or missing values
#Ref https://crashstats.nhtsa.dot.gov/Api/Public/ViewPublication/812315

dat <- subset(dat, !(COUNTY == 000 | COUNTY == 997 | COUNTY == 998 | COUNTY == 999))
dat <- subset(dat, !(DAY == "--"))
dat <- subset(dat, !(MONTH == "--"))
dat <- subset(dat, !(YEAR == "--" | YEAR == 99))
dat <- subset(dat, !(DAY_WEEK == "--"))
dat <- subset(dat, !(HOUR == "--" | HOUR == 99))
dat <- subset(dat, !(MINUTE == "--" | MINUTE == 99))
dat <- subset(dat, !(LATITUDE == 77.7777 | LATITUDE == 88.8888 | LATITUDE == 99.9999))
dat <- subset(dat, !(LONGITUD == 77.7777 | LONGITUD == 88.8888 | LONGITUD == 99.9999))
dat <- subset(dat, !(WEATHER == 98 | WEATHER == 99))

str(dat)

#get a full date column
dat <- mutate(dat, DATE = paste(YEAR, MONTH, DAY, sep = '/')) %>%
  select(-DAY, -MONTH, -YEAR) 

dat$DATE <- as.Date(dat$DATE)

str(dat)

# some basic stats
summary(dat$FATALS)

#summarizing by total fatalities and total drunk incidents, and its ratio
totals <- summarise(dat,
                    totFat = sum(FATALS),
                    totDrunk = sum(DRUNK_DR),
                    ratio = totDrunk/totFat)
totals

#plotting accidents by date (must group since any day has multiple accidents)

group <- group_by(dat, DATE)
accidentsByDate <- summarise(group, FATALS = n())
head(accidentsByDate)

plot1 <- ggplot(accidentsByDate, aes(DATE, FATALS)) +
  geom_point(color = 'firebrick') +
  labs(x = "Date", y = "Number of Fatalities") +
  geom_smooth()
plot1


plot2 <- ggplot(dat, aes(x = DATE, fill = ROUTE)) +
  geom_histogram(binwidth = 1) + facet_grid(ROUTE~.)
plot2


# write.csv(dat, file = "fars.csv", row.names = F)