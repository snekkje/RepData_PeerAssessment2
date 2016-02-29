library(dplyr)
library(ggplot2)

#temp <- tempfile()
#download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", 
                          #temp, mode = "wb")
#StormData <- read.csv("here.csv.bz2")
#unlink(temp)


StormDataHarm <- select(StormData, EVTYPE, FATALITIES, INJURIES)
StormDataHarm <- mutate(StormDataHarm, HARMED = FATALITIES + INJURIES)

aggHarm <- aggregate(StormDataHarm$HARMED, list(Event = StormDataHarm$EVTYPE),
                           sum, na.rm = TRUE)

aggHarm <- arrange(aggHarm, desc(x))
ggplot(data = aggHarm[1:5,], aes(Event, x)) + geom_bar(stat="identity")



StormDataExpense <- select(StormData, EVTYPE, PROPDMG, PROPDMGEXP, CROPDMG, CROPDMGEXP)
StormDataExpense <- mutate(StormDataExpense, EXPENSE = 
                             {ifelse(PROPDMGEXP == "K" | PROPDMGEXP == "k", PROPDMG * 1000, 
                              ifelse(PROPDMGEXP == "M" | PROPDMGEXP == "m", PROPDMG * 1000000,
                              ifelse(PROPDMGEXP == "B" | PROPDMGEXP == "b", PROPDMG * 1000000000,
                                NA)))})

aggExpense <- aggregate(StormDataExpense$EXPENSE, list(Event = StormDataExpense$EVTYPE),
                              sum, na.rm = TRUE)
aggExpense <- arrange(aggExpense, desc(x))
ggplot(data = aggExpense[1:5,], aes(Event, x)) + geom_bar(stat="identity") + 
                              scale_x_discrete(labels = abbreviate)


head(aggExpense,30)
str(aggExpense)
