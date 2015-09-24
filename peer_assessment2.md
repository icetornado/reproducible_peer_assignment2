# Reproducible Research - Peer Assessment 2
Trieu Tran  
September 23, 2015  
## Synopsis

## Loading and processing raw data

```r
library(dplyr)
library(ggplot2)

## checking existence of a folder named "figure", if not then creating one to store plot figures
figureDir <- 'figure'
if (!file.exists(figureDir)){
    dir.create(figureDir)
} 
```

```r
# reading csv file
rawData <- read.csv(file.path("data", "repdata-data-StormData.csv"), na.strings = "", stringsAsFactors = FALSE)
eventTypes <- read.csv(file.path("data", "event_types.csv"), header = FALSE)
names(eventTypes) <- c("name", "designator")
eventTypes$name <- toupper(eventTypes$name)

## cleaning up EVTYPE 
data <- rawData
### first trim blank spaces
data$EVTYPE <- trimws(data$EVTYPE)

### upper case 
data$EVTYPE <- toupper(data$EVTYPE)

### kill "SUMMARY OF xxx"
data$EVTYPE <- gsub("^SUMMARY(\\s|\\w|.|-|_)*", "ERROR", data$EVTYPE)

### mapping exact event type in eventTypes df
for(i in 1: length(eventTypes$name)) {
        eventName <- eventTypes[i, "name"]
        #print(eventName)
        pattern <- paste(eventName, "(\\s|\\w|.|-|_)*", sep = "")
        #print(pattern)
        data$EVTYPE <- gsub(pattern, eventName, data$EVTYPE)
}

## read mapping table
eventMap <- read.csv(file.path("data", "event_mapping.csv"), header = FALSE)
names(eventMap) <- c("raw", "clean")

## replace data$EVTYPE  values with correct ones from eventMap
doMapping <- function(x, map) {
        found <- which(map$raw == x["EVTYPE"])
        if(found > 0) {
                map[found, "clean"]
        }
        else {
                "ERROR"
        }
}
#test <- data[1,10,]
data$EVTYPE <- apply(data, 1, FUN="doMapping", map=eventMap)

healthDamageData <- select(data, EVTYPE, FATALITIES, INJURIES)
```

```r
healthDamageData  <- healthDamageData %>%
        group_by(EVTYPE) %>%
        summarise(ttlFatal = sum(FATALITIES), ttlInjuries = sum(INJURIES)) %>%
        filter(ttlFatal > 0 | ttlInjuries > 0)
```
## Results

