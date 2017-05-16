# process_asos.R
# Bryan Holman // v0.1 // 20170516

# Extract verification from ASOS observations, then place the appropriate
# verification class into the final dataset. In short, we need to determine 
# what part of the day thunderstorms/rain were first observed at KMLB for each 
# of the summertime days in df.final

# libraries ---------------------------------------------------------------
library(lubridate) # improved date handling
library(riem) # get kmlb asos data

# functions ---------------------------------------------------------------
getHourFirstTSRA <- function(date, data) {
    df.date <- data[date(data$validlcl) == date,]
    
    # if no thunderstorms on this day, return NA!
    if (length(df.date$validlcl) == 0) return(NA)
    
    # if there are thunderstorms, return the earliest occurrence of them!
    return(min(df.date$hour, na.rm = TRUE))
}

# data --------------------------------------------------------------------
load('data/kmlb_mos_scaled.RData')

# grab asos data
df.asos <- riem_measures(station = 'MLB', date_start = format(df.final$date[1]),
                         date_end = format(tail(df.final$date, n = 1) + days(2)))

# all we need is the valid and presentwx columns
df.asos <- df.asos[c('valid', 'presentwx')]

# convert valid to eastern time
df.asos$validlcl <- format(df.asos$valid, tz="America/New_York", usetz = TRUE)
df.asos$validlcl <- as.POSIXct(df.asos$validlcl, tz="America/New_York")

# only save instances where either ts or ra occur
df.ts <- df.asos[grep('TS', df.asos$presentwx),]

# only keep obs during the summer
df.ts <- df.ts[month(df.ts$validlcl) %in% 6:9,]

# get the hour for each ts occurrence
df.ts$hour <- hour(df.ts$validlcl)

# get timing of first thunderstorms/rain for given day
df.scaled$TShr <- sapply(df.final$date, getHourFirstTSRA, data = df.ts)

# now determine the TS category for each day, 0 = no thunderstorms, 1 = before
# 6am, 2 = between 6 and noon, 3 = between noon and 18, 4 = after 18
df.scaled$TScat[is.na(df.scaled$TShr)] <- 0
df.scaled$TScat[df.scaled$TShr < 6] <- 1
df.scaled$TScat[df.scaled$TShr >= 6 & df.scaled$TShr < 12] <- 2
df.scaled$TScat[df.scaled$TShr >= 12 & df.scaled$TShr < 18] <- 3
df.scaled$TScat[df.scaled$TShr >= 18] <- 4

# mace TScat a factor
df.scaled$TScat <- factor(df.scaled$TScat)

# save df.scaled to perform classification!
save(df.scaled, file = 'data/kmlb_mos_scaled_verification.RData')
