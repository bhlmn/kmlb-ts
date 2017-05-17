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
    df.date <- data[date(data$validm10) == date,]
    
    # if no thunderstorms on this day, return NA!
    if (length(df.date$validm10) == 0) return(NA)
    
    # if there are thunderstorms, return the earliest occurrence of them!
    return(min(df.date$hourm10, na.rm = TRUE))
}

# data --------------------------------------------------------------------
load('data/kmlb_mos_scaled.RData')

# grab asos data
# df.asos <- riem_measures(station = 'MLB', date_start = format(df.final$date[1]),
#                          date_end = format(tail(df.final$date, n = 1) + days(2)))
# 
# # all we need is the valid and presentwx columns
# df.asos <- df.asos[c('valid', 'presentwx')]

# looks like the riem KMLB mesonet data does not have 5 min data before 2014 ...
# unfortunately this dramtically reduces the amount of thunderstorm obs. So I
# downloaded all the KMLB 5 minute data (200506 onwards) from noaa's ftp site.
# The .dat files are gross ... let's dig in!

df.asos <- read.delim('data/ftp/kmlb2005_2016.dat', sep = ' ', header = FALSE, 
                      stringsAsFactors = FALSE)
df.asos <- df.asos[df.asos[[1]] == '12838KMLB',]
df.asos$valid.est <- as.POSIXct(substr(df.asos$V2, 4, 15), format = '%Y%m%d%H%M', tz = 'EST')
df.asos$presentwx <- paste(df.asos$V8, df.asos$V9, df.asos$V10, df.asos$V11, 
                           df.asos$V12, df.asos$V13, df.asos$V14)
df.asos <- df.asos[grep('TS', df.asos$presentwx), c('valid.est', 'presentwx')]

# convert valid to eastern time
df.asos$validlcl <- format(df.asos$valid.est, tz="America/New_York", usetz = TRUE)
df.asos$validlcl <- as.POSIXct(df.asos$validlcl, tz="America/New_York")
df.asos$validm10 <- format(df.asos$valid.est, tz="Etc/GMT+10", usetz = TRUE)
df.asos$validm10 <- as.POSIXct(df.asos$validm10, tz="Etc/GMT+10")

# this asos dataset only covers from 2005 onwards
df.final <- df.final[year(df.final$date) %in% 2005:2016,]
df.scaled <- df.scaled[year(df.scaled$date) %in% 2005:2016,]

# only save instances where either ts or ra occur
df.ts <- df.asos[grep('TS', df.asos$presentwx),]

# only keep obs during the summer
df.ts <- df.ts[month(df.ts$validlcl) %in% 6:9,]

# get the hour for each ts occurrence
df.ts$hourlcl <- hour(df.ts$validlcl)
df.ts$hourm10 <- hour(df.ts$validm10) + 6

# get timing of first thunderstorms/rain for given day
df.scaled$TShr2 <- sapply(df.final$date, getHourFirstTSRA, data = df.ts)

# now determine the TS category for each day, 0 = no thunderstorms, 
# 1 = morning (before noon), 2 = afternoon (btwn noon and 6), 
# 3 = evening (btwn 6 and midnight), 4 = night (after midnight)
df.scaled$TScat[is.na(df.scaled$TShr2)] <- 0
df.scaled$TScat[df.scaled$TShr2 < 12] <- 1
df.scaled$TScat[df.scaled$TShr2 >= 12 & df.scaled$TShr2 < 18] <- 2
df.scaled$TScat[df.scaled$TShr2 >= 18 & df.scaled$TShr2 < 24] <- 3
df.scaled$TScat[df.scaled$TShr2 >= 24] <- 4

# mace TScat a factor
df.scaled$TScat <- factor(df.scaled$TScat)

# make another factor, TSbin, a simple binary classifier if there were 
# thunderstorms on that day
df.scaled$TSbin <- ifelse(is.na(df.scaled$TShr2), 0, 1)
df.scaled$TSbin <- factor(df.scaled$TSbin)

# save df.scaled to perform classification!
save(df.final, df.scaled, file = 'data/kmlb_mos_scaled_verification.RData')
