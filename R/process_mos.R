# process_mos.R
# Bryan Holman // v0.1 // 20170513

# Extract desired information from the raw KMLB MOS dataset, and transform it
# into desired form to load into classification models.

# libraries ---------------------------------------------------------------
library(lubridate) # improved date handling

# functions ---------------------------------------------------------------


# data --------------------------------------------------------------------
df.mos <- read.csv('data/kmlb_mos_2001_2016.csv', row.names = NULL, 
                   stringsAsFactors = FALSE)


# data processing ---------------------------------------------------------

# remove the row.names column
colnames(df.mos) <- c(colnames(df.mos)[-1], 'rmv')
df.mos <- df.mos[,!names(df.mos) %in% c('rmv', 'station', 'snw', 'cig', 'vis', 
                                        'obv', 'poz', 'pos', 'typ', 'cld', 
                                        'q06', 'q12', 't12')]

# for now, only keep GFS model data, not AVN model
df.mos <- df.mos[df.mos$model == 'GFS',]

# change runtime and ftime to POSIXct objects
df.mos$runtime <- as.POSIXct(gsub('\\+\\d\\d', '', df.mos$runtime), tz = 'UTC')
df.mos$ftime <- as.POSIXct(gsub('\\+\\d\\d', '', df.mos$ftime), tz = 'UTC')

# convert runtime and forecast time to local time
df.mos$runtimelcl <- format(df.mos$runtime, tz="America/New_York",usetz=TRUE)
df.mos$ftimelcl <- format(df.mos$ftime, tz="America/New_York",usetz=TRUE)

# remove nonsummer forecasts
df.mos <- df.mos[month(df.mos$ftimelcl) %in% c(5, 6, 7, 8, 9),]

# only keep next day forecasts
df.mos <- df.mos[date(df.mos$ftimelcl) <= date(df.mos$runtimelcl) + days(1),]

# only keep 18 UTC runs
df.mos <- df.mos[hour(df.mos$runtime) == 18,]

# make t06 numeric, save severe tstorm probabilities as svr06
df.mos$svr06 <- sapply(df.mos$t06, FUN = function(x) 
    as.numeric(unlist(strsplit(x, '/'))[2]))
df.mos$t06 <- sapply(df.mos$t06, FUN = function(x) 
    as.numeric(unlist(strsplit(x, '/'))[1]))

# save this edited df.mos dataset
save(df.mos, file = 'data/kmlb_mos_processed.RData')
# load('data/kmlb_mos_processed.RData')

# feature engineering -----------------------------------------------------

# Now it is time to build the actual dataset to use in the classification
# models
fdate <- seq.Date(from = as.Date('2004-05-01'), to = as.Date('2016-09-30'), 
                  by = 1)

# only keep summer months
df.all <- data.frame(fdate = fdate[month(fdate) %in% 5:9])

# time to build df.all one day at a time. To do so, I'll create a function that
# I can use sapply with to extract all the relevant features from df.mos
getDayData <- function(date, data) {
    df.date <- data[date(data$ftimelcl) == date &
                        date(data$runtimelcl) == date - days(1),]
    
    # get max and minimum temperature for the day
    tmax <- max(c(df.date.test$n_x, df.date.test$tmp), na.rm = TRUE)
    tmin <- min(c(df.date.test$n_x, df.date.test$tmp), na.rm = TRUE)
    
    # average dewpoint for the day
    dpt <- mean(df.date$dpt, na.rm = TRUE)
    
    # 6 hr precipitation and thunderstorm chances for 8am, 2pm, and 8pm
    p06.08 <- df.date$p06[hour(df.date$ftimelcl) == 8]
    p06.14 <- df.date$p06[hour(df.date$ftimelcl) == 14]
    p06.20 <- df.date$p06[hour(df.date$ftimelcl) == 20]
    t06.08 <- df.date$t06[hour(df.date$ftimelcl) == 8]
    t06.14 <- df.date$t06[hour(df.date$ftimelcl) == 14]
    t06.20 <- df.date$t06[hour(df.date$ftimelcl) == 20]
    
    # 12 hr precipitation at 8am/8pm
    p12.08 <- df.date$p12[hour(df.date$ftimelcl) == 8]
    p12.20 <- df.date$p12[hour(df.date$ftimelcl) == 20]
    
    return(list(tmax = tmax, tmin = tmin, dpt = dpt, p06.08 = p06.08, 
                p06.14 = p06.14, p06.20 = p06.20, t06.08 = t06.08, 
                t06.14 = t06.14, t06.20 = t06.20, p12.08 = p12.08, 
                p12.20 = p12.20, data = df.date))
}