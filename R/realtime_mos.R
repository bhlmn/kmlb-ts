# realtime_mos.R
# Bryan Holman // v0.1 // 20170517

# Get real time MOS forecasts and process data accordingly

# libraries ---------------------------------------------------------------
library(lubridate) # awesome data handling
library(WindVerification) # wind speed averaging

# functions ---------------------------------------------------------------
# function that I can use lapply with to extract all the relevant features 
# from df.mos
getDayData <- function(date, data) {
    df.date <- data[date(data$ftimelcl) == date,]
    
    # if there is no mos data for this date, return NAs
    if (length(df.date$model) == 0) {
        return(list(date = date, tmax = NA, tmin = NA, dpt = NA, wspd = NA, 
                    wdir = NA, p06.08 = NA, p06.14 = NA, p06.20 = NA, 
                    t06.08 = NA, t06.14 = NA, t06.20 = NA, s06.08 = NA, 
                    s06.14 = NA, s06.20 = NA, p12.08 = NA, p12.20 = NA))
    }
    
    # get max and minimum temperature for the day
    tmax <- max(c(df.date$n_x, df.date$tmp), na.rm = TRUE)
    tmin <- min(c(df.date$n_x, df.date$tmp), na.rm = TRUE)
    
    # average dewpoint for the day
    dpt <- mean(df.date$dpt, na.rm = TRUE)
    
    # average wind speed and wind direction for day
    uvs <- mapply(getuv, df.date$wsp, df.date$wdr)
    u.avg <- mean(uvs[1,], na.rm = TRUE)
    v.avg <- mean(uvs[2,], na.rm = TRUE)
    wspdwdir <- getwspdwdir(u.avg, v.avg)
    
    # 6 hr precipitation, thunderstorm, severe chances for 8am, 2pm, and 8pm
    p06.08 <- df.date$p06[hour(df.date$ftimelcl) == 8]
    p06.14 <- df.date$p06[hour(df.date$ftimelcl) == 14]
    p06.20 <- df.date$p06[hour(df.date$ftimelcl) == 20]
    t06.08 <- df.date$t06[hour(df.date$ftimelcl) == 8]
    t06.14 <- df.date$t06[hour(df.date$ftimelcl) == 14]
    t06.20 <- df.date$t06[hour(df.date$ftimelcl) == 20]
    s06.08 <- df.date$svr06[hour(df.date$ftimelcl) == 8]
    s06.14 <- df.date$svr06[hour(df.date$ftimelcl) == 14]
    s06.20 <- df.date$svr06[hour(df.date$ftimelcl) == 20]
    
    # 12 hr precipitation at 8am/8pm
    p12.08 <- df.date$p12[hour(df.date$ftimelcl) == 8]
    p12.20 <- df.date$p12[hour(df.date$ftimelcl) == 20]
    
    return(list(date = date, tmax = tmax, tmin = tmin, dpt = dpt, 
                wspd = round(wspdwdir[1], 2), wdir = wspdwdir[2], 
                p06.08 = p06.08, 
                p06.14 = p06.14, p06.20 = p06.20, t06.08 = t06.08, 
                t06.14 = t06.14, t06.20 = t06.20, s06.08 = s06.08, 
                s06.14 = s06.14, s06.20 = s06.20, p12.08 = p12.08, 
                p12.20 = p12.20))
}

# data --------------------------------------------------------------------
today <- Sys.Date()
yesterday <- Sys.Date() - days(1)
tomorrow <- Sys.Date() + days(1)

# get the ia state mesonet csv url for yesterday's 18 UTC GFS MOS
url <- paste('https://mesonet.agron.iastate.edu/mos/csv.php?station=KMLB&', 
             'runtime=', yesterday, '%2018:00&model=GFS', sep = '')
df.realtime <- read.csv(url(url), row.names = NULL, header = TRUE, 
                        stringsAsFactors = FALSE)

# data processing ---------------------------------------------------------

# remove the row.names column
colnames(df.realtime) <- c(colnames(df.realtime)[-1], 'rmv')
df.realtime <- df.realtime[,!names(df.realtime) %in% c('rmv', 'station', 'snw', 'cig', 'vis', 
                                        'obv', 'poz', 'pos', 'typ', 'cld', 
                                        'q06', 'q12', 't12')]

# change runtime and ftime to POSIXct objects
df.realtime$runtime <- as.POSIXct(gsub('\\+\\d\\d', '', df.realtime$runtime), tz = 'UTC')
df.realtime$ftime <- as.POSIXct(gsub('\\+\\d\\d', '', df.realtime$ftime), tz = 'UTC')

# convert runtime and forecast time to local time
df.realtime$runtimelcl <- format(df.realtime$runtime, tz="America/New_York",usetz=TRUE)
df.realtime$ftimelcl <- format(df.realtime$ftime, tz="America/New_York",usetz=TRUE)

# make t06 numeric, save severe tstorm probabilities as svr06
df.realtime$svr06 <- sapply(df.realtime$t06, FUN = function(x) 
    as.numeric(unlist(strsplit(x, '/'))[2]))
df.realtime$t06 <- sapply(df.realtime$t06, FUN = function(x) 
    as.numeric(unlist(strsplit(x, '/'))[1]))

# feature engineering -----------------------------------------------------

# Now it is time to build the actual dataset to use in the classification
# models
fdate <- c(as.Date(today), as.Date(tomorrow))

# apply getDayData for today and tomorrow, then turn this info into the 
# dataframe df.final
final <- lapply(fdate, getDayData, data = df.realtime)
df.final <- as.data.frame(matrix(unlist(final), ncol = length(final[[1]]), 
                                 byrow = TRUE))
colnames(df.final) <- names(final[[1]])
df.final$date <- fdate

# cleaning
rm(final, fdate)

# turn wdir into a categorical, if wspd < 3 knts, then state no mean wind 
# direction
df.final$wdir.cat[df.final$wspd < 3] <- 'None'
df.final$wdir.cat[df.final$wspd >= 3 & 
                      (df.final$wdir < 22.5 | df.final$wdir > 337.5)] <- 'N'
df.final$wdir.cat[df.final$wspd >= 3 & 
                      (df.final$wdir >= 22.5 & df.final$wdir < 67.5)] <- 'NE'
df.final$wdir.cat[df.final$wspd >= 3 & 
                      (df.final$wdir >= 67.5 & df.final$wdir < 112.5)] <- 'E'
df.final$wdir.cat[df.final$wspd >= 3 & 
                      (df.final$wdir >= 112.5 & df.final$wdir < 157.5)] <- 'SE'
df.final$wdir.cat[df.final$wspd >= 3 & 
                      (df.final$wdir >= 157.5 & df.final$wdir < 202.5)] <- 'S'
df.final$wdir.cat[df.final$wspd >= 3 & 
                      (df.final$wdir >= 202.5 & df.final$wdir < 247.5)] <- 'SW'
df.final$wdir.cat[df.final$wspd >= 3 & 
                      (df.final$wdir >= 247.5 & df.final$wdir < 292.5)] <- 'W'
df.final$wdir.cat[df.final$wspd >= 3 & 
                      (df.final$wdir >= 292.5 & df.final$wdir < 337.5)] <- 'NW'

# make wdir.cat categorical
df.final$wdir.cat <- factor(df.final$wdir.cat)

# add the day of the year
df.final$doy <- as.numeric(strftime(df.final$date, format = '%j'))

# add cosine (more or less) of the day of the year
df.final$cosdoy <- cos((((df.final$doy/365) * 360) + 180) * pi/180)

# cleaning
rm(df.realtime)
df.realtime <- df.final
rm(df.final)

# save df.realtime
save(df.realtime, file = 'data/realtime_mos.RData')