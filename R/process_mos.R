# process_mos.R
# Bryan Holman // v0.1 // 20170513

# Extract desired information from the raw KMLB MOS dataset, and transform it
# into desired form to load into classification models.

# libraries ---------------------------------------------------------------
library(lubridate) # improved date handling

# functions ---------------------------------------------------------------


# data --------------------------------------------------------------------
df.mos <- read.csv('data/kmlb_mos_2000_2016.csv', row.names = FALSE,
                   stringsAsFactors = FALSE)

# remove the row.names column
colnames(df.mos) <- c(colnames(df.mos)[-1], 'rmv')
df.mos <- df.mos[,!names(df.mos) %in% c('rmv', 'station', 'snw', 'cig', 'vis', 
                                        'obv', 'poz', 'pos', 'typ', 'cld')]

# change runtime and ftime to POSIXct objects
df.mos$runtime <- as.POSIXct(gsub('\\+\\d\\d', '', df.mos$runtime), tz = 'UTC')
df.mos$ftime <- as.POSIXct(gsub('\\+\\d\\d', '', df.mos$ftime), tz = 'UTC')

# save this edited df.mos dataset
save(df.mos, file = 'data/kmlb_mos_processed.RData')
