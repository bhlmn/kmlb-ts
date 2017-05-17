# data_exploration.R
# Bryan Holman // v0.1 // 20170516

# I now have a pretty good dataset, and want to do some basic exploration of 
# this data before I run some classification models!

# libraries ---------------------------------------------------------------
library(ggplot2) # awesome data visualization
library(plyr)

# data --------------------------------------------------------------------
load('data/kmlb_mos_scaled_verification.RData')
df.final$TShr2 <- df.scaled$TShr2
df.final$TScat <- df.scaled$TScat
df.final$TScat <- revalue(df.final$TScat,
                          c('0' = 'None',
                            '1' = 'Morning (9.0%)', 
                            '2' = 'Afternoon (70.9%)', 
                            '3' = 'Evening (17.5%)', 
                            '4' = 'Late Night (2.5%)'))
df.final$TScat <- factor(df.final$TScat,
                         levels = c('None', 'Morning (9.0%)', 
                                    'Afternoon (70.9%)', 'Evening (17.5%)', 
                                    'Late Night (2.5%)'))

# exploration -------------------------------------------------------------

# what is the frequency of each category?
count <- table(df.final$TScat)[-1]
freq <- round((count * 100)/sum(count), 1)
freqtext <- paste(freq, '%', sep = '')

breaks <- seq(6, 29, by = 3)
labels <- c('6am', '9am', '12pm', '3pm', '6pm', '9pm', '12am', '3am')
minor <- 6:29

# let's begin by looking at histograms of TShr and TScat
p <- ggplot(df.final, aes(x = TShr2, fill = TScat)) +
    geom_bar(aes(y = (..count..)/sum(..count..)), color = 'black') +
    theme_light() + xlab('Hour of first thunderstorm') +
    ylab('Frequency (2005 - 2016)') + ylim(0, 0.2) +
    scale_x_continuous(breaks = breaks, labels = labels, minor_breaks = minor) +
    geom_text(stat = 'count', aes(y = (..count..)/sum(..count..),
                                  label = ..count..), vjust = -0.4) +
    theme(legend.position="bottom", legend.title = element_blank())
print(p)
ggsave('docs/img/ts_by_hr.png', height = 4, width = 7, units = 'in', dpi = 150)

# let's look at TScat by year
p <- ggplot(df.final, aes(x = TScat, fill = TScat)) +
    geom_bar(aes(y = ..count..), color = 'black') + facet_wrap(~ factor(year(date)))
print(p)

# Things still look suspect. There are a lot more thunderstorm days for 2014,
# 2015, and 2016. I wonder if they have changed what counts as being within
# the vicinity or something else ... but things definitely look weird!