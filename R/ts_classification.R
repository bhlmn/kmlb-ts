# ts_classification.R
# Bryan Holman // v0.1 // 20170517

# Time to perform the actual classification prediction!

# libraries ---------------------------------------------------------------
library(caTools) # splitting test/training datasets
library(randomForest) # random forest classifier
library(class) # k-nn classifier
library(knncat) # k-nn classifier that handles categorical variables
library(kknn) # weighted k-nn classifier
library(e1071) # support vector machine classifier

# functions ---------------------------------------------------------------

# calculate several metrics given a contingency table
getForecastMetrics <- function(cm, name) {
    a <- cm[1,1]
    b <- cm[1,2]
    c <- cm[2,1]
    d <- cm[2,2]
    n <- sum(cm)
    pc <- round((a+d)/n, 3)
    hr <- round(d/(c+d), 3)
    far <- round(b/(b+d), 3)
    ts <- round(d/(b+c+d), 3)
    bias <- round((b+d)/(c+d), 3)
    return(list(algorithm = name, percent_correct = pc, hit_rate = hr, 
                false_alarm_ratio = far, threat_score = ts, bias = bias))
}

# calculate skill scores given the forecast metrics
getForecastSkillScores <- function(cm, climo.cm, name) {
    cm.metrics <- getForecastMetrics(cm, name)
    climo.metrics <- getForecastMetrics(climo.cm, name)
    cm.metrics$ss_percent_correct <- 
        getSkillScore(cm.metrics$percent_correct, 
                      climo.metrics$percent_correct, 1)
    cm.metrics$ss_hit_rate <- getSkillScore(cm.metrics$hit_rate,
                                            climo.metrics$hit_rate, 1)
    cm.metrics$ss_false_alarm_ratio <- 
        getSkillScore(cm.metrics$false_alarm_ratio,
                      climo.metrics$false_alarm_ratio, 0)
    cm.metrics$ss_threat_score <- getSkillScore(cm.metrics$threat_score,
                                                climo.metrics$threat_score, 1)
    return(cm.metrics)
}

# calculate a skill score
getSkillScore <- function(a, ref, perf) {
    return(round(((a - ref) / (perf - ref)) * 100, 1))
}

# data --------------------------------------------------------------------
load('data/kmlb_mos_scaled_verification.RData')

# remove some columns that don't aid in classification
df.scaled <- df.scaled[,!colnames(df.scaled) %in% c('date', 'wdir', 'TShr2', 
                                                    'TScat')]

# create empty matrices to store information
climo.cm <- matrix(0, 2, 2)
log.cm <- matrix(0, 2, 2)
rf.cm <- matrix(0, 2, 2)
knn.cm <- matrix(0, 2, 2)
svm.cm <- matrix(0, 2, 2)
ens.cm <- matrix(0, 2, 2)

# number of tests to perform
tests <- 100

for (i in 1:tests) {
    
    print(i)
    
    # split data into training and test set
    split <- sample.split(df.scaled$TSbin, SplitRatio = 0.8)
    df.train <- subset(df.scaled, split == TRUE)
    df.test <- subset(df.scaled, split == FALSE)
    
    # create a dataframe that will store ensemble predictions
    df.ens <- data.frame(TSbin = df.test$TSbin)
    
    # references --------------------------------------------------------------
    
    # I need to have a reference forecast to see how well these machine learning
    # forecasts are. For starters I'll sample from climatology.
    
    # determine climatological odds of a thunderstorm occurring
    climo.prob <- table(df.scaled$TSbin)[2]/sum(table(df.scaled$TSbin))
    
    # make predictions given these probabilities
    climo.pred <- rbinom(n = length(df.test$TSbin), size = 1, prob = climo.prob)
    
    # Making the Confusion Matrix
    climo.cm <- climo.cm + table(df.test$TSbin, climo.pred)
    
    # classification ----------------------------------------------------------

    # Fitting Logistic Regression to the Training set
    log.classifier <- glm(formula = TSbin ~ .,
                          family = binomial,
                          data = df.train)
    
    # Predicting the Test set results
    log.prob <- predict(log.classifier, type = 'response', 
                        newdata = df.test[-19])
    log.pred <- ifelse(log.prob > 0.5, 1, 0)
    
    # Making the Confusion Matrix
    log.cm <- log.cm + table(df.test$TSbin, log.pred > 0.5)
    
    # add logistic regression probabilities to df.ens
    df.ens$log.prob <- log.prob
    
    # Fitting Random Forest Classification to the Training set
    rf.classifier <- randomForest(x = df.train[-19],
                                  y = df.train$TSbin,
                                  ntree = 500)
    
    # Predicting the Test set results
    rf.prob <- predict(rf.classifier, type = 'prob', newdata = df.test[-19])
    rf.pred <- ifelse(rf.prob[,2] > 0.5, 1, 0)
    
    # Making the Confusion Matrix
    rf.cm <- rf.cm + table(df.test$TSbin, rf.pred)
    
    # add random forest probabilities to df.ens
    df.ens$rf.prob <- rf.prob[,2]

    # Fitting Nearest Neighbors Classification to the Training set    

    # note that standard k-nn needs all continuous variables! Therefore for the 
    # standard k-nn I'll leave out the wind direction categorical feature
    knn.pred = knn(train = df.train[, -c(16, 19)],
                   test = df.test[, -c(16, 19)],
                   cl = df.train$TSbin,
                   k = 9,
                   prob = TRUE)
    knn.prob <- ifelse(as.numeric(knn.pred) - 1 == 1, attr(knn.pred, 'prob'), 
                       1 - attr(knn.pred, 'prob'))
    
    # Making the Confusion Matrix
    knn.cm <- knn.cm + table(df.test$TSbin, knn.pred)
    
    # add k-nn probabilities to df.ens
    df.ens$knn.prob <- knn.prob
    
    # Fitting Support Vector Machine Classification to the Training set 
    svm.classifier = svm(formula = TSbin ~ .,
                         data = df.train,
                         type = 'C-classification',
                         kernel = 'linear', 
                         probability = TRUE)
    
    # Predicting the Test set results
    svm.pred <- predict(svm.classifier, newdata = df.test[-19], 
                       probability = TRUE)
    svm.prob <- attr(svm.pred, 'probabilities')[,1]
    

    
    # Making the Confusion Matrix
    svm.cm <- svm.cm + table(df.test$TSbin, svm.pred)
    
    # add random forest probabilities to df.ens
    df.ens$svm.prob <- svm.prob
    
    
    # ensemble ----------------------------------------------------------------
    
    # average the probabilities for all methods
    df.ens$ens.prob <- rowMeans(df.ens[-1])
    ens.pred <- ifelse(df.ens$ens.prob > 0.5, 1, 0)
    
    # Making the Confusion Matrix
    ens.cm <- ens.cm + table(df.test$TSbin, ens.pred)
}

# Make a data frame of results
df.results <- as.data.frame(getForecastSkillScores(climo.cm, climo.cm, 
                                                   'Climatology'), 
                            stringsAsFactors = FALSE)
df.results <- rbind(df.results, 
                    getForecastSkillScores(log.cm, climo.cm, 
                                           'Logistic Regression'))
df.results <- rbind(df.results, 
                    getForecastSkillScores(rf.cm, climo.cm, 
                                           'Random Forest'))
df.results <- rbind(df.results, 
                    getForecastSkillScores(knn.cm, climo.cm, 
                                           'Nearest Neighbors'))
df.results <- rbind(df.results, 
                    getForecastSkillScores(svm.cm, climo.cm, 
                                           'Support Vector Machine'))
df.results <- rbind(df.results, 
                    getForecastSkillScores(ens.cm, climo.cm, 
                                           'Ensemble'))
save(df.results, file = 'data/classification_results.RData')