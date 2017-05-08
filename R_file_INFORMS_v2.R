############# Calling the libraries required ############
library(caret)
library(ISLR)

################ Input the Raw Data file ################
Experiment_data=read.csv("B:\\MS BAIM\\INFORMS OR case competition\\Experiment Data.csv"
                         ,head=TRUE,sep=","
                         ,colClasses = c("factor","factor","factor","factor"
                                         ,"factor","factor","numeric","numeric"
                                         ,"numeric","factor","factor","numeric"))

dim(Experiment_data)
################# Experimental data #####################
data_with_bagsold = Experiment_data[Experiment_data$BAGSOLD>0 & !is.na(Experiment_data$BAGSOLD),]
summary(data_with_bagsold)

############# Aggregating the Experimental data ###############
YIELD = aggregate(YIELD ~ VARIETY + YEAR, data = data_with_bagsold, sum)
BAGSOLD = aggregate(BAGSOLD ~ VARIETY + YEAR, data = data_with_bagsold, sum)
RM = aggregate(RM ~ VARIETY + YEAR, data = data_with_bagsold, mean)
#CHECK = aggregate(CHECK ~ VARIETY + YEAR, data = data_with_bagsold, mean)

agg_data = merge(YIELD, BAGSOLD,by = c('VARIETY','YEAR'))
agg_data = merge(agg_data, RM,by = c('VARIETY','YEAR'))
head(agg_data)
summary(agg_data)

################ making dummy variables for Year ##################

dummies_YEAR = model.matrix(~agg_data$YEAR-1)
colnames(dummies_YEAR) = c('YEAR_2009','YEAR_2010','YEAR_2011','YEAR_2012'
                           ,'YEAR_2013','YEAR_2014')

agg_data=data.frame(agg_data,dummies_YEAR)

########### Splitting into the training and the test set ##########
inTrain=sample(nrow(agg_data),112*0.7, replace = FALSE)
train_with_bagsold=agg_data[inTrain,]
test_with_bagsold=agg_data[-inTrain,]

################ training the models ###################
linear.model=step(lm(BAGSOLD~YIELD+I(YIELD^2)+I(YIELD^3)+RM+I(RM^2)+I(RM^3)
                     +YEAR_2012+YEAR_2013+YEAR_2014+YEAR_2009+YEAR_2010+YEAR_2011
                     ,data = train_with_bagsold),direction='backward')
summary(linear.model)


validate_lm = defaultSummary(data=data.frame(obs=test_with_bagsold$BAGSOLD
                                             ,pred=predict(linear.model,newdata=test_with_bagsold)))
validate_lm

##################### Ridgefit ########################

ctrl=trainControl(classProbs = FALSE,summaryFunction = defaultSummary)
Ridgefit=train(BAGSOLD~YIELD+I(YIELD^2)+I(YIELD^3)+RM+I(RM^2)+I(RM^3)
               +YEAR_2012+YEAR_2013+YEAR_2014+YEAR_2009+YEAR_2010+YEAR_2011
               ,data = train_with_bagsold
               ,method = 'foba'
               ,trControl = ctrl
               ,preProcess=c("center","scale")
               ,tuneLength = 16
               ,metric = 'RMSE')

Ridgefit

validate_lm = defaultSummary(data=data.frame(obs=test_with_bagsold$BAGSOLD
                                             ,pred=predict(Ridgefit,newdata=test_with_bagsold)))
validate_lm

##################### Lassofit ########################

lassofit=train(BAGSOLD~YIELD+I(YIELD^2)+I(YIELD^3)+RM+I(RM^2)+I(RM^3)
               +YEAR_2012+YEAR_2013+YEAR_2014+YEAR_2009+YEAR_2010+YEAR_2011
               ,data = train_with_bagsold
               ,method = 'lars'
               ,trControl = ctrl
               ,preProcess=c("center","scale")
               ,tuneLength = 16
               ,metric = 'RMSE')
lassofit
coefficients = coef(lassofit$finalModel,lassofit$bestTune$fraction)
coefficients

names(coefficients[which(coefficients != 0),])

validate_lm = defaultSummary(data=data.frame(obs=test_with_bagsold$BAGSOLD
                                             ,pred=predict(lassofit,newdata=test_with_bagsold)))
validate_lm

pred = predict(Ridgefit,newdata = test_with_bagsold)
summary(pred)
hist(pred)

############### Preparing Evaluation set ####################
Evaluation_set = Experiment_data[Experiment_data$CLASS_OF=='2014',]
dim(Evaluation_set)
############# aggregating the Evaluation data ###############
YIELD = aggregate(YIELD ~ VARIETY + YEAR, data = Evaluation_set, sum)
RM = aggregate(RM ~ VARIETY + YEAR, data = Evaluation_set, mean)

agg_eval_data = merge(YIELD, RM,by = c('VARIETY','YEAR'))
dim(agg_eval_data)
summary(agg_eval_data)
################ making dummy for Year ######################

dummies_YEAR = model.matrix(~agg_eval_data$YEAR-1)
colnames(dummies_YEAR) = c('YEAR_2009','YEAR_2010','YEAR_2011','YEAR_2012'
                           ,'YEAR_2013','YEAR_2014')

agg_eval_data=data.frame(agg_eval_data,dummies_YEAR)

############# Predicting using the appropriate model #############
############# and adding that to the column in the Evaluation set #########
pred = predict(lassofit,newdata = agg_eval_data)
agg_eval_data$BAGSOLD = predict(lassofit,newdata = agg_eval_data)
agg_eval_data$BAGSOLD = agg_eval_data$BAGSOLD/50
hist(agg_eval_data$BAGSOLD,xlab = 'BAGSOLD',cex.lab = 1.5
     ,main='Distribution of BAGSOLD in 2014',border = 'black',col = 'blue')

########## Output the final file with Predicted values on the Evaluation set ######
write.csv(agg_eval_data,file = 'B:\\MS BAIM\\INFORMS OR case competition\\Evaluation set with Bagsold.csv'
          ,row.names = FALSE)


