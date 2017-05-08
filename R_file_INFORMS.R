#read file
# 
# boxplot(data$YieldperRM ~ data$YEAR
#         ,ylim = range(0,100)
#         ,xlab = "Year"
#         ,ylab = "Yield per RM"
#         ,main = "Boxplot for yield per RM")


# variety_years=aggregate(cbind(VARIETY)~YEAR,data=data,function(VARIETY) length(unique(VARIETY)))
# head(variety_years)
# tail(variety_years)
# 
# variety_years=variety_years[order(variety_years$YEAR,decreasing = TRUE),]
# head(variety_years,10)
# dim(variety_years)
# df=aggregate(variety_years$VARIETY~variety_years$YEAR,data=variety_years,FUN = length)
# df
# 
# df=aggregate(variety_years$VARIETY~variety_years$YEAR,data=variety_years,FUN = length)
# df

#install.packages("lubridate")
#library(lubridate)

###############################################################################
# 
# rmfilter_data=data[data$RM >= 1,]
# summary(rmfilter_data)
# 
# rmfilter_data$YieldperRM=rmfilter_data$YIELD/rmfilter_data$RM
# summary(rmfilter_data)
# 
# boxplot(rmfilter_data$YieldperRM ~ rmfilter_data$YEAR
#         ,xlab = "Year"
#         ,ylab = "Yield per RM"
#         ,main = "Boxplot for yield per RM")
# 
# boxplot(rmfilter_data$BAGSOLD ~ rmfilter_data$CHECK
#         ,xlab = "CHECK"
#         ,ylab = "BAGSOLD"
#         ,main = "Boxplot for BAGSOLD with CHECK")
# 
# #DF=as.numeric(rmfilter_data)
# plot(y=rmfilter_data$BAGSOLD,x=rmfilter_data$YieldperRM)

# inputting the data
Experiment_data=read.csv("B:\\MS BAIM\\INFORMS OR case competition\\Experiment Data.csv"
              ,head=TRUE,sep=","
              ,colClasses = c("factor","factor","factor","factor"
                              ,"factor","factor","numeric","numeric"
                              ,"numeric","factor","factor","numeric"))

data_with_bagsold = Experiment_data[Experiment_data$BAGSOLD>0 & !is.na(Experiment_data$BAGSOLD),]
gradYear2014 = Experiment_data[Experiment_data$CLASS_OF=='2014',]
data = Experiment_data
data = data[!data$RM<1,]
data$YieldperRM=data$YIELD/data$RM
gradYear2014$YieldperRM=gradYear2014$YIELD/gradYear2014$RM

data = data[data$GRAD=='YES' | data$GRAD=='NO',]
data = data[data$YEAR=='2012' | data$YEAR=='2013' | data$YEAR=='2014',]

grad_data = data[data$YEAR=='2012' | data$YEAR=='2013',]
data_2014 = data[data$YEAR=='2014',]

hist(grad_data[grad_data$YEAR=='2012',]$BAGSOLD,xlab = 'BAGSOLD',cex.lab = 1.5
     ,main='Distribution of BAGSOLD in 2012',border = 'black',col = 'blue',xlim = c(0,2000000))

hist(grad_data[grad_data$YEAR=='2013',]$BAGSOLD,xlab = 'BAGSOLD',cex.lab = 1.5
     ,main='Distribution of BAGSOLD in 2013',border = 'black',col = 'blue',xlim = c(0,2000000))


# t-tests
# t.test(rmfilter_data[rmfilter_data$CHECK=="FALSE","YieldperRM"]
#        ,rmfilter_data[rmfilter_data$CHECK=="TRUE","YieldperRM"]
#        ,alternative = "greater")
# 
# t.test(rmfilter_data[rmfilter_data$CHECK=="FALSE","YIELD"]
#        ,rmfilter_data[rmfilter_data$CHECK=="TRUE","YIELD"]
#        ,alternative = "greater")
# 
# # to check which is a good measure of good variety
# t.test(rmfilter_data[rmfilter_data$CHECK=="FALSE" & rmfilter_data$BAGSOLD>0,"BAGSOLD"]
#        ,rmfilter_data[rmfilter_data$CHECK=="TRUE" & rmfilter_data$BAGSOLD>0,"BAGSOLD"]
#        ,alternative = "greater")
# 
# 
# head(rmfilter_data[rmfilter_data$CHECK=="True","YieldperRM"])
# head(rmfilter_data[rmfilter_data$CHECK=="False",])

############## Creating dummy variables for grad_data ###############
dummies_CLASS_OF = model.matrix(~grad_data[,c("CLASS_OF")]-1)
colnames(dummies_CLASS_OF)=c('CLASS_OF_','CLASS_OF_2011','CLASS_OF_2012'
                             ,'CLASS_OF_2013','CLASS_OF_2014')

dummies_CHECK = model.matrix(~grad_data[,'CHECK']-1)
colnames(dummies_CHECK) = c('CHECK_TRUE','CHECK_FALSE')

dummies_YEAR = model.matrix(~grad_data$YEAR-1)
colnames(dummies_YEAR) = c('YEAR_2009','YEAR_2010','YEAR_2011','YEAR_2012'
                           ,'YEAR_2013','YEAR_2014')

grad_data=data.frame(grad_data,dummies_CLASS_OF,dummies_CHECK,dummies_YEAR)
summary(grad_data)

############# aggregating the grad_data ###############
YIELD = aggregate(YIELD ~ VARIETY + YEAR, data = grad_data, mean)
BAGSOLD = aggregate(BAGSOLD ~ VARIETY + YEAR, data = grad_data, mean)
RM = aggregate(RM ~ VARIETY + YEAR, data = grad_data, mean)
#CHECK = aggregate(CHECK ~ VARIETY + YEAR, data = grad_data, mean)

agg_grad_data = merge(YIELD, BAGSOLD,by = c('VARIETY','YEAR'))
agg_grad_data = merge(agg_grad_data, RM,by = c('VARIETY','YEAR'))
head(agg_grad_data)
summary(agg_grad_data)

hist(agg_grad_data[agg_grad_data$YEAR=='2012',]$BAGSOLD,xlab = 'BAGSOLD in 2012',main='BAGSOLD')
hist(agg_grad_data[agg_grad_data$YEAR=='2013',]$BAGSOLD,xlab = 'BAGSOLD in 2013',main='BAGSOLD')

############## Creating dummy variables for gradYear2014 ###############
dummies_CLASS_OF = model.matrix(~gradYear2014[,c("CLASS_OF")]-1)
colnames(dummies_CLASS_OF)=c('CLASS_OF_','CLASS_OF_2011','CLASS_OF_2012'
                             ,'CLASS_OF_2013','CLASS_OF_2014')

dummies_CHECK = model.matrix(~gradYear2014[,'CHECK']-1)
colnames(dummies_CHECK) = c('CHECK_TRUE','CHECK_FALSE')

dummies_YEAR = model.matrix(~gradYear2014$YEAR-1)
colnames(dummies_YEAR) = c('YEAR_2009','YEAR_2010','YEAR_2011','YEAR_2012'
                           ,'YEAR_2013','YEAR_2014')

gradYear2014=data.frame(gradYear2014,dummies_CLASS_OF,dummies_CHECK,dummies_YEAR)

dim(gradYear2014)

############## Creating dummy variables for data_2014 ###############
dummies_CLASS_OF = model.matrix(~data_2014[,c("CLASS_OF")]-1)
colnames(dummies_CLASS_OF)=c('CLASS_OF_','CLASS_OF_2011','CLASS_OF_2012'
                             ,'CLASS_OF_2013','CLASS_OF_2014')

dummies_CHECK = model.matrix(~data_2014[,'CHECK']-1)
colnames(dummies_CHECK) = c('CHECK_TRUE','CHECK_FALSE')

dummies_YEAR = model.matrix(~data_2014$YEAR-1)
colnames(dummies_YEAR) = c('YEAR_2009','YEAR_2010','YEAR_2011','YEAR_2012'
                           ,'YEAR_2013','YEAR_2014')

data_2014=data.frame(data_2014,dummies_CLASS_OF,dummies_CHECK,dummies_YEAR)

dim(data_2014)
############# making clusters for data of 2014 ######################
cluster1 = gradYear2014[gradYear2014$RM>=3.55,]
cluster2 = gradYear2014[gradYear2014$RM<3.55 & gradYear2014$RM>=3.25,]
cluster3 = gradYear2014[gradYear2014$RM<3.25 & gradYear2014$RM>=2.75,]
cluster4 = gradYear2014[gradYear2014$RM<2.75 & gradYear2014$RM>=2.55,]
cluster5 = gradYear2014[gradYear2014$RM<2.55 & gradYear2014$RM>=2.35,]
cluster6 = gradYear2014[gradYear2014$RM<2.35 & gradYear2014$RM>=2.00,]
cluster7 = gradYear2014[gradYear2014$RM<2.00,]
# ####################################################################
# set.seed(2016)
# inTrain=createDataPartition(data$CHECK,p=0.70,list=F)
# train=data[inTrain,]
# test=data[-inTrain,]
# summary(train)

#####################################################################

set.seed(2016)
inTrain_2014=createDataPartition(data_2014$REPNO,p=0.70,list=F)
train_2014=data_2014[inTrain,]
test_2014=data_2014[-inTrain,]
summary(train_2014)
dim(train_2014)

########### Splitting data on the basis of graduation year ###########
gradYear2011 = grad_data[grad_data$CLASS_OF=='2011',]
gradYear2012 = grad_data[grad_data$CLASS_OF=='2012',]
gradYear2013 = grad_data[grad_data$CLASS_OF=='2013',]

###############################################
library(caret)
library(ISLR)
library(pROC)
library(MASS)

set.seed(2016)

inTrain=createDataPartition(data_with_bagsold$CHECK,p=0.70,list=F)
train_with_bagsold=data_with_bagsold[inTrain,]
test_with_bagsold=data_with_bagsold[-inTrain,]

inTrain=createDataPartition(grad_data$CHECK,p=0.70,list=F)
train=grad_data[inTrain,]
test=grad_data[-inTrain,]

inTrain=createDataPartition(gradYear2011$CHECK,p=0.70,list=F)
train_2011=gradYear2011[inTrain,]
test_2011=gradYear2011[-inTrain,]

inTrain=createDataPartition(gradYear2012$CHECK,p=0.70,list=F)
train_2012=gradYear2012[inTrain,]
test_2012=gradYear2012[-inTrain,]

inTrain=createDataPartition(gradYear2013$CHECK,p=0.70,list=F)
train_2013=gradYear2013[inTrain,]
test_2013=gradYear2013[-inTrain,]

ctrl <- trainControl(method = "cv"
                     ,classProbs = TRUE
                     ,summaryFunction = twoClassSummary)

#logit models using caret
# names(train)
# logit <- train(CHECK ~ BAGSOLD+YieldperRM
#                ,data = train
#                ,method = "glm"
#                ,family = "binomial"
#                ,trControl = ctrl
#                ,metric = "ROC")
# 
# logitprobs <- predict(logit,newdata=test,type="prob")[,1]
# logitprobs
# logitClasses <- predict(logit,newdata=test)
# logitClasses
# confusionMatrix(data=logitClasses, test$CHECK)

# pairs(grad_data)
# cor(grad_data)

###################### Step-8 #########################
rocCurve=roc(response=test$CHECK
             ,predictor = logitprobs
             ,levels=rev(levels(test$CHECK)))#reversing the labels in this case for"Up" and "Down"

par(mfrow=c(1,1)) #reset plot graphics to one plot

plot(rocCurve
     ,legal.axes=T
     ,col="red"
     ,main="Receiver Operating Characterstic (ROC) Curve")
#?legend
legend("bottomright"
       ,inset=0,title="Model"
       ,border="black"
       ,bty="n"
       ,cex=0.8
       ,legend=c("Logit")
       ,fill=c("red"))

###################### Step-9 #########################
auc(rocCurve)

################# Linear model estimating BAGSOLD ################

linear.model=step(lm(BAGSOLD~YIELD+I(YIELD^2)+I(YIELD^3)+RM+I(RM^2)+I(RM^3)
                     +YieldperRM+I(YieldperRM^2)+I(YieldperRM^3)
                     +I(YIELD * RM^2)+I(YieldperRM*YIELD)
                     +CHECK_TRUE+CHECK_FALSE+REPNO
                ,data = train),direction='backward')
summary(linear.model)

pred=predict(linear.model,newdata=data_2014)
summary(pred)
hist(pred)

BAGSOLD = data.frame(pred)
pred
cluster1_eval=data.frame(cluster1,pred)

ctrl=trainControl(classProbs = FALSE,summaryFunction = defaultSummary)
Ridgefit=train(BAGSOLD~YIELD+I(YIELD^2)+I(YIELD^3)+RM+I(RM^3)
               +YieldperRM+I(YieldperRM^2)+I(YieldperRM^3)
               +YEAR_2012+YEAR_2013
               +CHECK_TRUE+REPNO
               ,data = train[train$RM<3.55 & train$RM>=3.25,]
               ,method = 'foba'
               ,trControl = ctrl
               ,preProcess=c("center","scale")
               ,tuneLength = 16
               ,metric = 'RMSE')
summary(Ridgefit)

lassofit=train(BAGSOLD~YIELD+I(YIELD^2)+I(YIELD^3)+RM+I(RM^3)
               +YieldperRM+I(YieldperRM^2)+I(YieldperRM^3)
               +YEAR_2012+YEAR_2013+YEAR
               +CHECK_TRUE+REPNO
               ,data = train[train$RM>=3.55,]
               ,method = 'lars'
               ,trControl = ctrl
               ,preProcess=c("center","scale")
               ,tuneLength = 16
               ,metric = 'RMSE')
lassofit

pred = predict(Ridgefit,newdata = gradYear2014[gradYear2014$RM>=3.55,])
summary(pred)
hist(pred)

validate_lm = defaultSummary(data=data.frame(obs=gradYear2014[gradYear2014$RM>=2.75 & gradYear2014$RM<3.25 ,]$BAGSOLD
                                             ,pred=predict(linear.model,newdata=gradYear2014[gradYear2014$RM>=2.75 & gradYear2014$RM<3.25,])))
validate_lm

obs=gradYear2013_test$BAGSOLD
pred=predict(linear.model,newdata=gradYear2013_test)
difference=data.frame(obs,pred)
difference

testYhatRidge <- predict(RidgeFit,newdata=test)
TestErrorRidge <- postResample(pred=testYhatRidge, obs=test$Apps) 
TestErrorRidge[[1]]^2

################################################################################
# set.seed(2016)
# # 
# levels(grad_data$CHECK) <- make.names(levels(factor(grad_data$CHECK)))
# # grad_data$CHECK <- relevel(grad_data$CHECK, 'FALSE')
# 
# control <- trainControl(method = "cv", classProbs = "TRUE", summaryFunction = twoClassSummary)
# 
# LDA <- train(train$CHECK~RM+YEAR+CLASS_OF+YieldperRM, data=train, method="lda", family="binomial", trControl = control, metric="ROC")
# LDAProbs <- predict(LDA, newdata = test, type="probs")[,1]
# LDAProbs
# 
# QDA <- train(train$CHECK~RM+YEAR+CLASS_OF+YieldperRM, data=train, method="qda", family="binomial", trControl = control, metric="ROC")
# QDAProbs <- predict(QDA, newdata = test, type="probs")[,1]
# QDAProbs
################################################################################
################# Applying Decision Tree, Random Forest and SVM ###############
set.seed(2016)
#install.packages("tree")
library(tree)
library(randomForest)
library(e1071)

dim(grad_data)
boxplot(BAGSOLD ~ CHECK,data = grad_data)
summary(train)

cor.test(grad_data$YIELD,grad_data$YieldperRM)

treefit_1 = tree(CHECK~YIELD+RM+REPNO
               ,control = tree.control(nobs = nrow(train)
                                      ,mincut = 0
                                     ,minsize = 1
                                    ,mindev = 0.01)
           ,data = train)
summary(treefit_1)
plot(treefit_1);text(treefit_1, pretty = 0)

treefit_2 = tree(CHECK~BAGSOLD+YIELD+RM
                 ,control = tree.control(nobs = nrow(train)
                                         ,mincut = 0
                                         ,minsize = 1
                                         ,mindev = 0.01)
                 ,data = train)
summary(treefit_2)
plot(treefit_2);text(treefit_2, pretty = 0)

################ Pruning the tree ##################
head((train$CHECK))
cv.grad_data = cv.tree(treefit_1)
cv.grad_data

plot(x=cv.grad_data$size,y=cv.grad_data$dev,type = 'b', col = "blue"
     ,xlab = "Number of Terminal Nodes",ylab = "Cross-validated error")

prunedfit = prune.tree(treefit_1, best = 3)
summary(prunedfit)

plot(prunedfit);text(prunedfit, pretty = 0)

############ Random forest #################
rf = randomForest(CHECK~RM+YIELD+YieldperRM
                  # ,mtry = 5
                  # ,ntree = 200
                  ,replace = TRUE
                  # ,sampsize = 300
                  ,data = train)
summary(rf)
plot(rf,type = "simple")
############# KNN Clustering ##############
library(class)
sqrt(11837)
m1 = knn(train = train
         ,test = test
         ,cl=train$CHECK
         ,k=109)
head(m1)

############### Validating Tree Performance ###########
testYhatTree = predict(treefit_1,newdata = train,prob = TRUE)
testYhatTree[testYhatTree[,2]>0.5,2] = 1
testYhatTree[testYhatTree[,2]<=0.5,2] = 0

testYhatforest = predict(rf,type = "prob",newdata = test)
testYhatforest[testYhatforest[,2]>0.5,2] = 1
testYhatforest[testYhatforest[,2]<=0.5,2] = 0

testYhatsvm = predict(tune.out$best.model,type = "prob",newdata = test)
testYhatsvm[testYhatsvm[,2]>0.5,2] = 1
testYhatsvm[testYhatsvm[,2]<=0.5,2] = 0

head(testYhatTree)
testYhatforest
testYhatsvm = as.numeric(testYhatsvm)
testYhatsvm = testYhatsvm-1
testYhatsvm
test$CHECK_TRUE
confusionMatrix(data = testYhatTree[,2], test$CHECK_TRUE, positive = '1')
confusionMatrix(data = testYhatforest[,2], test$CHECK_TRUE, positive = '1')
confusionMatrix(data = testYhatsvm, test$left_numeric, positive = '1')
summary(grad_data)

########## Applying Least squares SVM ###########
#install.packages("kernlab")
library(kernlab)
library(caret)

lssvm.fit = lssvm(CHECK~RM+YEAR+CLASS_OF+YieldperRM
      ,data = train
      ,tau = 0.01
      ,decision.values = T) 
summary(lssvm.fit)
alpha(lssvm.fit)
error(lssvm.fit)
lev(lssvm.fit)
library(ROCR)
fitted = attributes(predict(lssvm.fit,train,decision.values=TRUE))$decision.values
predob = prediction(fitted,train[,"CHECK"])
perf = performance(predob,"tpr","fpr")
plot(perf)


library(MASS)
lda.fit = lda(CHECK~RM+YEAR+CLASS_OF+YieldperRM
              ,data = train)

Summary(lda.fit)

