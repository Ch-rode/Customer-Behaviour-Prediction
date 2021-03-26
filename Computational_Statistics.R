## COMPUTATIONAL STATISTICS PROJECT ##

# Chiara Rodella 

##################################################################

##################################################################
### PACKAGES ###
##################################################################

suppressMessages(library(gridExtra))
suppressMessages(library(ggplot2))
suppressMessages(library(lattice))
suppressMessages(library(corrplot))
suppressMessages(library(plyr))
suppressMessages(library(dplyr))
suppressMessages(library(caret))
suppressMessages(library(ROSE))
suppressMessages(library(randomForest))
suppressMessages(library(partykit))
suppressMessages(library(tree))
suppressMessages(library(rpart))
suppressMessages(library(rpart.plot))
suppressMessages(library(bestglm))
suppressMessages(library(caret))
suppressMessages(library(boot))
suppressMessages(library(plot.matrix))
suppressMessages(library(gridExtra))
suppressMessages(library(ggplotify))
suppressMessages(library(rattle))

############################
### DATA PREPARATION #######
############################

data=read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv")

str(data) 
## We see that "SeniorCitizen" is an "int" we change it into a "Factor" with categoires YES/NO
## instead of 1/0

data$SeniorCitizen= as.factor(mapvalues(data$SeniorCitizen,
                                        from=c("0","1"),
                                        to=c("No", "Yes")))
str(data) ## check OK 

summary(data)

sapply(data, function(x) sum(is.na(x))) 
## There are Nas in "TotalCharges"

subset(data, is.na(data$TotalCharges))

## Only 11 rows out of 7043 total and 
## all with variable "Churn = No",
## As we will see later "Churn = No" is overrappresented 
## so we can remove theese rows

dim(data)

data=na.omit(data)

dim(data) 

tenure_initial=data[,"tenure"] 

## Will be usefull for a later analysis
TotalCharges_initial=data[,"TotalCharges"] 

## We see how some colums present a redundacy in "No" and "No internet services":

#View(data)

cols1 = c(10:15)

for(i in 1:ncol(data[,cols1])) {
  data[,cols1][,i] <- as.factor(mapvalues
                                (data[,cols1][,i], from =c("No internet service"),to=c("No")))
}

#View(data)

## "Multiple lines" Colum presents the same problem 

data$MultipleLines = as.factor(mapvalues(data$MultipleLines,
                                         from = c("No phone service"),
                                         to = c("No")))

View(data)

## implementing the quantitative variable "Tenure"

summary(data$tenure)

color = c('Mean'='green',
          '1st Q.'='darkgreen',
          '3rd Q.'='blue',
          'Median'='magenta',
          'MAX'='red',
          'min'='azure')
ggplot()+
  geom_density(data=data,mapping = aes(x=data$tenure),fill='lightblue')+
  geom_vline(mapping=aes(xintercept = mean(data$tenure),color='Mean'),
             linetype = "dashed", size = 0.6)+
  geom_vline(mapping=aes(xintercept = quantile(data$tenure,0.25),color='1st Q.'),
             linetype = "dashed", size = 0.6)+
  geom_vline(mapping=aes(xintercept = median(data$tenure),color='Median'),
             linetype = "dashed", size = 0.6)+
  geom_vline(mapping=aes(xintercept = quantile(data$tenure,0.75),color='3rd Q.'),
             linetype = "dashed", size = 0.6)+
  geom_vline(mapping=aes(xintercept = max(data$tenure,0.75),color='MAX'),
             linetype = "dashed", size = 0.6)+
  geom_vline(mapping=aes(xintercept = min(data$tenure,0.75),color='min'),
             linetype = "dashed", size = 0.6)+
  scale_colour_manual(name="Positions",values=color)+
  scale_x_continuous(breaks = seq(0,75,by=5))+
  xlab('Tenure')+
  ylab('Density')+
  ggtitle('Tenure distribution',subtitle = 'Mean and quantile division')


# We have min 1 month and max 72 month 
# We decide to divide "Tenure" into five groups 

data$tenure_groups = 0

data$tenure_groups[data$tenure>=1 & data$tenure<=12]="1y"
data$tenure_groups[data$tenure>12 & data$tenure<=24]="2y" 
data$tenure_groups[data$tenure>24 & data$tenure<=36]="3y"  
data$tenure_groups[data$tenure>36 & data$tenure<=48]="4y"
data$tenure_groups[data$tenure>48 ]="5y+"

summary.factor(data$tenure_groups)

str(data) #We see that "tenure_groups" is a "char", we want to change it into a "factor"

data$tenure_groups = as.factor(data$tenure_groups)

str(data$tenure_groups) ## check ok

##We remove the columns that we do not need for the EDA analysis 

data$customerID = NULL
data$tenure = NULL


## This is the dataset We will use from now on:

D_Churn=data #EDA DATASET

View(D_Churn)

#######################################################################################################
### EDA ###
#####################################################################################################

## Quantitative variables

Quant.var=sapply(D_Churn, is.numeric)

summary.factor(Quant.var)

corr.matrix=cor(D_Churn[,Quant.var])  ## Calculate the correlation matrix

corrplot(corr.matrix, type = "upper", order = "hclust", 
         tl.col = "black",tl.srt = 55, method = "number",
         main="\n\nCorrelation Plot for Quantitative Variables")


D_Churn$TotalCharges = NULL

View(D_Churn)


## Plotting the single variables distribution

Gender=ggplot(D_Churn, aes(x=gender)) + ggtitle("Gender") + xlab("Gender") +
  geom_bar(aes(y = (..count..)/sum(..count..)), width = 0.5, fill =c("deeppink3", "blue")) + 
  ylab("Percentage")  

Senior=ggplot(D_Churn, aes(x=SeniorCitizen)) + ggtitle("Senior") + xlab("Senior") +
  geom_bar(aes(y = (..count..)/sum(..count..)), width = 0.5, fill =c("darkred","darkgreen")) + 
  ylab("Percentage")

Partner=ggplot(D_Churn, aes(x=Partner)) + ggtitle("Partner") + xlab("Partner") +
  geom_bar(aes(y = (..count..)/sum(..count..)), width = 0.5, fill =c("darkred","darkgreen")) + 
  ylab("Percentage")


Dependents=ggplot(D_Churn, aes(x=Dependents)) + ggtitle("Dependents") + xlab("Dependents") +
  geom_bar(aes(y = (..count..)/sum(..count..)), width = 0.5, fill =c("darkred","darkgreen")) + 
  ylab("Percentage")

Phone=ggplot(D_Churn, aes(x=PhoneService)) + ggtitle("Phone Service") + xlab("Phone Service") +
  geom_bar(aes(y = (..count..)/sum(..count..)), width = 0.5, fill =c("darkred","darkgreen")) + 
  ylab("Percentage")

Multilines=ggplot(D_Churn, aes(x=MultipleLines)) + ggtitle("Multiple Lines") + xlab("Multiple Lines") +
  geom_bar(aes(y = (..count..)/sum(..count..)), width = 0.5, fill =c("darkred","darkgreen")) + 
  ylab("Percentage")

Internet_Service=ggplot(D_Churn, aes(x=InternetService)) + ggtitle("Internet Service") + xlab("Internet Service") +
  geom_bar(aes(y = (..count..)/sum(..count..)), width = 0.5, fill =c("darkgrey", "darkblue", "darkorchid4")) + 
  ylab("Percentage")

OnlineSec=ggplot(D_Churn, aes(x=OnlineSecurity)) + ggtitle("Online Security") + xlab("Online Security") +
  geom_bar(aes(y = (..count..)/sum(..count..)), width = 0.5, fill =c("darkred","darkgreen")) + 
  ylab("Percentage")


OnlineBackup=ggplot(D_Churn, aes(x=OnlineBackup)) + ggtitle("Online Backup") + xlab("Online Backup") +
  geom_bar(aes(y = (..count..)/sum(..count..)), width = 0.5, fill =c("darkred","darkgreen")) + 
  ylab("Percentage")

DeviceProtection=ggplot(D_Churn, aes(x=DeviceProtection)) + ggtitle("Device Protection") + xlab("Device Protection") +
  geom_bar(aes(y = (..count..)/sum(..count..)), width = 0.5, fill =c("darkred","darkgreen")) + 
  ylab("Percentage")

TechSupport=ggplot(D_Churn, aes(x=TechSupport)) + ggtitle("Tech Support") + xlab("Tech Support") +
  geom_bar(aes(y = (..count..)/sum(..count..)), width = 0.5, fill =c("darkred","darkgreen")) + 
  ylab("Percentage")

StreamingTV=ggplot(D_Churn, aes(x=StreamingTV)) + ggtitle("Streaming TV") + xlab("Streaming TV") +
  geom_bar(aes(y = (..count..)/sum(..count..)), width = 0.5, fill =c("darkred","darkgreen")) + 
  ylab("Percentage")

StreamingMovies=ggplot(D_Churn, aes(x=StreamingMovies)) + ggtitle("Streaming Movies") + xlab("Streaming Movies") +
  geom_bar(aes(y = (..count..)/sum(..count..)), width = 0.5, fill =c("darkred","darkgreen")) + 
  ylab("Percentage")

Contract=ggplot(D_Churn, aes(x=Contract)) + ggtitle("Contract") + xlab("Contract") +
  geom_bar(aes(y = (..count..)/sum(..count..)), width = 0.5, fill =c("darkgrey", "darkblue", "darkorchid4")) + 
  ylab("Percentage")

PaperlessBilling=ggplot(D_Churn, aes(x=PaperlessBilling)) + ggtitle("Paperless Billing") + xlab("Paperless Billing") +
  geom_bar(aes(y = (..count..)/sum(..count..)), width = 0.5, fill =c("darkred","darkgreen")) + 
  ylab("Percentage")


PaymentMethod=ggplot(D_Churn, aes(x=PaymentMethod)) + ggtitle("Payment Method") + xlab("Payment Method") +
  geom_bar(aes(y = (..count..)/sum(..count..)), width = 0.2, fill =c("darkgrey", "darkblue", "darkorchid4","darkorange3")) + 
  ylab("Percentage")


tenure_groups=ggplot(D_Churn, aes(x=tenure_groups)) + ggtitle("Tenure Groups") + xlab("Tenure Groups") +
  geom_bar(aes(y = (..count..)/sum(..count..)), width = 0.2, fill =c("darkgrey", "darkblue", "darkorchid4","darkorange3","indianred4")) + 
  ylab("Percentage")

## Now we show the results 

grid.arrange(Gender,Senior,Partner,Dependents)
grid.arrange(Multilines,DeviceProtection,Phone,Internet_Service)
grid.arrange(OnlineBackup,OnlineSec,TechSupport,StreamingTV)
grid.arrange(StreamingMovies,StreamingTV)
grid.arrange(Contract,PaymentMethod,PaperlessBilling, ncol=3)
grid.arrange(PaymentMethod,tenure_groups,ncol=3)


ggplot(D_Churn)+
  geom_boxplot(aes(y = MonthlyCharges),outlier.colour="black", outlier.shape=16,
               outlier.size=2, notch=FALSE,colour="black",fill="azure")

## Now we plot the dependent variable

Churn=ggplot(D_Churn, aes(x=Churn)) + ggtitle("Churn") + xlab("Churn") +
  geom_bar(aes(y = (..count..)/sum(..count..)), width = 0.5, fill =c("darkred","darkgreen")) + 
  ylab("Percentage")  

Churn

table(data$Churn)/nrow(data) 

## As we can see the dataset is quite unbalanced towards "Churn = No"
## Is not heavily unbalanced but it could and should be implemented 
## before we start modelling 

## We use the previous version of the variable "Tenure" to see how this variable behaves in months
## w.r.t Churn 

ggplot(data=data,aes(tenure_initial, fill = Churn))+
  geom_bar(size = 1)+
  labs(x="Tenure in month") 


## The distribution for tenure is very different between customers who churned and who didn't churn. 

pl1=ggplot(D_Churn) +
  geom_bar(aes(x = gender, fill = Churn),position = "fill", stat = "count", 
           show.legend = T)+
  labs(x="Churn VS Gender",y="Percentage")

pl2=ggplot(D_Churn) +
  geom_bar(aes(x = SeniorCitizen, fill = Churn), position = "fill", stat = "count", 
           show.legend = T)+
  labs(x="Churn VS Senior-citizen",y="Percentage")


pl3=ggplot(D_Churn) +
  geom_bar(aes(x = Partner, fill = Churn), position = "fill", stat = "count", 
           show.legend = T)+
  labs(x="Churn VS Partner",y="Percentage")

pl4=ggplot(D_Churn) +
  geom_bar(aes(x = Dependents, fill = Churn), position = "fill", stat = "count", 
           show.legend = T) +
  labs(x="Churn VS Dependents",y="Percentage")

pl5=ggplot(D_Churn) +
  geom_bar(aes(x = PhoneService, fill = Churn), position = "fill", stat = "count", 
           show.legend = T)+
  labs(x="Churn VS Phone Service",y="Percentage")

pl6=ggplot(D_Churn) +
  geom_bar(aes(x = InternetService, fill = Churn), position = "fill", stat = "count", 
           show.legend = T)+
  labs(x="Churn VS Internet Service",y="Percentage")


grid.arrange(pl1,pl2,pl3,pl4,pl5,pl6, ncol = 2, nrow = 3)

pl7=ggplot(D_Churn)+
  geom_boxplot(aes(y = MonthlyCharges, fill = Churn),outlier.colour="black",outlier.shape=16,
               outlier.size=2, notch=FALSE)
pl7

#####################################################################################################
### SPLITTING IN TRAIN AND TEST SET ####
#####################################################################################################
set.seed(2020)
intrain= createDataPartition(D_Churn$Churn,p=0.8,list=FALSE)
C_train= D_Churn[intrain,]
C_test= D_Churn[-intrain,]

dim(C_train); dim(C_test) ## To confirm the right dimentions of the partition

table(C_train$Churn)  ### Look at the imbalance

barplot(table(C_train$Churn),col=c("darkred","darkgreen"),main="Churn Train Dataset")


C_train$gender=as.factor(C_train$gender)
C_train$SeniorCitizen=as.factor(C_train$SeniorCitizen)
C_train$Partner=as.factor(C_train$Partner)
C_train$Dependents=as.factor(C_train$Dependents)
C_train$PhoneService=as.factor(C_train$PhoneService)
C_train$InternetService=as.factor(C_train$InternetService)
C_train$Contract=as.factor(C_train$Contract)
C_train$PaperlessBilling=as.factor(C_train$PaperlessBilling)
C_train$PaymentMethod=as.factor(C_train$PaymentMethod)
C_train$Churn=as.factor(C_train$Churn)

C_test$gender=as.factor(C_test$gender)
C_test$SeniorCitizen=as.factor(C_test$SeniorCitizen)
C_test$Partner=as.factor(C_test$Partner)
C_test$Dependents=as.factor(C_test$Dependents)
C_test$PhoneService=as.factor(C_test$PhoneService)
C_test$InternetService=as.factor(C_test$InternetService)
C_test$Contract=as.factor(C_test$Contract)
C_test$PaperlessBilling=as.factor(C_test$PaperlessBilling)
C_test$PaymentMethod=as.factor(C_test$PaymentMethod)

C_test$Churn=as.factor(C_test$Churn)

str(C_train)
str(C_test)

# generate new balanced data by ROSE
model.train=ROSE(Churn ~ ., data=C_train, seed=123)$data 
model.test=C_test

model.complete=rbind(model.train,model.test)

# check balance of new data

table(model.train$Churn) 

barplot(table(model.train$Churn), main="Churn balanced",col=c("darkred","darkgreen"))


###############################
######LOGISTIC REGRESSION######
###############################

library(ROCR)

LogModel=glm(Churn ~ .,family=binomial,data=model.train)

print(summary(LogModel))

#prediction
pred.log.train = predict(LogModel, newdata=model.train,
                         type="response")
summary(pred.log.train)

#Thresholding:
# We can convert the probabilities to predictions using what's called a threshold value, t. 
# If the probability of ChurnNo is greater than this threshold value, t, we predict ChurnNo. 
# But if the probability of ChurnNo is less than the threshold value, t, then we predict ChurnYes. 

# Confusion matrix for threshold of 0.5,0.2 and 0.7
table(model.train$Churn, pred.log.train > 0.2)
table(model.train$Churn, pred.log.train > 0.5)
table(model.train$Churn, pred.log.train > 0.7)

#Picking a good threshold value is difficult, to do that we use roc curve

#Recall that we made predictions on our training set and called them
#We'll use these predictions to create our ROC curve.

ROCRpred = prediction(pred.log.train, model.train$Churn)

#The first is the predictions we made with our model, which we called pred.log.train.
#The second argument is the true outcomes of our data points,
#which in our case, model.train$Churn.

# Performance function
ROCRperf = performance(ROCRpred, "tpr", "fpr")

# plot the output of the performance function 
# Plot ROC curve
plot(ROCRperf)
# Add colors
plot(ROCRperf, colorize=TRUE)
# Add threshold labels 
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

#Given the graph we would choose between 0.5 and 0.6

# Prediction on Test Set
# we used a threshold value of 0.6 and we obtain the 
# following confusion matrix, on the test.set

log.pred.test = predict(LogModel, type = "response", newdata = model.test)

fitted.results=ifelse(log.pred.test> 0.59,"Yes","No")
fitted.results=as.factor(fitted.results)

require(caret) 

cm=confusionMatrix(data=fitted.results, 
                   reference=model.test$Churn)


cm #confusion matrix with all statistics
accuracy=round(cm$overall[1],2) 

Log_acc = accuracy

Log_acc

#########################
######DECISION TREE######
#########################


#For the trees we will use three different techniques:
#1. Validation set approach
#2. Leave-one-out cross validation (LOOCV)
#3. k-fold cross validation

library(rpart.plot)
library(RColorBrewer)
library(tree)
library(rattle)


### TREE 1: CON LOOCV
##  It is good to keep in mind that this technique 
##  is more likely to overfit when compared to the k-fold 

set.seed(2020)
fit=tree(Churn~.,data=model.complete,method = 'gini')

summary(fit)
plot(fit)
text(fit, pretty=0)

#In the case of a classification tree, the argument
#type="class" instructs R to return the actual class prediction.
set.seed(2020)
cv.fit=cv.tree(fit, FUN=prune.misclass,K=length(model.complete)) 

# Fixing K equal to the total of the observations
# we are able to implement the LOOCV

cv.fit

ggplot(mapping=aes(x=cv.fit$size,y=cv.fit$dev))+
  geom_line(linetype='dashed')+
  geom_point()+
  geom_point(mapping=aes(x=4,y=cv.fit$dev[which(cv.fit$size==4)],colour='4 leafs'),shape='X',size=6)+
  scale_x_continuous(breaks = c(cv.fit$size))+
  labs(x='Size',y='Dev',colour='Model selected:')+
  theme(legend.position = c(0.8,0.8),legend.text = element_text(size=10))


prune.fit=prune.misclass(fit,best=4) 
summary(prune.fit)


plot(prune.fit)
text(prune.fit, pretty=0)

fitted=predict(prune.fit, newdata=model.complete,type='class')
mean(fitted==model.complete$Churn)
table(fitted,model.complete$Churn)


tree_l = confusionMatrix(data=fitted, 
                         reference=model.complete$Churn)
tree_l_acc = tree_l$overall[1]

## The model has an accuracy better than a random decision, we also are aware that
## the tree is not the best model available to us, so it is useful to use it as a benchmark model

##########################################################
##### DECISION TREE 2  K FOLD (K=10) ON whole DATASET#####
##########################################################


set.seed(2020)
model_T=train(
  Churn ~., data = model.train, method = "rpart",
  trControl = trainControl("cv", number = 10)
)

# Best tuning parameter
model_T$bestTune

# Make predictions on the test data
predicted.classes_T = model_T %>% predict(model.test)

#compute model prediction accuracy rate
Tree_k = confusionMatrix(data=predicted.classes_T, 
                         reference=model.test$Churn)

Tree_k_acc = Tree_k$overall[1]

plot(model_T)

fancyRpartPlot(model_T$finalModel)

####################################################
##### DECISION TREE 3   VALIDATION SET APPROACH#####
####################################################
set.seed(2020)
mtree = rpart(Churn~., data = model.train, method="class", control = rpart.control(minsplit = 20, minbucket = 7, maxdepth = 10, usesurrogate = 2, xval =10 ))
mtree

# view
prp(mtree, faclen = 0, cex = 0.8, extra = 1)

# pruning:
printcp(mtree)
bestcp=mtree$cptable[which.min(mtree$cptable[,"xerror"]),"CP"]

# Prune the tree using the best cp.
pruned= prune(mtree, cp = bestcp)

# Plot pruned tree
prp(pruned, faclen = 0, cex = 0.8, extra = 1)

# confusion matrix (training data)
conf.matrix = table(model.train$Churn, predict(pruned,type="class"))
rownames(conf.matrix) = paste("Actual", rownames(conf.matrix), sep = ":")
colnames(conf.matrix) = paste("Pred", colnames(conf.matrix), sep = ":")
print(conf.matrix)

#Scoring
val1 = predict(pruned, model.test, type = "prob")
#Storing Model Performance Scores
pred_val=prediction(val1[,2],model.test$Churn)

# Calculating Area under Curve  
perf_val=performance(pred_val,"auc")
perf_val

## The random model is defined by a bisector, the area subtended by my model 
## is greater than the area subtended from the bisector and 
## therefore clearly superior to the random model

# Plotting Lift curve 
## A lift curve is a way of visualizing the performance of a classification model
## and it shows shows the ratio of a model to a random guess

plot(performance(pred_val, measure="lift", x.measure="rpp"), colorize=TRUE)

# Calculating True Positive and False Positive Rate
perf_val =performance(pred_val, "tpr", "fpr")
perf_val

# Plot the ROC curve
plot(perf_val, col = "red", lwd = 1.5)

#pruned tree plot
prp(pruned, faclen = 0, cex = 0.8, extra = 1,main="Tree")

# Accuracy on test set

Tc = confusionMatrix(data=predict(pruned, newdata = model.test,  type="class"), 
                     reference=model.test$Churn)

Tree_acc = Tc$overall[1]

####################################
##BAGGING TREES AND RANDOM FOREST ##
####################################

# The fundamental difference is that in Random forests
# only a subset of features are selected at random out of the total 
# and the best split feature from the subset is used to split each node in a tree, 
# unlike in bagging where all features are considered for splitting a node.

library(randomForest)

#Bagging is a special case of a random forest where the
# randomForest () function from the randomForest library can
#be used to build a bagged model or a random forest.

set.seed(2020)
bag=randomForest(Churn~.,data=model.train,
                 mtry=18,
                 importance=TRUE,
                 ntree=200)
# mtry=18: 18 predictors
# ntree=200: number of bootstrap samples

print(bag)

## RANDOM FOREST ##

# define training control
train_control = trainControl(method="cv", number=10)

# train the model
tunegrid = expand.grid(.mtry=c(8,7,12,15))

model = train(Churn~., data=model.train, trControl=train_control,method = "rf",tuneGrid = tunegrid)

# summarize results
print(model)

## We see, via cross validation  the best mtry
## We have shown only a subsaple of all the cross validation trials 
## so to not make the scrip too computationally heavy

set.seed(2020)
rf=randomForest(Churn~.,data=model.train,
                mtry=8, #using the best mtry
                importance=TRUE,
                ntree=200)
rf

#for classification, the purity of a knot
# is measured with the gini index

varImpPlot(rf,pch=19,cex=1.5,col="black",lwd=2, main="Random Forrest")

#prediction for random forests

rf.predict=predict(rf,model.test)
rf.predict
model.test$pred.Churn=predict(rf,model.test) 

Rfc = confusionMatrix(data=rf.predict, 
                      reference=model.test$Churn)

Rfc_acc = Rfc$overall[1]


## We create a new column so to make the comparison
## between the predicted and the original values

Churn.pred = data.frame(Original=model.test$Churn,Pred=model.test$pred.Churn) 

## Other variables can be added to the data frame 
## depending on the extra analysis one may wish to perform

View(Churn.pred)


###############
####XGBOOST####
###############

#This is an alternative method, which is similar to the bagging method, 
#except that the trees are grown sequentially.

#tidyverse for easy data manipulation and visualization
#caret for easy machine learning workflow
#xgboost for computing boosting algorithm

library(tidyverse)
library(caret)
library(xgboost)

#We'll use the caret workflow,
#to automatically adjust the model parameter values, and fit the final best boosted tree 
#that explains the best our data.

set.seed(2020)
model=train(
  Churn ~., data = model.train, method = "xgbTree",
  trControl = trainControl("cv", number = 10)
)

# Best tuning parameter
model$bestTune

# Make predictions on the test data
predicted.classes = model %>% predict(model.test)
head(predicted.classes)

#The function varImp() displays the importance of variables in percentage:
Vi = varImp(model)

plot(Vi)

## Accuracy
# mean(predicted.classes == model.test$Churn)

Xgb = confusionMatrix(data=predicted.classes, 
                      reference=model.test$Churn)

Xgb_acc = Xgb$overall[1]


############################################
# Final DataFrame with models and accuracy #
############################################


Accuracy = data.frame("Logistic" = Log_acc,"Tree LOOCV"= tree_l_acc ,"Tree k-fold CV"= Tree_k_acc, "Tree Train-Test" = Tree_acc, "RandomForest" = Rfc_acc, "Xgboost" = Xgb_acc,
                row.names = "Accuracy")

print(Accuracy)
