## RandomForest
#### New data set
pre.df<-read.csv("W:/Rhododendron data/Rh_3_spp_Presence_Elev_2500_to_4200.csv")
abs.df<-read.csv("W:/Rhododendron data/Rhodendron_absence_900_to_5000.csv")
pseu.df<-read.csv("W:/Rhododendron data/Random_pseudoabsence_elev_1000_to_5500.csv")
latt.df<-read.csv("W:/Rhododendron data/lattice3km_current.csv")
latt70s.df<-read.csv("W:/Rhododendron data/Lattice3km 2070s.csv")

setwd("W:/Rhododendron data/R outputs")

# These data set lacks Presence/Absence column, i.e. 1/0 
# Add with 1/0 column
pre.df$PA<-1 ; head(pre.df)
abs.df$PA<-0 ; head(pre.df)
pseu.df$PA<-0 ; head(pseu.df)

#check data sets, remove extra columns.
head(pre.df)
pre.df<-pre.df[,-4]
head(abs.df)
abs.df<-abs.df[,-4]
head(pseu.df)
pseu.df<-pseu.df[,-4]
head(latt.df)
head(latt70s.df)
dim(pre.df); dim(pseu.df); dim(abs.df)

# Extarct presence points by SPECIES
pre.lepi<-subset(pre.df, species=="lepidotum" )
pre.lown<-subset(pre.df, species=="lowndesii" )
pre.cown<-subset(pre.df, species=="cowanianum" )

=============================
  # FILE TO WORK ON
  PFi  = pre.lepi  # presence file
AFi = abs.df    # Absence file

#### Change peseudo-absence #### 
### partition train and test for presennce 
set.seed(4437)
sample.idP <- sample (2, nrow (PFi), replace =TRUE, prob = c(0.7, 0.3))
rh.trainP <- PFi [sample.idP == 1, ]
rh.testP <- PFi [sample.idP == 2, ]

### partition train and test for pseudoabsence 
set.seed(4436)
sample.idA <- sample (2, nrow (AFi), replace =TRUE, prob = c(0.7, 0.3))
rh.trainA <- AFi [sample.idA == 1, ]
rh.testA <- AFi [sample.idA == 2, ]

### bind train and test data
rbind(rh.trainP, rh.trainA)-> rh.train
rbind(rh.testP, rh.testA)-> rh.test
rbind(rh.train, rh.test)->rh.df
dim(rh.df)

# Training data set
table(rh.train$PA)

# Test data set
table(rh.test$PA)

# Selecting variables
varTrain<- names(rh.train)
print(varTrain)
# Exclude non-predictor variables
varTrain <- varTrain [!varTrain %in% c("lat", "long", "species",   "PA")]
print (varTrain)

# add "+" sign between predictor variables 
varTrain1 <- paste(varTrain, collapse = "+")
print (varTrain1)

# Add response variables and convert to a formula object
rf.formula<- as.formula(paste (as.factor ("PA"), varTrain1, sep= "~"))
print (rf.formula)

==================================
  # Load library
  library(randomForest)
# IMPUTE to deal with missing values
set.seed(414)
rh.trainIM<-rfImpute(rf.formula, data=rh.train, ntree = 2000)
head(rh.trainIM)

# Run RandomForest with above formula and IMPUTED data
rh.rf<-randomForest(x=rh.trainIM[2:25], y=as.factor (rh.trainIM$PA), ntree=2000,  importance = TRUE)
print(rh.rf)
plot(rh.rf)

# 2000 decision trees or a forest has been built using the Random Forest 
# algorithm based learning. We can plot the error rate across decision trees. 
# The plot seems to indicate that after 500 decision trees, there is not a 
# significant reduction in error rate

# Variable Importance plot
varImpPlot(rh.rf, sort = TRUE, main = "Variable Importance", n.var=10)
# Variable Importance Table
var.imp<- data.frame (importance(rh.rf, type= 2))
var.imp$Variables <- row.names(var.imp)
var.imp[order (var.imp$MeanDecreaseGini, decreasing = TRUE), ]->ImpVarList
print(ImpVarList)

# To measure the accuracy of the Random Forest model. 
# Some of the other model performance statistics are KS, Lift Chart and ROC Curve

# Predicting response variables
rh.train$predicted.resp<- predict(rh.rf, rh.trainIM)

# Confusion matrix
# confusionMatrix function from "caret" package can be used for creating 
# confusion matrix based on actual response variable and predicted value.
library(e1071)
library(caret)
confusionMatrix(data= rh.train$predicted.resp, reference= rh.train$PA, positive = '1')->confu.train
confu.train
# The "positive" represents true case such as 1 for 1/0, Yes for Yes/No, Ture for T/F
# It has accuracy of 99.18%, which is fantastic. 
# Now we can predict response for the validation sample and 
# calculate model accuracy for the sample.

# Predicting to test data set
# Predicting response variable
set.seed(414)
rh.testIM<-rfImpute(rf.formula, data=rh.test, ntree = 2000)
head(rh.testIM)
rh.test$predicted.resp<- predict(rh.rf, rh.testIM, OOB=TRUE, type="response")

# Create confusion matrix
confusionMatrix(data=rh.test$predicted.resp, reference = rh.test$PA, positive = '1')->confu.test
confu.test



==================================
  # Response curves of top variables
  library(ggplot2)
as.vector(ImpVarList[1:10,2])->Top10Var  # Top 10 variables from RF imp list

pPA<-predict(rh.rf, rh.testIM, 'vote')[,2]
plotData<-lapply (Top10Var, function(x) {
  out<-data.frame(
    var=x,
    type=c(rep('Actuall', nrow(rh.testIM)), rep('Predicted', nrow (rh.testIM))),
    value=c(rh.testIM[,x], rh.testIM[,x]),
    PA=c(as.numeric(rh.testIM$PA), pPA)
  )
  out$value<-out$value - min(out$value) #Normalize to [0,1]
  out$value<-out$value/max (out$value)
  out
})
plotData<-do.call(rbind, plotData)
# Export Response curves
tiff(filename = "Response Curves RF top 10 var.tiff", 
     width = 5000, height = 1500, compression = "lzw", res=250)
qplot(value, PA, data=plotData, facets=type ~var, geom='smooth', span=0.5)+ theme_bw() 
dev.off()

========================================
  
  #### K-fold Corss Validation in RandomForest ####
#### To partition the first fold
k=5
n= floor(nrow(rh.df)/k) # n = size of each fold, and value is rounded by "floor" 
err.vect= rep (NA, k)   # store the error in this vector

i = 1
s1 = ((i-1) * n+1) # the start of the subset
s2 = (i*n)         # The end of the subset
subset = s1:s2     # the range of the subset

##### Because of round above, the end of the subset may be slightly out of range

##### Next, move to the second fold:
###### i = 2
###### .......
###### But we automate this by looping

##### Load library
library(randomForest)
##### For error test
library(verification)
##### Need to loop over each of the K- folds
###### The input y-variables must be numeric to calculate the roc.area error, which RF needs in vector
#store output probability in

OutputPre<-PFi[,c(1,2,3)]       # Extract Long/lat and Species colum
PFi.IM<-rfImpute(rf.formula, data=PFi[,-c(1,2,3)], ntree = 2000)
#### change file if error in above line ####
#PFi.IM<-PFi[,-c(1,2,3)]

##### Predict to lattice, extracct probability and True/False
head(latt.df)
latt.df$PA <- 0                  # Add PA colunm to fill NAs
latt.p<-latt.df[,c(2,3)]         # Extact Long/Lat only
# IMPUTE to deal with missing values
set.seed(414)
#LattIM.df<-rfImpute(rf.formula, data=latt.df, ntree = 1000)
#write.csv(LattIM.df, file="LattIM.csv")
read.csv("LattIM.csv")->LattIM.df
LattIM.df<-LattIM.df[,-1]


head(latt70s.df)
latt70s.df$PA <- 0              # Add PA colunm to fill NAs
latt70s.p<-latt70s.df[,c(2,3)]  # Extact Long/Lat only
set.seed(414)
#LattIM70s.df<-rfImpute(rf.formula, data=latt70s.df, ntree = 1000)
#write.csv(LattIM70s.df, file="LattIM70s.csv")
read.csv("LattIM70s.csv")->LattIM70s.df
LattIM70s.df<-LattIM70s.df[,-1]

================
  ## RandomForest Looping for 5 times
  for (i in 1:k){
    s1 = ((i-1) * n+1) # the start of the subset
    s2 = (i*n)         # The end of the subset
    subset = s1:s2     # the range of the subset
    
    cv.train = rh.trainIM # Train the model using this data
    cv.test = rh.testIM # test the model's performace on this data
    
    # run the random forest on the training data set, exclude on predictor variables
    fit = randomForest (x=cv.train[,-c(1)], y= as.factor (cv.train[,1]), ntree=2000)
    prediction = predict (fit, newdata= cv.test[, -c(1)], type="prob")[,2]
    # Predict to species data for threshold
    OutputPre[3+i]<- predict (fit, newdata= PFi.IM, type="prob")[,2]
    # Predict to Current climate lattice 
    latt.p[2+i]<- predict (fit, newdata=LattIM.df, type= "prob" ) [,2]
    # Predict to Future climate lattice
    latt70s.p[2+i]<- predict (fit, newdata=LattIM70s.df, type= "prob" ) [,2]
    
    # calculate the model's accurancy for the ith fold
    err.vect[i] <-   roc.area (cv.test[ , 1], prediction)$A
    #print(paste("AUC for fold", i, ":", err.vect[i]))
  }

#### Mean error
print(data.frame(err.vect)) # list of AUCs from k fold
print(paste("Average AUC:", mean(err.vect)))

#mean of 5 probabilities
head(OutputPre)
OutputPre$AvgProb<- rowMeans(OutputPre[,4:8])
quantile(OutputPre$AvgProb, c(0.05, 0.10))
# rename columns 
library(data.table)
setnames(OutputPre, old=c(4,5,6,7,8), new=c("prob1", "prob2", "prob3", "prob4", "prob5" ))

head(latt.p)
latt.p$AvgProb<- rowMeans(latt.p[,3:7])
setnames(latt.p, old=c(3,4,5,6,7), new=c("prob1", "prob2", "prob3", "prob4", "prob5" ))

head(latt70s.p)
latt70s.p$AvgProb<- rowMeans(latt70s.p[,3:7])
setnames(latt70s.p, old=c(3,4,5,6,7), new=c("prob1", "prob2", "prob3", "prob4", "prob5" ))

===========================
  
  # Creating performance object
  library(ROCR)
a.v<-as.vector(rh.rf$votes[,2]) # extract predicted '1'
perf.obj<- prediction( predictions = a.v, labels = rh.train$PA  )

# Calculate AUC
rh.AUC <- performance(perf.obj, "auc")
AUC=rh.AUC@y.values[[1]]; AUC
# Plot ROC 
rh.ROC <- performance(perf.obj, 'tpr', 'fpr')

======================================
  # Export ROC plot and things to NOTE
  tiff(filename = "ROC curve_ Lepidotum with Rhodo Pseudo.tiff", 
       width = 2000, height = 1500, compression = "lzw", res=200)
plot(rh.ROC, main="ROC Plot", xlab=" 1 - Specificity: False Positive Rate", 
     ylab="Sensitivity: True Positive Rate", lwd=2)
abline(a=0,b=1, lty=3, lwd=2)  # diagonal line
text(0.85,0.0, labels = paste("Average AUC:",round( mean(err.vect),3)))
dev.off()

tiff(filename = "Model plot.tiff", 
     width = 2000, height = 1500, compression = "lzw", res=200)
plot(rh.rf, col="black")
legend(1200, 0.4, lty=c(3, 1, 2), bty="n", legend=c("Presence", "OOB", "Pseudoabsence" ))
dev.off()

#Export Variable Importance, 
write.csv(ImpVarList, file ="Lepidotum with Rhodo Pseudo_ Var Imp list.csv")
#Export predicted file    
write.csv(latt.p, file = "Lepidotum with Random Rhodo Current Climate.csv")
write.csv(latt70s.p, file = "Lepidotum with Random Rhodo Future 2070s.csv")
# Export ACU value
write.csv(err.vect, file="AUC from K-fold.csv")

print(paste("Average AUC:", mean(err.vect)))

rh.rf
confu.train
confu.test
print(paste("Average AUC:", mean(err.vect)))
quantile(OutputPre$AvgProb, c(0.05, 0.10))
