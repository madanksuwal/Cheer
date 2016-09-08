### ModelMap for Ananta
# Set working directory, where all files must be stored. 
#setwd("/Users/madanksuwal/Desktop/From Prakash/Data")  # for MAC
setwd("D:/Ananta Pradhan_landslide/test continous Y with absence") #WIndows
# check what files are there
dir()  

# Import file, it should have extracted values of all predictor variables.
read.csv("PA combined.csv")->slide.df    # Binary Presence file import
#read.csv("continous_presence.csv")->slide.df    # Presence file import
dim(slide.df)
head(slide.df)
# check prevalence 
table(slide.df$PA)           # check presence and pseudoabsence ratio
names(slide.df)

====================================
### partition train and test for presennce 
set.seed(4437)
sample.idP <- sample (2, nrow (slide.df), replace =TRUE, prob = c(0.7, 0.3))
Train <- slide.df [sample.idP == 1, ]
Test <- slide.df [sample.idP == 2, ]
table (Train$PA)
table (Test$PA)


library(randomForest)
Model.fit<-randomForest(x=Train[5:22], y=as.factor (Train$PA), ntree=2000,  importance = TRUE)
print(Model.fit)
plot(Model.fit)

================================
# Variable Importance plot
varImpPlot(Model.fit, sort = TRUE, main = "Variable Importance", n.var=10)
# Variable Importance Table
var.imp<- data.frame (importance(Model.fit, type= 2))
var.imp$Variables <- row.names(var.imp)
var.imp[order (var.imp$MeanDecreaseGini, decreasing = TRUE), ]->ImpVarList
print(ImpVarList)

===================================
# Partial plot
xv<-c(elev, slope, twi)
partialPlot(Model.fit, pred.data = Train, x.var=slope)
imp<-importance(Model.fit)
impvar<-rownames(imp)[order(imp[,1], decreasing = T)]

x11()
op <- par(mfrow=c(6, 3))
  for (i in seq_along(impvar)) {
  partialPlot(Model.fit, Train, impvar[i], xlab=impvar[i],
              main=paste("Partial Dependence on", impvar[i]),
              ylim=c(0, 3))
}
par(op)


#Export the response curve to TIFF format in working directory
tiff(filename = "Response plot.tiff", width = 4000, height = 2000, compression = "lzw", res=200)
op <- par(mfrow=c(3, 6))  # this must be defined as per number of predictor variables
for (i in seq_along(impvar)) {
  partialPlot(Model.fit, Train, impvar[i], xlab=impvar[i],
              main=paste("Partial Dependence on", impvar[i]),
              ylim=c(0.5, 3.5))  # change the value as per need
          }
par(op)
dev.off()

=============================
# To plot AUC Curve 
library(ROCR)
a.v<-as.vector(Model.fit$votes[,2]) # extract predicted '1'
perf.obj<- prediction( predictions = a.v, labels = Train$PA  )

# Calculate AUC
rh.AUC <- performance(perf.obj, "auc")
AUC=rh.AUC@y.values[[1]]
AUC
# Plot ROC 
rh.ROC <- performance(perf.obj, 'tpr', 'fpr')

# Export ROC plot and things to NOTE
tiff(filename = "ROC curve.tiff", width = 2000, height = 1500, compression = "lzw", res=200)
plot(rh.ROC, main="ROC Plot", xlab=" 1 - Specificity: False Positive Rate", 
     ylab="Sensitivity: True Positive Rate", lwd=2)
abline(a=0,b=1, lty=3, lwd=2)  # diagonal line
AUC<-round(AUC,3)
text(0.85,0.0, labels = paste ("Average AUC:", AUC))
dev.off()

======================================

# Use lattice file here, I just gave Test file as an example
names(Test)
Test.p<-Test[,c("Long", "Lat")]
# Predict to lattice file
Test.p$Prediction<- predict (Model.fit, newdata= Test, type="prob")[,2]  

# Export the predicted file
write.csv(Test.p, file ="Predicted file.csv")

