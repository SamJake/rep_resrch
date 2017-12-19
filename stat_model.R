library(kernlab)
library(boot)
data(spam)

set.seed(3435)
trainInd <- rbinom(4601,size = 1,prob=0.5)
table(trainInd)

trainSpam <- spam[trainInd==1,]
testSpam <- spam[!trainInd==0,]

plot(trainSpam$capitalAve ~ trainSpam$type)
plot(log10(trainSpam$capitalAve+1)~trainSpam$type)
plot(log10(trainSpam[,1:4]))

hcl <- hclust(dist(t(trainSpam[,1:57])))
plot(hcl)


trainSpam$numType <- as.numeric(trainSpam$type)-1
costF <- function(x,y){sum(x!=(y>0.5))}
cvError <- rep(NA,55)


for(i in 1:55)
{
  lmFormula <- reformulate(names(trainSpam)[i],response = "numType")
  glmFit <- glm(lmFormula,family = binomial,data = trainSpam)
  cvError[i] <- cv.glm(trainSpam,glmFit,costF,2)$delta[2]
}



predictionModel <- glm(numType~charDollar,family = binomial,data=trainSpam)
predictionTest <- predict(predictionModel,testSpam)
predictedSpam <- rep("nonspam",dim(testSpam)[1])
predictedSpam[predictionModel$fitted > 0.5] <- "spam"


table(predictedSpam,testSpam$type)
