library(boot)

setwd("C:/Users/SamJacobJulian/Desktop/Rstd/rep_resrch")
bank <- read.csv(file = "./Data/bank.csv",header = TRUE)

train <- bank[1:70,]
test <- bank[71:nrow(bank),]
train$Customer_Select <- as.factor(train$Customer_Select)
plot(log10(train$Credit_Score) ~ train$Customer_Select)
plot(train[,c(2,3,4,5)])

train$nSelect <- as.numeric(train$Customer_Select)
train$logCredit <- log10(train$Credit_Score)

hc <- hclust(dist(t(train[,c(2:3,5,7:8)])))

cost_fn <- function(x,y){sum(x!=(y>0.5))}
cvErr <- rep(NA,7)

for(i in 2:8)
{
  lmForm <- reformulate(names(train)[i],response = "nSelect")
  glmFitt <- glm(lmForm,family=binomial,data = train)
  cvErr[i-1] <- cv.glm(train,glmFitt,cost_fn,2)$delta[2]
}


