ER_test_1 <- 0

sigmoid <- function(y){
  sig<-1/(1+exp(-y))
  return(sig)
}

############################################

logistic_regr <- function(X,Y,iteration,n){
  
  for(i in 1:iteration){
    
    
    z <- lambda_d%*%t(X)
    z<- z + lambda_not[1]
    
    #Sigmoid Function calculation
    Fn <- sigmoid(z)
    
    #Likelihood function
    ones <- which(Y==1)
    zeroes <- which(Y==0)
    L1 <- -1 * sum(log(Fn[ones]))
    L2 <- -1 * sum(log(1-Fn[zeroes]))
    L <- L1+L2
    
    #Gradient Descent
    cn = (Y-(1-Y))/((Y*Fn)+((1-Y)*(1-Fn)))
    delFn = Fn*(1-Fn)
    grad = -1*cn*delFn
    
    #New Values of Lambda 
    adjust_lambda_X<- grad%*%X
    adjust_lambda_not <- grad%*%matrix(1,nrow(X),1)
    
    #Adjusting values
    lambda_d <- lambda_d-(n)*(adjust_lambda_X)
    lambda_not <- lambda_not-(n)*(adjust_lambda_not)
    
  }
  x<-lambda_d
  y<-lambda_not
  out_dict <- list("Function"=Fn,"Weights" = x , "Lambda_not"=y)
  return(out_dict)
}

#######################################

setwd("E:/RHUL/Data Analysis")

Auto <- read.table("Auto.data.txt", header=T, na.strings="?")
Auto1 <- Auto
fix(Auto)
Auto <- na.omit(Auto)
names(Auto)
Auto$high[Auto$mpg>=23] <- 1
Auto$high[Auto$mpg<23] <- 0

Auto$origin1 <- 0
Auto$origin2 <- 0
Auto$origin1[Auto$origin==1]<-1
Auto$origin2[Auto$origin==2]<-1

#auto2 <- data.frame(horsepower= Auto$horsepower,weight = Auto$weight,origin1 = Auto$origin1,origin2 = Auto$origin2)
#auto2 <- scale(auto2)
Auto$horsepower <- scale(Auto$horsepower)
Auto$weight <- scale(Auto$weight)
Auto$origin1 <- scale(Auto$origin1)
Auto$origin2 <- scale(Auto$origin2)
Auto$year <- scale(Auto$year)
set.seed(311)
train <- sample(1:nrow(Auto), nrow(Auto)/2)
train.X <- cbind(Auto$horsepower,Auto$weight,Auto$year,Auto$origin1,Auto$origin2)[train,]
test.X <-  cbind(Auto$horsepower,Auto$weight,Auto$year,Auto$origin1,Auto$origin2)[-train,]
train.Y <- cbind(Auto$high)[train,]
test.Y <- cbind(Auto$high)[-train,]

#########################################################

for(s in 1:100){
  
  lambda_d <- runif(ncol(train.X),-0.7,0.7)
  lambda_not <- runif(1,-0.7,0.7)
  
  Logistic_regr_out <- logistic_regr(train.X,train.Y,1000,0.001)
  lambda_not_updated <- Logistic_regr_out$Lambda_not
  lambda_updated <- Logistic_regr_out$Weights
  
  F_test <- lambda_updated %*% t(test.X) + lambda_not_updated[1]

  Y_pred_test <- sigmoid(F_test)
  Y_pred_test[Y_pred_test>=0.5]<-1
  Y_pred_test[Y_pred_test<0.5]<-0

  ER_test_1 <- append(ER_test_1,(1/196*(sum((test.Y - Y_pred_test)**2))))
  
  
}

boxplot(ER_test_1[2:100])
title("Plot of Test Errors for different initial weights")
print(ER_test_1[2:100])








