obj_func <-0
###########################################
percentage_diff <- function(m,n){
  
  temp = ((n-m)/n)*100
  return(temp)
}

###########################################


sigmoid <- function(y){
  sig<-1/(1+exp(-y))
  return(sig)
}

############################################

logistic_regr <- function(X,Y,iteration,n,lambda_d,lambda_not){
  
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
set.seed(1403)
train <- sample(1:nrow(Auto), nrow(Auto)/2)
train.X <- cbind(Auto$horsepower,Auto$weight,Auto$year,Auto$origin1,Auto$origin2)[train,]
test.X <-  cbind(Auto$horsepower,Auto$weight,Auto$year,Auto$origin1,Auto$origin2)[-train,]
train.Y <- cbind(Auto$high)[train,]
test.Y <- cbind(Auto$high)[-train,]


###########################################

lambda_d_1 <- runif(ncol(train.X),-0.2,0.2)
lambda_not_1 <- runif(1,-0.2,0.2)


Logistic_regr_out_1 <- logistic_regr(train.X,train.Y,100,0.001,lambda_d_1,lambda_not_1)
Y_pred_1 <- Logistic_regr_out_1$Function
Y_pred_1[Y_pred_1>=0.5]<-1
Y_pred_1[Y_pred_1<0.5]<-0
lambda_not_updated_1<- Logistic_regr_out_1$Lambda_not
lambda_updated_1<- Logistic_regr_out_1$Weights

ER_1 = 1/196*sum((train.Y - Y_pred_1)**2)

############################################

lambda_d_2<- runif(ncol(train.X),-0.4,0.4)
lambda_not_2<- runif(1,-0.4,0.4)


Logistic_regr_out_2<- logistic_regr(train.X,train.Y,100,0.001,lambda_d_2,lambda_not_2)
Y_pred_2<- Logistic_regr_out_2$Function
Y_pred_2[Y_pred_2>=0.5]<-1
Y_pred_2[Y_pred_2<0.5]<-0
lambda_not_updated_2 <- Logistic_regr_out_2$Lambda_not
lambda_updated_2 <- Logistic_regr_out_2$Weights

ER_2 = 1/196*sum((train.Y - Y_pred_2)**2)

##############################################


lambda_d_3 <- runif(ncol(train.X),-0.25,0.55)
lambda_not_3 <- runif(1,-0.2,0.5)


Logistic_regr_out_3<- logistic_regr(train.X,train.Y,100,0.001,lambda_d_3,lambda_not_3)
Y_pred_3 <- Logistic_regr_out_3$Function
Y_pred_3[Y_pred_3>=0.5]<-1
Y_pred_3[Y_pred_3<0.5]<-0
lambda_not_updated_3<- Logistic_regr_out_3$Lambda_not
lambda_updated_3 <- Logistic_regr_out_3$Weights

ER_3 = 1/196*sum((train.Y - Y_pred_3)**2)

###############################################


lambda_d_4<- runif(ncol(train.X),-0.3,0)
lambda_not_4<- runif(1,-0.3,0)


Logistic_regr_out_4<- logistic_regr(train.X,train.Y,100,0.001,lambda_d_4,lambda_not_4)
Y_pred_4 <- Logistic_regr_out_4$Function
Y_pred_4[Y_pred_4>=0.5]<-1
Y_pred_4[Y_pred_4<0.5]<-0
lambda_not_updated_4 <- Logistic_regr_out_4$Lambda_not
lambda_updated_4<- Logistic_regr_out_4$Weights

ER_4= 1/196*sum((train.Y - Y_pred_4)**2)


################################################




