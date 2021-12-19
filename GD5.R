
obj_func <- 0

###########################################

sigmoid <- function(y){
  sig<-1/(1+exp(-y))
  return(sig)
}

############################################
percentage_diff <- function(m,n){

  temp = ((n-m)/n)*100
  return(temp)
}

############################################

logistic_regr <- function(X,Y,iteration,n){
  
  k <- 1
  
  while(1){
    
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
    obj_func<-append(obj_func,L)
    
    
    if (k>11 && percentage_diff(obj_func[k+1],obj_func[k-10]) < 1){
      break
    }

    #Gradient Descent
    cn = (Y-(1-Y))/((Y*Fn)+((1-Y)*(1-Fn)))
    delFn = Fn*(1-Fn)
    grad = -1*cn*delFn
    obj_func
    #New Values of Lambda 
    adjust_lambda_X<- grad%*%X
    adjust_lambda_not <- grad%*%matrix(1,nrow(X),1)
    
    #Adjusting values
    lambda_d <- lambda_d-(n)*(adjust_lambda_X)
    lambda_not <- lambda_not-(n)*(adjust_lambda_not)
    
    k = k+1
    
  }
  print("The number of iterations is")
  print(k)
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

lambda_d <- runif(ncol(train.X),-0.4,0.4)
lambda_not <- runif(1,-0.4,0.4)


Logistic_regr_out <- logistic_regr(train.X,train.Y,1000,0.001)
Y_pred <- Logistic_regr_out$Function
Y_pred[Y_pred>=0.5]<-1
Y_pred[Y_pred<0.5]<-0
lambda_not_updated <- Logistic_regr_out$Lambda_not
lambda_updated <- Logistic_regr_out$Weights

ER = 1/196*sum((train.Y - Y_pred)**2)

############################################

F_test <- lambda_updated %*% t(test.X) + lambda_not_updated[1]

Y_pred_test <- sigmoid(F_test)
Y_pred_test[Y_pred_test>=0.5]<-1
Y_pred_test[Y_pred_test<0.5]<-0

ER_test = 1/196*(sum((test.Y - Y_pred_test)**2))


############################################







