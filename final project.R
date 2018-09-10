start_time <- Sys.time()
library(caret)
library(e1071)

err_comp <- function(predicted,expected){
  for(i in 1:length(predicted)){
    flag <- FALSE
    if(predicted[i]==expected[i]){
      flag <- TRUE
    }
    if(!exists("fn_out")){
      fn_out <- c(flag)
    }
    else{
      fn_out[length(fn_out)+1] <- flag
    }
  }
  return(fn_out)
}

#predict $classe variable from any other variable
training <- read.csv("pml-training.csv")
testing <- read.csv("pml-testing.csv")


subTrain_all <- training[,8:160]
testSet <- testing[,8:160]

for(col in 1:(dim(training)[2]-1)){
  training[,col] <- as.numeric(training[,col])
  testing[,col] <- as.numeric(testing[,col])
}

#sec_first <- c(8,46,84,122)
#sec_last <- c(45,83,121,159)



#count the number of NAs per column
count <- numeric(dim(subTrain_all)[2])
for(col in 1:dim(subTrain_all)[2]){
  count[col] <- length(which(is.na(training[,7+col])))+length(which(is.na(testing[,7+col])))
  count_all <- which(count != 0)
}

subTrain_all <- subTrain_all[,-count_all]
testSet <- testSet[,-count_all]

#variable selection
fit_all <- glm(classe~.,data=subTrain_all,family="binomial")
inds <- c(which(summary(fit_all)$coef[,dim(summary(fit_all)$coef)[2]]<=0.05),dim(subTrain_all)[2])
subTrain_all <- subTrain_all[,inds]
testSet <- testSet[,inds]

fit_all <- train(classe~.,data=subTrain_all,method="rf",ntree=5)

out_all <- predict(fit_all)

err_all <- table(err_comp(out_all,subTrain_all[,"classe"]))

test_out <- predict(fit_all,testSet)

end_time <- Sys.time()
print("Total runtime:")
print(end_time - start_time)