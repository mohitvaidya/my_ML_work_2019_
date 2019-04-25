
library(caTools)
library(rpart)
library(ROCR)
library(e1071)
library(caret)
library(corrplot)
require(randomForest)
require(MLmetrics)
library(pROC)
library(corrplot)
setwd("C:\\Users\\user\\Desktop\\alabs\\4\\")

# data loading
main_data <- read.csv('train.csv') 

data <- main_data


#dropping all of the irrelevant columns

col_drop <- c('country','unique_id','region', 'sourceMedium', 'dayHourMinute', 'page1_exits', 'page2_exits', 'page3_exits', 'page4_exits', 'page5_exits', 'page6_exits')

data <- data[,!colnames(data) %in% col_drop]

# vector of all numerical columns in data  
num_col <- c('metric2', 'metric6', 'metric3', 'metric4')

###UDF for basic stat generation for numerical variables

mystats = function(x) {

    n <- length(x)
    nmiss <- sum(is.na(x))
    mean <- mean(x, na.rm = T)
    std <- sd(x, na.rm = T)
    var <- var(x, na.rm = T)
    min <- min(x, na.rm = T)
    p1 <- quantile(x, 0.01, na.rm = T)
    p5 <- quantile(x, 0.05, na.rm = T)
    q1 <- quantile(x, 0.25, na.rm = T)
    q3 <- quantile(x, 0.75, na.rm = T)
    p95 <- quantile(x, 0.95, na.rm = T)
    p99 <- quantile(x, 0.99, na.rm = T)
    max <- max(x, na.rm = T)
    UC1 = mean(x, na.rm = T) + 3 * sd(x, na.rm = T)
    LC1 = mean(x, na.rm = T) - 3 * sd(x, na.rm = T)
    UC2 = quantile(x, 0.99, na.rm = T)
    LC2 = quantile(x, 0.01, na.rm = T)
    iqr = IQR(x, na.rm = T)
    UC3 = q3 + 1.5 * iqr
    LC3 = q1 - 1.5 * iqr
    ot1 <- max > UC1 | min < LC1
    ot2 <- max > UC2 | min < LC2
    ot3 <- max > UC3 | min < LC3
    return(
      c(
        n = n,nmiss = nmiss,mean = mean,std = std,var = var,min = min,p1 = p1,p5 = p5,
        q1 = q1,q3 = q3,p95 = p95,p99 = p99,max = max,ot_m1 = ot1,ot_m2 = ot2,
        ot_m2 = ot3))
  }


#data_stat <- t(data.frame(apply(data[num_col], 2, mystats)))
#write.csv(data_stat,'./data_stat.csv')
### capping outliers with percentile 1 and 99. also applying square root transformation to reduce skewness of the data


out_capp <- function(data_,num_col)
{
  tryCatch({
    for (item in num_col)
    { data_[item][data_[item]>quantile(data_[item],0.99,na.rm = TRUE)] <-  quantile(data_[item],0.99,na.rm = TRUE)
    data_[item] <- sqrt(data_[item])
    }
    return(data_)
  },
  error=function(msg){
    print(msg)
  })
}




data <- out_capp(data,num_col)

corrplot(cor(data[num_col]), type = "upper", order = "hclust",tl.col = "black", tl.srt = 45)
## dropping metric4 based on high correlation with metric3

data$metric4 <- NULL

# bucketing numerical values to reduce skewness and factorisation

data$metric5	<- factor(ifelse(data$metric5>0,0,1))
data$page1_top	<- factor(ifelse(data$page1_top>0,0,1))
data$page2_top	<- factor(ifelse(data$page2_top>0,0,1))
data$page3_top	<- factor(ifelse(data$page3_top>0,0,1))
data$page4_top	<- factor(ifelse(data$page4_top>0,0,1))
data$page5_top	<- factor(ifelse(data$page5_top>0,0,1))
data$page6_top	<- factor(ifelse(data$page6_top>0,0,1))

data$device <-  factor(data$device)
data$target <-  factor(data$target)
# undersampling majority class to make data balanced.
data <- data[sample(nrow(data)),]
data_0 <- data[data$target==0,]

rows  <- c(sample(nrow(data)*0.15))

data_0 <- data[rows,]

data_1 <- data[data$target==1,]

final_data <- rbind(data_0,data_1)

set.seed(174)

# train and test split of data(70 and 30)  
split = sample.split(final_data$target, SplitRatio = 0.7)
train = subset(final_data, split==TRUE)
test = subset(final_data, split==FALSE)
nrow(train)
nrow(test)


########################################################################

## fitting logistic model

lgmodel <- glm(target ~ ., data=train, family = binomial(logit))

pred1 <- prediction(predict(lgmodel,type="response"),train$target )
perf1 <- performance(pred1,"tpr","fpr")
#plotting ROC curve

plot(perf1)

predictCART = as.numeric(predict(lgmodel, newdata=test,type="response"))

# Deciding probability cut off of 0.4 based on ROC

predi <- ifelse(predictCART>0.50,1,0)

confusionMatrix(factor(test$target), factor(predi))

F1_Score(test$target,predi)  

roc_obj <- roc(test$target, predi)
auc(roc_obj)

summary(lgmodel)

#############################################################################################
#Fitting random forest with 30 trees

write.csv(data.frame(colnames(train)),'./train_cols.csv')


rforest <- randomForest(target ~ ., data=train,ntree=30)

predictCART  <-  factor(predict(rforest, newdata = test))

confusionMatrix(factor(test$target), factor(predictCART))

F1_Score(test$target,predictCART)

roc_obj <- roc(as.numeric(test$target), as.numeric(factor(predictCART)))
auc(roc_obj)
summary(rforest)

##############################################################################################
#prediction on test data

test_data <- read.csv('./test.csv')
unique_id <- test_data$unique_id

# same pre_processing as training data

test_data <- out_capp(test_data,num_col)
test_data$metric4 <- NULL

## selecting columns on which final model has been trained
test_col <- c("metric1",	"metric2",	"metric6",	"metric3",	"metric5",	"binary_var1",	"device",	"binary_var2",	"page1_top",	"visited_page1",	"page2_top",	"visited_page2",	"page3_top",	"visited_page3",	"page4_top",	"visited_page4",	"page5_top",	"visited_page5",	"page6_top",	"visited_page6")
test_data <- test_data[test_col]

test_data$metric5	<- factor(ifelse(test_data$metric5>0,0,1))
test_data$page1_top	<- factor(ifelse(test_data$page1_top>0,0,1))
test_data$page2_top	<- factor(ifelse(test_data$page2_top>0,0,1))
test_data$page3_top	<- factor(ifelse(test_data$page3_top>0,0,1))
test_data$page4_top	<- factor(ifelse(test_data$page4_top>0,0,1))
test_data$page5_top	<- factor(ifelse(test_data$page5_top>0,0,1))
test_data$page6_top	<- factor(ifelse(test_data$page6_top>0,0,1))

test_data$device <-  factor(test_data$device)
# predicting results

predictCART_test  <-  factor(predict(rforest, newdata = test_data))

test_res <- as.data.frame(cbind(unique_id,predictCART_test))
names(test_res) <- c("unique_id","predicted_label")

# writing predictions
write.csv(test_res,'./test_results.csv')
