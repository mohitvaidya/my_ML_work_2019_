#####logistic case study#######

mydata <-
  read.csv("C:\\Users\\uesr\\Desktop\\alabs_ass\\2\\Proactive Attrition Management-Logistic Regression Case Study.csv"  )
View(mydata)

str(mydata)
View(mydata)

mydata$CSA <- factor(mydata$CSA)

#Create user defined function for descriptive analysis
var_Summ = function(x) {
  if (class(x) == "numeric") {
    Var_Type = class(x)
    n <- length(x)
    nmiss <- sum(is.na(x))
    mean <- mean(x, na.rm = T)
    std <- sd(x, na.rm = T)
    var <- var(x, na.rm = T)
    min <- min(x, na.rm = T)
    p1 <- quantile(x, 0.01, na.rm = T)
    p5 <- quantile(x, 0.05, na.rm = T)
    p10 <- quantile(x, 0.1, na.rm = T)
    q1 <- quantile(x, 0.25, na.rm = T)
    q2 <- quantile(x, 0.5, na.rm = T)
    q3 <- quantile(x, 0.75, na.rm = T)
    p90 <- quantile(x, 0.9, na.rm = T)
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
        Var_Type = Var_Type,
        n = n,
        nmiss = nmiss,
        mean = mean,
        std = std,
        var = var,
        min = min,
        p1 = p1,
        p5 = p5,
        p10 = p10,
        q1 = q1,
        q2 = q2,
        q3 = q3,
        p90 = p90,
        p95 = p95,
        p99 = p99,
        max = max,
        ot_m1 = ot1,
        ot_m2 = ot2,
        ot_m2 = ot3
      )
    )
  }
  else{
    Var_Type = class(x)
    n <- length(x)
    nmiss <- sum(is.na(x))
    fre <- table(x)
    prop <- prop.table(table(x))
    return(c(
      Var_Type = Var_Type,
      n = n,
      nmiss = nmiss,
      freq = fre,
      proportion = prop
    ))
  }
}

#Vector of numerical variables
num_var = sapply(mydata, is.numeric)
Other_var = !sapply(mydata, is.numeric)

#Applying above defined function on numerical variables
my_num_data <- t(data.frame(apply(mydata[num_var], 2, var_Summ)))
my_cat_data <- data.frame(t(apply(mydata[Other_var], 2, var_Summ)))
View(my_num_data)
View(my_cat_data)

#write.csv(my_num_data, file = "Data_stats.csv")

# missing values
apply(is.na(mydata[, ]), 2, sum)

mydata1 <- mydata[!is.na(mydata$CHURN), ]

View(mydata1)

#Missing Value Treatment
mydata1[, num_var] <-
  apply(data.frame(mydata1[, num_var]), 2, function(x) {
    x <- replace(x, is.na(x), mean(x, na.rm = TRUE))
  })
mydata1[, Other_var] <-
  apply(data.frame(mydata1[, Other_var]), 2, function(x) {
    x <- replace(x, is.na(x), which.max(prop.table(table(x))))
  })


## OUTLIER TREATMENT AT 1% AND 99%
mydata1$REVENUE[mydata1$REVENUE > 135.39] <- 135.39
mydata1$MOU[mydata1$MOU > 1580.25] <- 1580.25
mydata1$RECCHRGE[mydata1$RECCHRGE > 85] <- 85
mydata1$DIRECTAS[mydata1$DIRECTAS > 4.21] <- 4.21
mydata1$OVERAGE[mydata1$OVERAGE > 190.375] <- 190.375
mydata1$ROAM[mydata1$ROAM > 5.09] <- 5.09
mydata1$CHANGEM[mydata1$CHANGEM > 345.25] <- 345.25
mydata1$CHANGER[mydata1$CHANGER > 46.218] <- 46.218
mydata1$DROPVCE[mydata1$DROPVCE > 22] <- 22
mydata1$BLCKVCE[mydata1$BLCKVCE > 17.33] <- 17.33
mydata1$UNANSVCE[mydata1$UNANSVCE > 97.67] <- 97.67
mydata1$CUSTCARE[mydata1$CUSTCARE > 9.33] <- 9.33
mydata1$THREEWAY[mydata1$THREEWAY > 1.33] <- 1.33
mydata1$MOUREC[mydata1$MOUREC > 440.938] <- 440.938
mydata1$OUTCALLS[mydata1$OUTCALLS > 90.33] <- 90.33
mydata1$INCALLS[mydata1$INCALLS > 35.67] <- 35.67
mydata1$PEAKVCE[mydata1$PEAKVCE > 279.67] <- 279.67
mydata1$OPEAKVCE[mydata1$OPEAKVCE > 242] <- 242
mydata1$DROPBLK[mydata1$DROPBLK > 35.33] <- 35.33
mydata1$CALLFWDV[mydata1$CALLFWDV > 0] <- 0
mydata1$CALLWAIT[mydata1$CALLWAIT > 8.67] <- 8.67
mydata1$CHURN[mydata1$CHURN > 1] <- 1
mydata1$MONTHS[mydata1$MONTHS > 37] <- 37
mydata1$UNIQSUBS[mydata1$UNIQSUBS > 3] <- 3
mydata1$ACTVSUBS[mydata1$ACTVSUBS > 2] <- 2
mydata1$PHONES[mydata1$PHONES > 4] <- 4
mydata1$MODELS[mydata1$MODELS > 3] <- 3
mydata1$EQPDAYS[mydata1$EQPDAYS > 865.75] <- 865.75
mydata1$CUSTOMER[mydata1$CUSTOMER > 1095335.7] <- 1095335.7
mydata1$AGE1[mydata1$AGE1 > 62] <- 62
mydata1$AGE2[mydata1$AGE2 > 62] <- 62
mydata1$RETCALLS[mydata1$RETCALLS > 0] <- 0
mydata1$RETACCPT[mydata1$RETACCPT > 0] <- 0
mydata1$REFER[mydata1$REFER > 0] <- 0
mydata1$INCOME[mydata1$INCOME > 9] <- 9
mydata1$CREDITAD[mydata1$CREDITAD > 0] <- 0
mydata1$SETPRC[mydata1$SETPRC > 149.99] <- 149.99

mydata1$REVENUE[mydata1$REVENUE < 15.515] <- 15.515
mydata1$MOU[mydata1$MOU < 20.415] <- 20.415
mydata1$RECCHRGE[mydata1$RECCHRGE < 10] <- 10
mydata1$DIRECTAS[mydata1$DIRECTAS < 0] <- 0
mydata1$OVERAGE[mydata1$OVERAGE < 0] <- 0
mydata1$ROAM[mydata1$ROAM < 0] <- 0
mydata1$CHANGEM[mydata1$CHANGEM < -376] <- -376.25
mydata1$CHANGER[mydata1$CHANGER < -47.5] <- -47.5
mydata1$DROPVCE[mydata1$DROPVCE < 0] <- 0
mydata1$BLCKVCE[mydata1$BLCKVCE < 0] <- 0
mydata1$UNANSVCE[mydata1$UNANSVCE < 0] <- 0
mydata1$CUSTCARE[mydata1$CUSTCARE < 0] <- 0
mydata1$THREEWAY[mydata1$THREEWAY < 0] <- 0
mydata1$MOUREC[mydata1$MOUREC < 0] <- 0
mydata1$OUTCALLS[mydata1$OUTCALLS < 0] <- 0
mydata1$INCALLS[mydata1$INCALLS < 0] <- 0
mydata1$PEAKVCE[mydata1$PEAKVCE < 0] <- 0
mydata1$OPEAKVCE[mydata1$OPEAKVCE < 0] <- 0
mydata1$DROPBLK[mydata1$DROPBLK < 0] <- 0
mydata1$CALLFWDV[mydata1$CALLFWDV < 0] <- 0
mydata1$CALLWAIT[mydata1$CALLWAIT < 0] <- 0
mydata1$CHURN[mydata1$CHURN < 0] <- 0
mydata1$MONTHS[mydata1$MONTHS < 7] <- 7
mydata1$UNIQSUBS[mydata1$UNIQSUBS < 1] <- 1
mydata1$ACTVSUBS[mydata1$ACTVSUBS < 1] <- 1
mydata1$PHONES[mydata1$PHONES < 1] <- 1
mydata1$MODELS[mydata1$MODELS < 1] <- 1
mydata1$EQPDAYS[mydata1$EQPDAYS < 42] <- 42
mydata1$CUSTOMER[mydata1$CUSTOMER < 1004986.9] <- 1004986.9
mydata1$AGE1[mydata1$AGE1 < 0] <- 0
mydata1$AGE2[mydata1$AGE2 < 0] <- 0
mydata1$RETCALLS[mydata1$RETCALLS < 0] <- 0
mydata1$RETACCPT[mydata1$RETACCPT < 0] <- 0
mydata1$REFER[mydata1$REFER < 0] <- 0
mydata1$INCOME[mydata1$INCOME < 0] <- 0
mydata1$CREDITAD[mydata1$CREDITAD < 0] <- 0
mydata1$SETPRC[mydata1$SETPRC < 0] <- 0

TESTDATA <- t(data.frame(apply(mydata1[num_var], 2, var_Summ)))
#write.csv(TESTDATA, file = "TESTDATA.csv")

#_____correlation matrix___

corrm <-
  cor(mydata1[, num_var])                                 ### CORRELATION MATRIX
View(corrm)

#write.csv(corrm, file = "corrm1.csv")


##VAR TRANSFORM

mydata1$root_MOU <- sqrt(mydata1$MOU)
mydata1$root_eqpdays <- sqrt(mydata1$EQPDAYS)
mydata1$root_overage <- sqrt(mydata1$OVERAGE)


#Splitting data into Training, Validaton and Testing Dataset
train_ind <-
  sample(1:nrow(mydata1), size = floor(0.70 * nrow(mydata1)))

training <- mydata1[train_ind, ]
testing <- mydata1[-train_ind, ]
#nrow(training)
#nrow(testing)

names(training)

#Building Models for training dataset

fit <- glm(
  CHURN ~ OVERAGE + EQPDAYS
  + OPEAKVCE
  + OUTCALLS
  + PEAKVCE
  + DROPVCE
  + INCALLS
  + RECCHRGE
  + PHONES
  + MODELS
  + SETPRC
  + MONTHS
  + CREDITA
  + WEBCAP
  + UNIQSUBS
  + ACTVSUBS
  + CHANGEM
  + CHANGER
  + INCOME
  + PRIZMTWN
  + PRIZMUB
  + CREDITDE
  + CREDITAA
  + ROAM
  + CREDITC
  + PRIZMRUR
  + NEWCELLY
  + OCCRET,
  data = training,
  family = binomial(logit)
)

#Output of Logistic Regression
summary(fit)
ls(fit)
fit$model

coeff <- fit$coef #Coefficients of model
#write.csv(   coeff,  "coeff.csv" )

library(car)
asd <- as.matrix(vif(fit))
#write.csv(  asd,  "vif1.csv")

#Check concordance
source(
"file_path_concordance"
)

concordance(fit)
# 0.61028


#running Stepwise regression
step1 = step(fit, direction = "both")


#Final Model
fit2 <- glm(
  CHURN ~ PRIZMTWN +
    root_MOU +
    root_eqpdays +
    root_overage +
    NEWCELLY +
    INCALLS +
    OCCRET +
    PRIZMUB +
    CREDITA +
    INCOME +
    PRIZMRUR +
    WEBCAP +
    ROAM +
    DROPVCE +
    SETPRC +
    CREDITC +
    MAILRES +
    RECCHRGE +
    ACTVSUBS +
    CHANGER +
    UNIQSUBS +
    CREDITDE +
    CHANGEM,
  data = training,
  family = binomial(logit)
)

summary(fit2)

source(  "your_path" )

Concordance(fit2)

#Multicollinierity Check using VIF


install.packages("car")
library(car)
asd2 <- as.matrix(vif(fit2))

write.csv(
  asd2,
  "C:\\Users\\uesr\\Desktop\\alabs_ass\\2\\model_vif.csv"
)

write.csv(as.matrix(fit2$coefficients),  "C:\\Users\\uesr\\Desktop\\alabs_ass\\2\\model_coeff.csv")
################################VALIDATION ##############################

#Decile Scoring for
##Training dataset
train1 <- cbind(training, Prob = predict(fit2, type = "response"))
View(train1)

##Creating Deciles
decLocations <- quantile(train1$Prob, probs = seq(0.1, 0.9, by = 0.1))
train1$decile <- findInterval(train1$Prob, c(-Inf, decLocations, Inf))
View(train1)
require(dplyr)
train1$decile <- factor(train1$decile)
decile_grp <- group_by(train1, decile)
decile_summ_train <-
  summarize(
    decile_grp,
    total_cnt = n(),
    min_prob = min(p = Prob),
    max_prob = max(Prob),
    CHURN_cnt = sum(CHURN),
    non_CHURN_cnt = total_cnt - CHURN_cnt
  )
decile_summ_train <- arrange(decile_summ_train, desc(decile))
View(decile_summ_train)

write.csv(
  decile_summ_train,
  "C:\\Users\\uesr\\Desktop\\alabs_ass\\2\\log_results.csv",
  row.names = F
)

##Testing dataset
test1 <- cbind(testing, Prob = predict(fit2, testing, type = "response"))
View(test1)

##Creating Deciles
decLocations <- quantile(test1$Prob, probs = seq(0.1, 0.9, by = 0.1))
test1$decile <- findInterval(test1$Prob, c(-Inf, decLocations, Inf))
names(test1)


test1$decile <- factor(test1$decile)
decile_grp <- group_by(test1, decile)
decile_summ_test <-
  summarize(
    decile_grp,
    total_cnt = n(),
    min_prob = min(p = Prob),
    max_prob = max(Prob),
    CHURN_cnt = sum(CHURN),
    non_CHURN_cnt = total_cnt - CHURN_cnt
  )
decile_summ_test <- arrange(decile_summ_test, desc(decile))
View(decile_summ_test)
write.csv(
  decile_summ_test,
  "C:\\Users\\uesr\\Desktop\\alabs_ass\\2\\test_res.csv",
  row.names = F)

##########  END   ###########

