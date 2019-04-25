#####

mydata <- read.csv("C:\\Users\\uesr\\Desktop\\alabs_ass\\3\\Line_reg.csv")

#UDF

mystats = function(x) {
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


#Vector of numaerical variables
num_var = sapply(mydata, is.numeric)
Other_var = !sapply(mydata, is.numeric)


#Applying above defined function on numerical variables
my_num_data <- t(data.frame(apply(mydata[num_var], 2, mystats)))
my_cat_data <- data.frame(t(apply(mydata[Other_var], 2, mystats)))
View(my_num_data)
View(my_cat_data)

#Missing Value Treatment
mydata[, num_var] <-
  apply(data.frame(mydata[, num_var]), 2, function(x) {
    x <- replace(x, is.na(x), mean(x, na.rm = TRUE))
  })
mydata[, Other_var] <-
  apply(data.frame(mydata[, Other_var]), 2, function(x) {
    x <- replace(x, is.na(x), which.max(prop.table(table(x))))
  })


mydata$debtinc[mydata$debtinc > 29.1535098654681]  <-
  29.1535098654681
mydata$commutetime[mydata$commutetime > 35.173897327865]  <-
  35.173897327865
mydata$card2spent[mydata$card2spent > 599.754422304668]  <-
  599.754422304668
mydata$tollten[mydata$tollten > 3425.28726891825]  <-
  3425.28726891825
mydata$equipten[mydata$equipten > 3206.83827243166]  <-
  3206.83827243166
mydata$wireten[mydata$wireten > 3424.99447023256]  <-
  3424.99447023256
mydata$total_items <- mydata$carditems + mydata$card2items
mydata$total_items[mydata$total_items > 28.0420685633452]  <- 14.844
mydata$creddebt[mydata$creddebt > 12.1047318441384]  <-
  12.1047318441384

mydata$carditems[mydata$carditems > 20.3508194277328]  <-
  20.3508194277328
mydata$income[mydata$income > 220.892133461375]  <- 220.892133461375
mydata$reside[mydata$reside > 6.38593181564958]  <- 6.38593181564958
mydata$cardspent[mydata$cardspent > 1072.6377248896]  <-
  1072.6377248896
mydata$longmon[mydata$longmon > 51.7915934790731]  <-
  51.7915934790731
mydata$tollmon[mydata$tollmon > 62.1945035285336]  <-
  62.1945035285336
mydata$equipmon[mydata$equipmon > 70.6301378051285]  <-
  70.6301378051285
mydata$cardmon[mydata$cardmon > 60.4665572812341]  <-
  60.4665572812341
mydata$wiremon[mydata$wiremon > 70.1006996032225]  <-
  70.1006996032225
mydata$hourstv[mydata$hourstv > 35.1418256530511]  <-
  35.1418256530511
mydata$lninc[mydata$lninc > 5.94067589731683]  <- 5.94067589731683
mydata$othdebt[mydata$othdebt > 19.8400971616142]  <-
  19.8400971616142
mydata$carvalue[mydata$carvalue > 86.9274901043956]  <-
  86.9274901043956
mydata$card2items[mydata$card2items > 12.1576495342525]  <-
  12.1576495342525
mydata$lnlongmon[mydata$lnlongmon > 4.61408733984828]  <-
  4.61408733984828
mydata$lntollmon[mydata$lntollmon > 166.628284841284]  <-
  166.628284841284
mydata$lnequipmon[mydata$lnequipmon > 140.749676347097]  <-
  140.749676347097
mydata$lncardmon[mydata$lncardmon > 190.889849879953]  <-
  190.889849879953
mydata$lnwiremon[mydata$lnwiremon > 151.875115863804]  <-
  151.875115863804
mydata$total_spend <- mydata$cardspent + mydata$card2spent
mydata$total_spend[mydata$total_spend > 1552.66644022596]  <-
  1552.66644022596

#________________________________________________________________

mydata$lntotal_spend <- log(mydata$total_spend)
hist(mydata$lntotal_spend)

#_____correlation matrix___________________________________________________________

corrm <-
  cor(mydata[, num_var])                                 ### CORRELATION MATRIX
View(corrm)

library(corrplot)
corrplot(corrm, method="circle")
setwd("C:\\Users\\uesr\\Desktop\\alabs_ass\\3\\")
write.csv(corrm, file = "corrm1.csv")



#Splitting data into Training, Validaton and Testing Dataset
train_ind <-
  sample(1:nrow(mydata), size = floor(0.70 * nrow(mydata)))

training <- mydata[train_ind, ]
testing <- mydata[-train_ind, ]

#Building Models for training dataset


fit <- lm(
  lntotal_spend ~ age + region + ed + townsize
  +	income + agecat +	lninc + edcat + debtinc
  + jobcat +	creddebt + employ +	lncreddebt + empcat
  +	othdebt + inccat +	lnothdebt + jobsat +	spoused
  + marital +	carvalue + carcatvalue +	commutetime + commute
  +	tenure + reason +	 card +	total_items + cardbenefit +	active
  + cardfee +	tenure + card2 +	response_01 + card2benefit
  +	response_02 + card2fee +	response_03,
  data = training
)


summary(fit) # show results

install.packages("MASS")

require(MASS)
step3 <- stepAIC(fit, direction = "both")
? stepAIC()
ls(step3)
step3$anova

fit3 <-
  lm(
    lntotal_spend ~ age + agecat + lninc + jobcat + employ + jobsat +
      card + total_items + card2 + response_03,
    data = training
  )

summary(fit3)

#Multicollinierity Check using VIF
library(car)
vif(fit3)

fit4 <- lm(
  lntotal_spend ~ lninc + jobcat + employ + jobsat +
    card + total_items + card2 + response_03,
  data = training
)

summary(fit4)

# #____result_______

# lninc       jobcat       employ       jobsat         card
#
# total_items        card2  response_03

# Estimate Std. Error t value Pr(>|t|)    	 STBetas
# (Intercept)  1.4355762  0.0081218 176.755  < 2e-16
# lninc        0.0517600  0.0014885  34.774  < 2e-16 	 0.324834914
# jobcat      -0.0005699  0.0006592  -0.864  0.38738    	-0.008359996
# employ      -0.0001392  0.0001332  -1.045  0.29607    	-0.011388640
# jobsat      -0.0017480  0.0008610  -2.030  0.04239   	-0.020353911
# card        -0.0212529  0.0009525 -22.312  < 2e-16 	-0.212572308
# total_items  0.0171306  0.0002393  71.572  < 2e-16 	 0.634689673
# card2       -0.0114511  0.0009614 -11.910  < 2e-16 	-0.113426908
# response_03  0.0093940  0.0034349   2.735  0.00626  	 0.024063753


#_____________ standardized betas in R_____________________


lm.beta <- function (MOD)
{
  b <- summary(MOD)$coef[-1, 1]
  sx <- sd(MOD$model[-1])
  sy <- sd(MOD$model[1])
  beta <- b * sx / sy
  return(beta)
}


install.packages("QuantPsyc")
library(QuantPsyc)
lm.beta(fit3)
#
# #__________________________________________________________________
# results
# lninc       jobcat       employ       jobsat         card
# 0.324834914 -0.008359996 -0.011388640 -0.020353911 -0.212572308
# total_items        card2  response_03
# 0.634689673 -0.113426908  0.024063753
# #


########  END   ##################