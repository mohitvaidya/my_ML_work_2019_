
file <- choose.files()
setwd("C:\\Users\\uesr\\Desktop\\alabs_ass\\6")
ccmain <- read.csv('CC GENERAL.csv',header = T)
cc <- ccmain

#UDF for missing values and other stats
datastats <- function(x){
  nmiss <- sum(is.na(x))
  a <- x[!is.na(x)]
  m <- mean(a)
  ln <- length(a)
  min <- min(a)
  max <- max(a)
  sd <- sd(a)
  UC <- m+3*sd
  LC <- m-3*sd
  p95 <- quantile(a,0.95)
  p99 <- quantile(a,0.99)
  return(c(nmiss=nmiss,m=m,ln=ln,min=min,max=max,sd=sd,UC=UC,LC=LC,p95=p95,p99=p99))
}


vars <- c("PURCHASES",	"PURCHASES_TRX",	"ONEOFF_PURCHASES",	"INSTALLMENTS_PURCHASES",	"CASH_ADVANCE",	"CASH_ADVANCE_TRX",	"PAYMENTS",	"CREDIT_LIMIT",	"MINIMUM_PAYMENTS",	"BALANCE",	"PRC_FULL_PAYMENT",	"TENURE")
ccstats <- t(apply(cc[vars],2,datastats))
write.csv(ccstats,'ccstats.csv')

  #one row with credit limit Na has been deleted.
cc <- cc[!is.na(cc$CREDIT_LIMIT),]

require(Hmisc)

cc$MINIMUM_PAYMENTS[is.na(cc$MINIMUM_PAYMENTS) ]<- mean(cc$MINIMUM_PAYMENTS,na.rm = T)
mean(cc$PURCHASES)+3*sd(cc$PURCHASES)
quantile(cc$PURCHASES,0.99)
hist(cc$PURCHASES)
#outlier treatment
##################################################################
cc$PURCHASES[cc$PURCHASES>7413.10917913822]<-7413.10917913822
cc$PURCHASES_TRX[cc$PURCHASES_TRX>89.2827797318889]<-89.2827797318889
cc$ONEOFF_PURCHASES[cc$ONEOFF_PURCHASES>5572.10112326315]<-5572.10112326315
cc$INSTALLMENTS_PURCHASES[cc$INSTALLMENTS_PURCHASES>3124.08199021888]<-3124.08199021888
cc$CASH_ADVANCE[cc$CASH_ADVANCE>7270.36274239518]<-7270.36274239518
cc$CASH_ADVANCE_TRX[cc$CASH_ADVANCE_TRX>23.72276704814]<-23.72276704814
cc$PAYMENTS[cc$PAYMENTS>10418.3351227384]<-10418.3351227384
cc$CREDIT_LIMIT[cc$CREDIT_LIMIT>15410.8966268601]<-15410.8966268601
cc$MINIMUM_PAYMENTS[cc$MINIMUM_PAYMENTS>7981.54636205701]<-7981.54636205701
cc$BALANCE[cc$BALANCE>7809.07046604776]<-7809.07046604776
cc$PRC_FULL_PAYMENT[cc$PRC_FULL_PAYMENT>1.03121223719326]<-1.03121223719326
cc$TENURE[cc$TENURE>15.5323107438562]<-15.5323107438562

#Corelation Matrix
cccor <- cor(cc[vars])
write.csv(cccor,'cccor.csv')

y = prcomp(cc[vars])
summary(y)
y$sdev^2
screeplot(y,type="lines")
install.packages("psych")
require(plyr)
require(psych)
require(GPArotation)
require(Hmisc)


eigen <- mutate(data.frame(eigen(cccor)$values),cum_sum=cumsum(eigen(cccor)$values),
                cum_pct=eigen(cccor)$values/sum(eigen(cccor)$values),cs=cum_sum/sum(eigen(cccor)$values))

  
FA <- fa(cccor,4,rotate = "varimax",fm="ml")
FA <- fa.sort(FA)

write.csv(eigen,'eigen_val.csv')
write.csv(FA$loadings,'FA.csv')



clustercc <- cc[vars]
clustercc <- scale(clustercc)
cluster_3 <- kmeans(clustercc,3)
cluster_4 <- kmeans(clustercc,4)
cluster_5 <- kmeans(clustercc,5)
cluster_6 <- kmeans(clustercc,6)

newdata <- cbind(cc,clus_3=cluster_3$cluster,clus_4=cluster_4$cluster,clus_5=cluster_5$cluster,clus_6=cluster_6$cluster)
newdata$clus_3 <- factor(newdata$clus_3)
newdata$clus_4 <- factor(newdata$clus_4)
newdata$clus_5 <- factor(newdata$clus_5)
newdata$clus_6 <- factor(newdata$clus_6)

require(cluster)
clusplot(newdata,newdata$clus_5,color = T,lines = 6,labels = 2)

clusplot(newdata,newdata$clus_4,color = T,lines = 6)

clusplot(newdata,newdata$clus_3,color = T,lines = 6)

install.packages("tables")
require(tables)

ccprofile <- tabular(PURCHASES+PURCHASES_TRX+	ONEOFF_PURCHASES	+INSTALLMENTS_PURCHASES	+CASH_ADVANCE+	CASH_ADVANCE_TRX+	PAYMENTS	+CREDIT_LIMIT	+MINIMUM_PAYMENTS	+BALANCE+	PRC_FULL_PAYMENT	+TENURE
~mean+(mean*clus_3)+(mean*clus_4)+(mean*clus_5)+(mean*clus_6),data = newdata)

ccprofile <- as.matrix(ccprofile)
ccprofile <- data.frame(ccprofile)
write.csv(ccprofile,'cc_profile.csv')



OVERALL <- tabular(1~length+(length*clus_3)+(length*clus_4)+(length*clus_5)+(length*clus_6),data = newdata)

OVERALL <- as.matrix(OVERALL)
OVERALL <- data.frame(OVERALL)
write.csv(OVERALL,'OVERALL_profile.csv')




