#install.packages("neuralnet")
#install.packages("ISLR")

getwd()
setwd("E:/ADS/Class Practise")

# Read CSV into R
require(XLConnect)
library(xlsx)
loan= read.xlsx("loan.xlsx",sheetIndex = 1)

#Convert to Numeric
loan$Res_status<-as.numeric(loan$Res_status)-1
loan$Occupation<- as.numeric(loan$Occupation)-1
loan$Job_Status<-as.numeric(loan$Job_status)-1
loan$Liab_red<- as.numeric(loan$Liab_ref)-1
loan$Acc_ref<-as.numeric(loan$Acc_ref)-1
loan$Decision<-as.numeric(loan$Decision)-1

#Move required columns to one dataframe
myloan<-loan[c("Res_status","Occupation","Job_Status","Liab_red","Acc_ref","Decision")]

library(ISLR)
library(neuralnet)
print(head(myloan))

#Separate Train and Test data
x<-sample(1:nrow(myloan), nrow(myloan)*0.90)
train_ <- myloan[x,]
test_ <- myloan[-x,]

#apply neural network
nn <- neuralnet(train_$Decision ~ train_$Res_status+train_$Occupation+train_$Job_Status+train_$Liab_red+train_$Acc_ref,data=train_, hidden=c(2,2,2),linear.output=FALSE, stepmax = 1e6)
predicted.nn.values <- compute(nn,test_[1:5])
predicted.nn.values$net.result <- sapply(predicted.nn.values$net.result,round,digits=0)
table(test_$Decision,predicted.nn.values$net.result)
predicted.nn.values$net.result

#Plot Neural Network
plot(nn)
