setwd("C:/Users/Trupti/Desktop")

require(XLConnect)
loan = loadWorkbook("loan.xlsx")
loananalysis = readWorksheet(loan, sheet = "loan", header = TRUE)

Res_status<-as.factor(loananalysis$Res_status)
Occupation<- as.factor(loananalysis$Occupation)
Job_Status<-as.factor(loananalysis$Job_status)
Liab_red<- as.factor(loananalysis$Liab_ref)
Acc_ref<-as.factor(loananalysis$Acc_ref)
decision<-as.factor(loananalysis$Decision)

contrasts(Res_status)
contrasts(Occupation)
contrasts(Job_Status)
contrasts(Liab_red)
contrasts(Acc_ref)
contrasts(decision)

x<- data.frame(Res_status, Occupation, Job_Status, Liab_red, Acc_ref)
loan.fit<-glm(decision ~ Res_status + Occupation+ Job_Status+ Liab_red+ Acc_ref, family="binomial", data=x)
summary(loan.fit)

newdata1= data.frame(Res_status="owner", Occupation="creative_", Job_Status="governmen", Liab_red ="f", Acc_ref="given")
summary(newdata1)
predict(loan.fit, newdata1, type="response")

newdata2= data.frame(Res_status="rent", Occupation="creative_", Job_Status="governmen", Liab_red ="f", Acc_ref="given")
summary(newdata2)
predict(loan.fit, newdata2, type="response")


