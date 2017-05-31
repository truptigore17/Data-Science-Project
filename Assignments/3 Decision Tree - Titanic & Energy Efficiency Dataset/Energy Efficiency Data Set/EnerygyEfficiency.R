setwd("C:/Users/Trupti/Desktop")
require(XLConnect)
EnergyEfficiency = loadWorkbook("EnergyEfficiency.xlsx")
EnergyEfficiencyanalysis = readWorksheet(EnergyEfficiency, sheet = "EnergyEfficiency", header = TRUE)
EnergyEfficiencyanalysis

# Relation between variables in data set
cor(EnergyEfficiencyanalysis)

#DecisionTreeRegression for Yield Y1
library(rpart)
DescTreeRegressionY1<-rpart(Y1~X1+X2+X3+X4+X5+X6+X7+X8, data=EnergyEfficiencyanalysis, method = "anova")
summary(DescTreeRegressionY1)
prp(DescTreeRegressionY1)
rpart.plot(DescTreeRegressionY1)

#DecisionTreeRegression for Yield Y2
library(rpart)
DescTreeRegressionY2<-rpart(Y2~X1+X2+X3+X4+X5+X6+X7+X8, data=EnergyEfficiencyanalysis, method = "anova")
summary(DescTreeRegressionY2)
prp(DescTreeRegressionY2)
rpart.plot(DescTreeRegressionY2)

