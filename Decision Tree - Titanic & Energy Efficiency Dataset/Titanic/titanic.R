install.packages("RColorBrewer")
install.packages("rattle")
install.packages('rpart.plot')
install.packages("ggplot2")
library(rpart)

setwd("C:/Users/Trupti/Desktop")
titanictrain<-read.csv("titanictrain.csv", header=TRUE);
titanictrain
ggplot(titanictrain, aes(x=factor(Pclass),fill=factor(Sex)))+geom_bar(position = "dodge")

titanictrain_decisiontree<-rpart(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked, data= titanictrain,method="class")   #build decision tree
library(rattle)
library(RColorBrewer)
library(rpart.plot)
plot(titanictrain_decisiontree)
text(titanictrain_decisiontree)

#Build decision tree
fancyRpartPlot(titanictrain_decisiontree) 
summary(titanictrain_decisiontree)
prp(titanictrain_decisiontree)
prp

#prediction
prediction <- predict(titanictrain_decisiontree, titanictrain, type = "class")
plot(prediction)

#prediction in csv format
solution <- data.frame(PassengerId = titanictrain$PassengerId, Survived = prediction)
write.csv(solution, file = "solution.csv", row.names = FALSE)