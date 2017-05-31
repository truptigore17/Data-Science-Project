###########################################################
######### Please fill the ??? with proper description (atleast 130 charaters for each)
######### for SVM function try different values to achieve better results


# loading neccessary packages and dataset
install.packages("caret")
install.packages("e1701")
library(caret)
library(e1071)
data(GermanCredit)
dataset = GermanCredit
write.csv(dataset, file="Germancredit.csv")
getwd()

  

# ???
str(dataset)
dataset[,1:7] = as.data.frame(lapply(dataset[,1:7], scale))
str(dataset)
dataset
write.csv(dataset, file="dataset.csv")
# ???
sample_index = sample(1000, 200)
test_dateset = dataset[sample_index,]
train_dateset = dataset[-sample_index,]


#alternatively traditional interface :
#x<-subset(train_dateset, select = -Class)
#y<- train_dateset$Class
#model<-svm(x,y, scale=F)


##Compute the best kernel from the four 
#obj<-tune(svm, Class~.,kernel ="radial", data= train_dateset, ranges=list(gamma=2^(-15:3), cost=2^(-5:15)))
#summary(obj)k

#obj<-tune.svm(Class~.,kernel ="linear", data= train_dateset, cost=2^(-5:10))
#summary(obj)

#obj<-tune(svm, Class~.,kernel ="polynomial", data= train_dateset, ranges=list(gamma=2^(-15:3), cost=2^(-5:15)))
#summary(obj)

#obj<-tune(svm, Class~.,kernel ="sigmoid", data= train_dateset, ranges=list(gamma=2^(-15:3), cost=2^(-5:15)))
#summary(obj)

# ???
#model = svm(Class ~ ., kernel = ???, cost = ???, gamma = ???, data = train_dateset, scale = F)
model = svm(Class ~ ., kernel = "radial", cost = 2,gamma=0.0625, data = train_dateset, scale = F)



# ???
predictions <-  predict(model, test_dateset[-10])
# ???
table(test_dateset[,10], predictions)

