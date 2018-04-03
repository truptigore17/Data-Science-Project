require(XLConnect)
df = loadWorkbook("ChemicalProcessData.xlsx")
ChemicalProcessData = readWorksheet(df, sheet = "ChemicalProcessData", header = TRUE)
Mat_ChemicalProcessData<-as.matrix(ChemicalProcessData)
Identity_Mat<-diag(17)

Matx_Xtnd<-as.matrix(cbind(1,ChemicalProcessData))
Matx_X<-Matx_Xtnd[,c(1,3,4)]
Matx_X #Matrix X
Matx_Y<-Matx_Xtnd[,c(5)]
Matx_Y #Matrix Y
Matx_X_t<-t(Matx_X) #Transpose of X
Matx_Y_t<-t(Matx_Y)  #Transpose of Y
Matx_XtX<-Matx_X_t%*%Matx_X
library(MASS) #import library
Matx_XtX_I<-ginv(Matx_XtX)
Matx_Xt_Y<-Matx_X_t%*%Matx_Y  
Matx_Beta<-Matx_XtX_I%*%Matx_Xt_Y #Beta
Matx_Beta  #Beta Matrix
Matx_Hat_part<-Matx_X%*%Matx_XtX_I
Matx_Hat<-Matx_Hat_part%*%Matx_X_t
Matx_Hat  
print("Value of H is : ")
Matx_SSE_basic<- Identity_Mat-Matx_Hat
Matx_SSE_basic
Matx_SSE_basic1<-Matx_Y_t%*%Matx_SSE_basic
Matx_SSE<-Matx_SSE_basic1%*%Matx_Y
print("Value of SSE is : ")
Matx_SSE
J<-matrix(1, nrow=17,ncol=17) # Square matrix J
Matx_J<-J/17 # n=17
Matx_SSR_part<-Matx_Hat-Matx_J
Matx_SSR<-Matx_Y_t%*%Matx_SSR_part%*%Matx_Y
print("Value of SSR is : ")
Matx_SSR

MSE<-Matx_SSE/14 # Calculate MSE
print("Value of MSE is : ")
MSE

MSR<-Matx_SSR/2 # Calculate MSR
print("Value of MSR is : ")
MSR

F0<-MSR/MSE #Calculate F0
print("Value of F0 is : ")
F0


