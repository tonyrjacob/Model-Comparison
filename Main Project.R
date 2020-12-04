###########For house price data
#-----------------------1) Multiple linear regression ----------------------------
#--------------Importing data and data visualisation -----------------------------
data1=read.csv(file.choose())
data1=data1[,-1]
data2=read.csv(file.choose())
data2=data2[,-1]
data=cbind(data1,data2)
head(data)
str(data)
#----------------------------Dividing into development & validation dataset-------
set.seed(100)
ids=sample(1:nrow(data),(nrow(data)*0.8))
train_ols=data[ids,]
test_ols=data[-ids,]
nrow(train_ols)
nrow(test_ols)
#-------------------- Data cleaning using olsr package--------------------------
model=lm(SalePrice~.,data=train_ols)
summary(model)
library(olsrr)
ols_plot_resid_qq(model)
ols_test_normality(model)
ols_test_breusch_pagan(model)
ols_coll_diag(model)
ols=ols_step_both_p(model, pent = 0.05, prem = 0.1,details = FALSE)
#-------- Multiple linear regression Model---------------------------------------
model=lm(SalePrice~GrLivArea+OverallQual+LotArea+BsmtQual_Ex+Condition2_PosN+TotalBsmtSF+Neighborhood_NoRidge+
            SaleType_New
         +OverallCond+BsmtFinType1_GLQ+MSSubClass+Neighborhood_StoneBr+ExterQual_Ex+Condition2_PosA+GarageArea+HouseS
         tyle_1.5Fin+MoSold+PoolArea+Exterior1st_BrkFace+LandContour_HLS+Neighborhood_Crawfor+BsmtExposure_Av
         +KitchenAbvGr+TotRmsAbvGrd+FireplaceQu_Ex+Exterior2nd_ImStucc
         +Heating_OthW+LandContour_Bnk+GarageQual_Po+MasVnrType_BrkFace+RoofMatl_CompShg+RoofMatl_Tar.Grv
         +RoofMatl_WdShake +RoofMatl_Roll +RoofMatl_Metal
         +HouseStyle_2.5Fin+RoofMatl_Membran+FullBath+LotShape_IR1+SaleType_Con+MasVnrType_None+Functional_Maj1+MasVnrT
         ype_BrkCmn+BsmtExposure_Gd,data=train_ols)
summary(model)
#---Prediction----------------------------------------------------------------
predicted=predict(model,newdata=test_ols)
mape=mean(abs(predicted-test_ols$SalePrice)/test_ols$SalePrice)*100
mape
#---Prediction- for train-------------------------------------------------------
predicted=predict(model,newdata=train_ols)
length(predicted)
mape=mean(abs(predicted-train_ols$SalePrice)/train_ols$SalePrice)*100
mape
#-----------------------------------------------------------------------------
#---------------------2) Regression Tree --------------------------------------
#-----------------------------------------------------------------------------
#-------------- Importing data and data Visualisation----------------------------
decision=read.csv(file.choose())
head(decision)
#------------------------Split the data into training and test set------------------
set.seed(100)
ids=sample(1:nrow(decision),(nrow(decision)*0.8))
train_de=decision[ids,]
test_de=decision[-ids,]
#----------------------Installing Library --------------------------------------
library(rpart)
library(rpart.plot)
#--------------- Inorder to find the cp value -------------------------------------
raptm=rpart(SalePrice~.,train_de,cp=0)
summary(raptm)
#---After finding the cp value, using the cp value we have calculated, build the modelmodel1=rpart(SalePrice~.,train_de,cp=1.575639e-02)
summary(model1)
#-----plotting decision tree----------------------------------------------------
rpart.plot(model1,extra='auto',box.palette="RdYlGn", 
branch.lty=3,shadow.col="gray",nn=TRUE,tweak =1)
#---Prediction foe test data----------------------------------------------------
predicted1=predict(model1,newdata=test_de)
head(predicted1)
length(predicted1)
mape=mean(abs(predicted1-test_de$SalePrice)/test_de$SalePrice)*100
mape
#---Prediction train data-------------------------------------------------------
predicted11=predict(model1,newdata=train_de)
head(predicted)
length(predicted)
mape=mean(abs(predicted11-train_de$SalePrice)/train_de$SalePrice)*100
mape
#-----------------------------------------------------------------------------
#----------------------- 3)RANDOM FOREST -----------------------------------
#-----------------------------------------------------------------------------
#-----Installing library, importing data and data visualization ----------------------
library(randomForest)
mydata=read.csv(file.choose())
head(mydata)
#-------- Dividing data into test and train ----------------------------------------
set.seed(100)
ids=sample(1:nrow(mydata),(nrow(mydata)*0.8))
trainrf=mydata[ids,]
nrow(trainrf)
head(trainrf)
testrf=mydata[-ids,]
nrow(testrf)
head(testrf)
#-------Find mtry & mtree values, using the minimum mean square error method ---
results_df <- data.frame(matrix(nrow=80,ncol=5))
variables = c(10,15,20,25,30)
for(j in 1:length(variables))
{
   mtry = variables[j]
   set.seed(100)
   rf<-randomForest(SalePrice~.,data=trainrf
                    ,mtry=mtry,ntree=80)
   results_df[,j]=rf$mse
}
colnames(results_df)[1]="oob_mtry=10"
colnames(results_df)[2]="oob_mtry=15"
colnames(results_df)[3]="oob_mtry=20"
colnames(results_df)[4]="oob_mtry=25"
colnames(results_df)[5]="oob_mtry=30"
results_df$tree=1:80

plot(x=results_df[,6],y=results_df[,1],col="black",xlab = "Trees",ylab = "mse")
lines(x=results_df[,6],y=results_df[,2],col="blue")
lines(x=results_df[,6],y=results_df[,3],col="green")
lines(x=results_df[,6],y=results_df[,4],col="yellow")
lines(x=results_df[,6],y=results_df[,5],col="red")
legend(18,2.4e+09+5e+08,legend=c("mtry=10, Col=Black", "mtry=15, Col=Blue",
                                 "mtry=20, Col=Green","mtry=25, col=Yellow","mtry=30, col=Red"))
#---- Using the mtry and ntree we have calculaed, building RANDOM FOREST FOR Regression--------------------------
set.seed(45)
rf=randomForest(SalePrice~.,data=trainrf,mtry=25,ntree=80)
summary(rf)
#------ Finding the importance of the varaibles ----------------------------------
importance(rf)
varImpPlot(rf)
#----------- Prediction for test data set-----------------------------------------
predicted_rf=predict(rf,testrf)
per_abs_error_rf=abs((testrf$SalePrice-predicted_rf)/testrf$SalePrice)
mape_rf=mean(per_abs_error_rf)*100
mape_rf
#------------Prediction for train data set----------------------------------------
predicted_rf_train=predict(rf,trainrf)
per_abs_error_rf_train=abs((trainrf$SalePrice-predicted_rf_train)/trainrf$SalePrice)
mape_rf_train=mean(per_abs_error_rf_train)*100 

mape_rf_train #----------------------------------------------------------------------------- #--------------------- 4)SVM ------------------------------------------------- #----------------------------------------------------------------------------- #-------Installing library,importing data sets and data visualisation----------------
library(e1071)
library(kernlab)
svmdata=read.csv(file.choose())
svmdata=svmdata[,
                -1]
head(svmdata)
dim(svmdata) #------------------ Dividing data into test and train ------------------------------
set.seed(100)
ids=sample(1:nrow(svmdata),(nrow(svmdata)*0.8))
train_svm=svmdata[ids,]
nrow(train_svm)
head(train_svm)
test_svm=svmdata[
   -ids,]
nrow(test)
head(test) #-----Taking the the independent variables alone for test and train data ------------
test_ind=test_svm[,
                  -1]
train_ind=train_svm[,
                    -1]
head(test_ind)
head(train_ind) #---------------- Building the SVM model for regression-------------------------
svm_model=svm(SalePrice~.,kernal="radial",
              ranges=list(cost=10^(
                 -1:2),gamma=c(0.5,1,2)),data=train_svm,probability=TRUE)
svm_model #--------- Prediction for test---------------------------------------------------
svm_predict=predict(svm_model,test_ind,probability=TRUE)
per_abs_error_rf=abs((test$SalePrice
                      -svm_predict)/test$SalePrice)
mape_svm=mean(per_abs_error_rf)*100
mape_svm #------------------ Prediction for train-----------------------------------------
svm_predict_train=predict(svm_model,train_ind,probability=TRUE)
per_abs_error_rf_train=abs((train$SalePrice
                            -svm_predict_train)/train$SalePrice)
mape_svm_train=mean(per_abs_error_rf_train)*100
mape_svm_train #------------------------------------------------5)KNN---------------------- #--------------Importing data and data visualisation -----------------------------
data=read.csv(file.choose())
head(data)
str(data)
dim(data)
head(data) #------------------------------Dividing into development & validation dataset-----
set.seed(100)
ids=sample(1:nrow(data),(nrow(data)*0.8))
train_knn=data[ids,]
test_knn=data[
   -ids,]
nrow(train_knn)
nrow(test_knn) #-------------- Installing the reqiured libraries----------------------------------
install.packages("rJava",type="sourse")
library(RWeka)
library(rminer) #----------------- Building the SVM model for KNN model for regression----------
knn_model=IBk(SalePrice~.,data=train_knn,control=Weka_control(K=20,X=TRUE))
knn_model
summary(knn_model) #------------------ Prediction for test -----------------------------------------
knn_predi=predict(knn_model,test_knn)
per_abs_error_rf=abs((test_knn$SalePrice
                      -knn_predi)/test_knn$SalePrice)
mape_knn=mean(per_abs_error_rf)*100
mape_knn #----------------- Prediction for train------------------------------------------
knn_predi_train=predict(knn_model,train_knn)
per_abs_error_rf=abs((train_knn$SalePrice
                      -knn_predi_train)/train_knn$SalePrice) 
mape_knn_tr=mean(per_abs_error_rf)*100
mape_knn_tr
#-----------------------------------------------------------------------------
#-----------------------------------6)XG boosting-----------------------------
#----Reading dataset----------------------------------------------------------
data=read.csv(file.choose())
head(data)
#---Dividing into development & validation dataset--------------------------------
set.seed(100)
ids=sample(1:nrow(data),(nrow(data)*0.8))
train_xg=data[ids,]
test_xg=data[-ids,]
nrow(train_xg)
nrow(test_xg)
head(train_xg)
names(train_xg)
head(test_xg)
#--- Installing the libraries and converting the data into matrix format for test and train data sets------------
library(Matrix)
library(xgboost)
trainm=sparse.model.matrix(SalePrice ~ .-1,train_xg)
train_label=train_xg[,"SalePrice"]
train_matrix=xgb.DMatrix(data=as.matrix(trainm),label=train_label)
testm=sparse.model.matrix(SalePrice ~ .-1,test_xg)
test_label=test_xg[,"SalePrice"]
test_matrix=xgb.DMatrix(data=as.matrix(testm),label=test_label)
#---setting parameters--------------------------------------------------------
xgb_parms=list("objective"="reg:linear","eval_metric"="rmse")
watchlist=list(train=train_matrix,test=test_matrix)
#------------------------ Building the XG boosting model for regression----------
model=xgb.train(xgb_parms,data=train_matrix,nrounds=1500,watchlist=watchlist,
                eta=0.01,max.depth=8,gamma=0,subsample=0.1,colsample_bytree=0.9,missing=NA,
                set.seed=100)
summary(model)
#---Feature importances------------------------------------------------------
imp=xgb.importance(colnames(train_matrix),model=model)
head(print(imp))
#---Prediction for test ---------------------------------------------------------
predicted_xg=predict(model,newdata=test_matrix)
head(predicted_xg)
length(predicted_xg)
mape_xg=mean(abs(predicted_xg-test_xg$SalePrice)/test_xg$SalePrice)*100
mape_xg
#---Prediction train-----------------------------------------------------------
predicted_xg_tr=predict(model,newdata=train_matrix)
head(predicted_xg_tr)
length(predicted_xg_tr)
mape_xg_tr=mean(abs(predicted_xg_tr-train_xg$SalePrice)/train_xg$SalePrice)*100
mape_xg_tr 