#clear all the variables in the environment
rm(list = ls())

#install the required packages
install.packages("ISLR")
#install.packages("caret")
install.packages("leaps")

#call the required packages
library(ISLR)
library(class)
#library(caret)
library(leaps)
install.packages("corrplot")
library(corrplot)
install.packages("car")
library(car)
install.packages("e1071")
require(e1071)
#install.packages("bootstrap")
#library(bootstrap)
library(rpart)
library(gbm)
#install.packages("gbm")
white_wine = read.csv("C:\\Users\\Anooj Kinagi\\Documents\\513\\project\\winequality-white.csv", header = TRUE, sep = ',')
#white_wine <- as.data.frame(white_wine)
View(white_wine)

#can be reffered directly
#attach(white_wine)
#summary(white_wine)
#summary(quality)
#typeof(fixed_acidity)
#summary(fixed_acidity)
#?lapply()
white_wine = data.frame(lapply(white_wine, function(x) as.numeric(as.character(x))))
#typeof(fixed_acidity)
View(white_wine)
white_wine <- as.numeric(white_wine)

############################################################################################
# Histogram for quality
############################################################################################

hist(white_wine$quality, main = "Histogram for Quality", xlab = "Quality", ylab = "count", col = "lightblue");

############################################################################################
# Histogram for all predictors
############################################################################################
?par()
par(mfrow = c(3,4))

hist(white_wine$fixed.acidity, main = "Histogram for fixed.acidity", xlab = "fixed.acidity", ylab = "count", col = "lightblue");
hist(white_wine$volatile.acidity, main = "Histogram for volatile.acidity", xlab = "volatile.acidity", ylab = "count", col = "lightblue");
hist(white_wine$citric.acid, main = "Histogram for citric.acid", xlab = "citric.acid", ylab = "count", col = "lightblue");
hist(white_wine$residual.sugar, main = "Histogram for residual.sugar", xlab = "residual.sugar", ylab = "count", col = "lightblue");
hist(white_wine$chlorides, main = "Histogram for chlorides", xlab = "chlorides", ylab = "count", col = "lightblue");
hist(white_wine$free.sulfur.dioxide, main = "Histogram for free.sulfur.dioxide", xlab = "free.sulfur.dioxide", ylab = "count", col = "lightblue");
hist(white_wine$total.sulfur.dioxide, main = "Histogram for total.sulfur.dioxide", xlab = "total.sulfur.dioxide", ylab = "count", col = "lightblue");
hist(white_wine$density, main = "Histogram for density", xlab = "density", ylab = "count", col = "lightblue");
hist(white_wine$pH, main = "Histogram for pH", xlab = "pH", ylab = "count", col = "lightblue");
hist(white_wine$sulphates, main = "Histogram for sulphates", xlab = "sulphates", ylab = "count", col = "lightblue");
hist(white_wine$alcohol, main = "Histogram for alcohol", xlab = "alcohol", ylab = "count", col = "lightblue");

#############################################################################################
# Box plot to check outliers
############################################################################################
# Box plot before removing outliers
############################################################################################

par(mfrow = c(3,4))
#boxplot(fixed.acidity)
boxplot(white_wine$fixed.acidity, horizontal = FALSE, col="slategray2", pch=19)
mtext("Fixed Acidity", cex=0.8, side=1, line=2)
boxplot(white_wine$volatile.acidity, horizontal = FALSE, col="slategray2", pch=19)
mtext("volatile Acidity", cex=0.8, side=1, line=2)
boxplot(white_wine$citric.acid, horizontal = FALSE, col="slategray2", pch=19)
mtext("citric Acidity", cex=0.8, side=1, line=2)
boxplot(white_wine$residual.sugar, horizontal = FALSE, col="slategray2", pch=19)
mtext("Residual sugar", cex=0.8, side=1, line=2)
boxplot(white_wine$chlorides, horizontal = FALSE, col="slategray2", pch=19)
mtext("Chlorides", cex=0.8, side=1, line=2)
boxplot(white_wine$free.sulfur.dioxide, horizontal = FALSE, col="slategray2", pch=19)
mtext("free sulfur dioxide", cex=0.8, side=1, line=2)
boxplot(white_wine$total.sulfur.dioxide,horizontal = FALSE, col="slategray2", pch=19)
mtext("total sulfur dioxide", cex=0.8, side=1, line=2)
boxplot(white_wine$density,horizontal = FALSE, col="slategray2", pch=19)
mtext("density", cex=0.8, side=1, line=2)
boxplot(white_wine$pH, horizontal = FALSE, col="slategray2", pch=19)
mtext("pH", cex=0.8, side=1, line=2)
boxplot(white_wine$sulphates,horizontal = FALSE, col="slategray2", pch=19)
mtext("Sulphates", cex=0.8, side=1, line=2)
boxplot(white_wine$alcohol,horizontal = FALSE, col="slategray2", pch=19)

mtext("Alcohol", cex=0.8, side=1, line=2)

###########################################################################################
# Removing the outliers
###########################################################################################
outliers = rep(0,11)

for (i in 1:11){
  t1 <- quantile(white_wine[,i], 0.75)
  t2 <- IQR(white_wine[,i], 0.75)
  outliers[i] <- t1 + 1.5*t2
}
white_wine_index = matrix(0, 4898, 11)
for (i in 1:4898)
  for (j in 1:11){
    if (white_wine[i,j] > outliers[j]) white_wine_index[i,j] = 1
  }
w_index = apply(white_wine_index, 1, sum)
white_wine_data = cbind(w_index, white_wine)
index = rep(0)

j = 1
for (i in 1:4898){
  if (w_index[i] > 0) {index[j]= i
  j = j + 1}
  else j = j
}

new_white_wine = white_wine[-index,]

#After removing outliers, we get 4074 observations
nrow(new_white_wine)
############################################################################################
# Box plot after removing outliers
############################################################################################

par(mfrow = c(3,4))

#boxplot(fixed.acidity)
boxplot(new_white_wine$fixed.acidity, horizontal = FALSE, col="slategray2", pch=19)
mtext("Fixed Acidity", cex=0.8, side=1, line=2)
boxplot(new_white_wine$volatile.acidity, horizontal = FALSE, col="slategray2", pch=19)
mtext("volatile Acidity", cex=0.8, side=1, line=2)
boxplot(new_white_wine$citric.acid, horizontal = FALSE, col="slategray2", pch=19)
mtext("citric Acidity", cex=0.8, side=1, line=2)
boxplot(new_white_wine$residual.sugar, horizontal = FALSE, col="slategray2", pch=19)
mtext("Residual sugar", cex=0.8, side=1, line=2)
boxplot(new_white_wine$chlorides, horizontal = FALSE, col="slategray2", pch=19)
mtext("Chlorides", cex=0.8, side=1, line=2)
boxplot(new_white_wine$free.sulfur.dioxide, horizontal = FALSE, col="slategray2", pch=19)
mtext("free sulfur dioxide", cex=0.8, side=1, line=2)
boxplot(new_white_wine$total.sulfur.dioxide,horizontal = FALSE, col="slategray2", pch=19)
mtext("total sulfur dioxide", cex=0.8, side=1, line=2)
boxplot(new_white_wine$density,horizontal = FALSE, col="slategray2", pch=19)
mtext("density", cex=0.8, side=1, line=2)
boxplot(new_white_wine$pH, horizontal = FALSE, col="slategray2", pch=19)
mtext("pH", cex=0.8, side=1, line=2)
boxplot(new_white_wine$sulphates,horizontal = FALSE, col="slategray2", pch=19)
mtext("Sulphates", cex=0.8, side=1, line=2)
boxplot(new_white_wine$alcohol,horizontal = FALSE, col="slategray2", pch=19)
mtext("Alcohol", cex=0.8, side=1, line=2)
##############################DATA CLEANING################################################
##############################DATA CLEANING################################################
##############################DATA CLEANING################################################

##########################################################################################
###########################################################################################
#correlation and feature selection based on correlation (Manual feature selection)
###########################################################################################
?cor()
cor_white_wine = cor(new_white_wine)
cor_white_wine
par(mfrow =c(1,1))
corrplot(cor_white_wine, method = "number")
#cor_white_wine 
# volatile.acidity, residual.sugar, chloride, density, pH, alcohol 
#Vignesh
# density, free.sulfur.dioxide, total.sulfur.dioxide, density, ph, alcohol

###########################################################################################
# collinearity
###########################################################################################
#new_white_wine_mfs = new_white_wine[,c("volatile.acidity","residual.sugar","chlorides","density","pH","alcohol","quality")]
#vignesh
new_white_wine_mfs = new_white_wine[,c("density","residual_sugar","chlorides","total_sulfur_dioxide","alcohol","quality")]
new_white_wine_copy = new_white_wine[,c("density","residual_sugar","chlorides","total_sulfur_dioxide","alcohol","quality")]
?lm()
fit1 = lm(quality~., data = new_white_wine_mfs)
summary(fit1)
vif(fit1)
which.max(vif(fit1)) # density has higher collinearity (16.7) so removed
#whitewine.revised = whitewine[,c("volatile.acidity","fixed.acidity","residual.sugar","free.sulfur.dioxide","density","pH","sulphates","alcohol","quality")]
#new_white_wine_mf = new_white_wine[,-c("density")]

new_white_wine_mfs = subset(new_white_wine_mfs,select = -c(density))
new_white_wine_mfsda <- subset(new_white_wine_mfs, select = -c(chlorides,total_sulfur_dioxide))

new_white_wine_mfsda
################################################
#########DATA TO WORK WITH######################
new_white_wine_mfsda #only density and alcohol
new_white_wine_mfs 
View(new_white_wine_copy)
View(new_white_wine_mfs)
################################################


# 1 Naive Bayes using density:
library(e1071)
new_white_wine_copy$residual.sugar <- as.factor(new_white_wine_copy$residual.sugar)
new_white_wine_copy$chlorides <- as.factor(new_white_wine_copy$chlorides)
new_white_wine_copy$total.sulfur.dioxide <- as.factor(new_white_wine_copy$total.sulfur.dioxide)
new_white_wine_copy$alcohol <- as.factor(new_white_wine_copy$alcohol)
new_white_wine_copy$quality <- as.factor(new_white_wine_copy$quality)
nb <- naiveBayes(quality~., data = new_white_wine_copy)
nb
tablenb <- predict(nb,new_white_wine_copy)
tablenb

?table()
table(nb=tablenb , Class = new_white_wine_copy$quality)
wrong <- sum(tablenb!=new_white_wine_copy$quality)
rate <- wrong/length(tablenb)
rate #37

# 2 Naive Bayes without density:
library(e1071)
new_white_wine_mfs$residual.sugar <- as.factor(new_white_wine_mfs$residual.sugar)
new_white_wine_mfs$chlorides <- as.factor(new_white_wine_mfs$chlorides)
new_white_wine_mfs$total.sulfur.dioxide <- as.factor(new_white_wine_mfs$total.sulfur.dioxide)
new_white_wine_mfs$alcohol <- as.factor(new_white_wine_mfs$alcohol)
new_white_wine_mfs$quality <- as.factor(new_white_wine_mfs$quality)
nb <- naiveBayes(quality~., data = new_white_wine_mfs)
nb
tablenb <- predict(nb,new_white_wine_mfs)
tablenb 

?table()
table(nb=tablenb , Class = new_white_wine_mfs$quality)
wrong <- sum(tablenb!=new_white_wine_mfs$quality)
rate <- wrong/length(tablenb)
rate  #41

# 3 Naive Bayes with alcohol and density:
library(e1071)
new_white_wine_mfsda$residual.sugar <- as.factor(new_white_wine_mfsda$residual.sugar)
new_white_wine_mfsda$alcohol <- as.factor(new_white_wine_mfsda$alcohol)
new_white_wine_mfsda$quality <- as.factor(new_white_wine_mfsda$quality)
nb <- naiveBayes(quality~., data = new_white_wine_mfsda)
nb
tablenb <- predict(nb,new_white_wine_mfsda)
tablenb 

?table()
table(nb=tablenb , Class = new_white_wine_mfsda$quality)
wrong <- sum(tablenb!=new_white_wine_mfsda$quality)
rate <- wrong/length(tablenb)
rate  #42 






#################RANDOM_FOREST###################
library(randomForest)



##USING DENSITY
indexrf<-sort(sample(nrow(new_white_wine_copy),round(.25*nrow(new_white_wine_copy))))
training<-new_white_wine_copy[-indexrf,]
test<-new_white_wine_copy[indexrf,]
#View(bcw)
fit <- randomForest(quality~density+residual_sugar+chlorides+total_sulfur_dioxide+alcohol, data=training, importance=TRUE, ntree=1000)
importance(fit)
varImpPlot(fit)
Prediction <- predict(fit, test)
table(actual=test[,6],Prediction)


wrong<- (test[,6]!=Prediction )
error_rate<-sum(wrong)/length(wrong)
error_rate #100



##without density
indexrf<-sort(sample(nrow(new_white_wine_mfs),round(.25*nrow(new_white_wine_mfs))))
training<-new_white_wine_mfs[-indexrf,]
test<-new_white_wine_mfs[indexrf,]
#View(bcw)
fit <- randomForest(quality~residual_sugar+chlorides+total_sulfur_dioxide+alcohol, data=training, importance=TRUE, ntree=1000)
importance(fit)
varImpPlot(fit)
Prediction <- predict(fit, test)
table(actual=test[,5],Prediction)


wrong<- (test[,5]!=Prediction )
error_rate<-sum(wrong)/length(wrong)
error_rate #100


## with alcohol and density:
indexrf<-sort(sample(nrow(new_white_wine_mfsda),round(.25*nrow(new_white_wine_mfsda))))
training<-new_white_wine_mfsda[-indexrf,]
test<-new_white_wine_mfsda[indexrf,]
#View(bcw)
fit <- randomForest(quality~residual_sugar+alcohol, data=training, importance=TRUE, ntree=1000)
importance(fit)
varImpPlot(fit)
Prediction <- predict(fit, test)
table(actual=test[,3],Prediction)


wrong<- (test[,3]!=Prediction )
error_rate<-sum(wrong)/length(wrong)
error_rate #100

##C50
#library('C50')
#View(dsn)
##USING DENSITY

new_white_wine_copy$residual_sugar <- as.factor(new_white_wine_copy$residual_sugar)
new_white_wine_copy$chlorides <- as.factor(new_white_wine_copy$chlorides)
new_white_wine_copy$total_sulfur_dioxide <- as.factor(new_white_wine_copy$total_sulfur_dioxide)
new_white_wine_copy$alcohol <- as.factor(new_white_wine_copy$alcohol)
new_white_wine_copy$quality <- as.factor(new_white_wine_copy$quality)



indexrf<-sort(sample(nrow(new_white_wine_copy),round(.25*nrow(new_white_wine_copy))))
training<-new_white_wine_copy[-indexrf,]
test<-new_white_wine_copy[indexrf,]
# C50  classification 
library('C50')
C50_class <- C5.0( quality~.,data=training )

summary(C50_class )
#dev.off()
#plot(C50_class)
C50_predict<-predict( C50_class ,test , type="class" )
table(actual=test[,6],C50=C50_predict)
wrong<- (test[,6]!=C50_predict)
c50_rate<-sum(wrong)/length(test[,6]) #51


##without density
new_white_wine_mfs$residual_sugar <- as.factor(new_white_wine_mfs$residual_sugar)
new_white_wine_mfs$chlorides <- as.factor(new_white_wine_mfs$chlorides)
new_white_wine_mfs$total_sulfur_dioxide <- as.factor(new_white_wine_mfs$total_sulfur_dioxide)
new_white_wine_mfs$alcohol <- as.factor(new_white_wine_mfs$alcohol)
new_white_wine_mfs$quality <- as.factor(new_white_wine_mfs$quality)


indexrf<-sort(sample(nrow(new_white_wine_mfs),round(.25*nrow(new_white_wine_mfs))))
training<-new_white_wine_mfs[-indexrf,]
test<-new_white_wine_mfs[indexrf,]
# C50  classification 
library('C50')
C50_class <- C5.0( quality~.,data=training )

summary(C50_class )
#dev.off()
#plot(C50_class)
C50_predict<-predict( C50_class ,test , type="class" )
table(actual=test[,5],C50=C50_predict)
wrong<- (test[,5]!=C50_predict)
c50_rate<-sum(wrong)/length(test[,5]) #46


##with alcohol and density
new_white_wine_mfsda$residual_sugar <- as.factor(new_white_wine_mfsda$residual_sugar)
new_white_wine_mfsda$alcohol <- as.factor(new_white_wine_mfsda$alcohol)
new_white_wine_mfsda$quality <- as.factor(new_white_wine_mfsda$quality)

indexrf<-sort(sample(nrow(new_white_wine_mfsda),round(.25*nrow(new_white_wine_mfsda))))
training<-new_white_wine_mfsda[-indexrf,]
test<-new_white_wine_mfsda[indexrf,]
# C50  classification 
library('C50')
C50_class <- C5.0( quality~.,data=training )

summary(C50_class )
#dev.off()
#plot(C50_class)
C50_predict<-predict( C50_class ,test , type="class" )
table(actual=test[,3],C50=C50_predict)
wrong<- (test[,3]!=C50_predict)
c50_rate<-sum(wrong)/length(test[,3]) #48

#KNN
#USING DENSITY
library(class)
indexknn<-sort(sample(nrow(new_white_wine_copy),round(.25*nrow(new_white_wine_copy))))
training<-new_white_wine_copy[-indexknn,]
test<-new_white_wine_copy[indexrf,]

KNN <- knn(training[,-6],test[,-6],training[,6],k=1)
table(Predict=KNN, Actual=test[,6])
wrong<- (test[,6]!=KNN)
knnerror<-sum(wrong)/length(test[,6]) #10
knnerror

#without density
indexknn<-sort(sample(nrow(new_white_wine_mfs),round(.25*nrow(new_white_wine_mfs))))
training<-new_white_wine_mfs[-indexknn,]
test<-new_white_wine_mfs[indexrf,]

KNN <- knn(training[,-5],test[,-5],training[,5],k=1)
table(Predict=KNN, Actual=test[,5])
wrong<- (test[,5]!=KNN)
knnerror<-sum(wrong)/length(test[,5]) #12


#with alcohol and density
indexknn<-sort(sample(nrow(new_white_wine_mfsda),round(.25*nrow(new_white_wine_mfsda))))
training<-new_white_wine_mfsda[-indexknn,]
test<-new_white_wine_mfsda[indexrf,]

KNN <- knn(training[,-3],test[,-3],training[,3],k=1)
table(Predict=KNN, Actual=test[,3])
wrong<- (test[,3]!=KNN)
knnerror<-sum(wrong)/length(test[,3]) #25