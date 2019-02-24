install.packages(c('caret','dplyr','rpart','MASS','AUC','broom','ggplot2','randomForest','clustMixT ype'))
library(caret) library(dplyr) library(rpart) library(MASS) library(AUC) library(broom)
library(ggplot2) ## package for producing graphical plots library(randomForest)
library(clustMixType) library(arules) library(klaR) library(e1071) library(dummies)
library(gridExtra) ## package for drawing multiple graphs on a grid


############################################### ############################################### ############# import data #################### ############################################### ###############################################

datatrainurl <- 'http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data' datatesturl <-'http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.test' datanameurl <- 'http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.names' adulttrain <- read.table(datatrainurl,sep = ',',stringsAsFactors = FALSE)
adulttest <- readLines(datatesturl)[-1]
adulttest <- read.table(textConnection(adulttest),sep = ',',stringsAsFactors = FALSE)

adultnames <- readLines(datanameurl)[97:110]
adultnames <- as.character(lapply(strsplit(adultnames,':'), function(x) x[1])) adultnames <- c(adultnames,'income')
colnames(adulttrain) <- adultnames colnames(adulttest) <- adultnames

str(adulttrain)

# We first remove missing value (ones with ' ?')
no.question.mark <- apply(adulttrain, 1, function(r) !any(r %in% ' ?'))
 
adulttrain <- adulttrain[no.question.mark,]

no.question.mark <- apply(adulttest, 1, function(r) !any(r %in% ' ?')) 
adulttest <- adulttest[no.question.mark,]

adulttrain <- as.data.frame(unclass(adulttrain),stringsAsFactors = T) 
adulttest <- as.data.frame(unclass(adulttest),stringsAsFactors = T)

############################################################################## ################## ############################################################################## ##################
#############code for: Visulization, Association Rule, Dimension Reduction ##################### ############################################################################## ################## ############################################################################## ##################

Adultdata<-rbind(adulttrain,adulttest)
Adultdata$income <- gsub(".","",as.character(Adultdata$income),fixed=TRUE) # remove outliers #
no.outlier<-function(data,x)
{
for (i in x)
{
a<-boxplot.stats(data[,i])$out 
data<-data[!data[,i]%in%a,] 
print(length(a))
}
return(data)
}
str(adulttrain)
Adultdata<- no.outlier(Adultdata,c(1,3)) 
adulttrain <- no.outlier(adulttrain,c(1,3)) 
adulttest <- no.outlier(adulttest,c(1,3))

#------------------------------------------------------------------------
### Visualization
## Detecting skewed variables skewedVars<- NA library(moments) # for skewness() for(i in names(Adultdata)){ if(is.numeric(Adultdata[,i])){
 
if(i != "income"){
# Enters this block if variable is non-categorical skewVal <- skewness(Adultdata[,i]) print(paste(i, skewVal, sep = ": ")) if(abs(skewVal) > 0.5){
skewedVars <- c(skewedVars, i)
   }
  }
 }
}
N.obs<-dim(Adultdata)[1] N.var<-dim(Adultdata)[2]
## Explore Numerical Variable
## Correlation between numerical variables numeric.var <- sapply(Adultdata, is.numeric) ## Calculate the correlation matrix
corr <- cor(Adultdata[,numeric.var]) corr

p1 <- ggplot(Adultdata, aes(x=age)) + ggtitle("Histogram of Age") + geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth=5, colour="black", fill="salmon") + ylab("Percentage")
grid.arrange(p1)
p2 <- ggplot(Adultdata, aes(x=log10(fnlwgt))) + ggtitle("Histogram of Log(fnlwgt)") + geom_histogram(aes(y = 100*(..count..)/sum(..count..)), colour="black", fill="salmon") + ylab("Percentage")
grid.arrange(p2)
p3 <- ggplot(Adultdata, aes(x=education.num)) + ggtitle("Histogram of Educationnum") + geom_histogram(aes(y = 100*(..count..)/sum(..count..)), binwidth=1, colour="black", fill="salmon") + ylab("Percentage")
grid.arrange(p3)
p4 <- ggplot(Adultdata, aes(x=hours.per.week)) + ggtitle("Histogram of Hours per Week") + geom_histogram(aes(y = 100*(..count..)/sum(..count..)), colour="black", fill="salmon") + ylab("Percentage")
grid.arrange(p4)
p5 <- ggplot(Adultdata, aes(x=log10(capital.gain+1))) + ggtitle("Histogram Log(Capital Gain)") + geom_histogram(aes(y = 100*(..count..)/sum(..count..)), colour="black", fill="salmon") + ylab("Percentage")
grid.arrange(p5)
# numbers of data with zero Capital Gain
(CG <- sum(Adultdata$capital.gain==0)/N.obs*100 )
p6 <- ggplot(Adultdata, aes(x=log10(capital.loss+1))) + ggtitle("Histogram of log(Capital Loss)") + geom_histogram(aes(y = 100*(..count..)/sum(..count..)), colour="black", fill="salmon") + ylab("Percentage")
 
grid.arrange(p6)
# number of data with zero Capital Loss
(CL <- sum(Adultdata$capital.loss==0)/N.obs*100)

# remove the variable: capital.gain and capital.loss Adultdata[["capital.gain"]] <- NULL Adultdata[["capital.loss"]] <- NULL

## Explore Categorical Data
# Sort categorical variables in descending order
categ.sort <- function(x){reorder(x,x,function(y){-length(y)})} ## Sorting function for categorical variables
categ.var <- which(sapply(Adultdata, is.factor)) ## Find the categorical variables for (c in categ.var){ ## Apply the sort function on each categorical variable Adultdata[,c] <- categ.sort(Adultdata[,c])
}
attach(Adultdata)
p1 <- ggplot(Adultdata, aes(x=Adultdata$workclass)) + ggtitle("Histogram of Work Class") + xlab("Work Class") +
geom_bar(aes(y = 100*(..count..)/sum(..count..)),colour="black", fill="salmon") + ylab("Percentage") +
scale_x_discrete(limits = levels(Adultdata$workclass)) grid.arrange(p1)
p2 <- ggplot(Adultdata, aes(x=Adultdata$education)) + ggtitle("Histogram of Education") + xlab("Education") +
geom_bar(aes(y = 100*(..count..)/sum(..count..)),colour="black", fill="salmon") + ylab("Percentage") +
scale_x_discrete(limits = levels(Adultdata$education)) grid.arrange(p2)
p3 <- ggplot(Adultdata, aes(x=Adultdata$marital.status)) + ggtitle("Histogram of Marital Status")
+ xlab("Marital Status") +
geom_bar(aes(y = 100*(..count..)/sum(..count..)),colour="black", fill="salmon") + ylab("Percentage") +
scale_x_discrete(limits = levels(Adultdata$marital.status)) grid.arrange(p3)
p4 <- ggplot(Adultdata, aes(x=occupation)) + ggtitle("Histogram of Occupation") + xlab("Occupation") +
geom_bar(aes(y = 100*(..count..)/sum(..count..)),colour="black", fill="salmon") + ylab("Percentage") +
scale_x_discrete(limits = levels(Adultdata$occupation)) grid.arrange(p4)
p5 <- ggplot(Adultdata, aes(x=relationship)) + ggtitle("Histogram of Relationship") + xlab("Relationship") +
geom_bar(aes(y = 100*(..count..)/sum(..count..)),colour="black", fill="salmon") +
 
ylab("Percentage") +
scale_x_discrete(limits = levels(Adultdata$relationship)) grid.arrange(p5)
p6 <- ggplot(Adultdata, aes(x=race)) + ggtitle("Histogram of Race") + xlab("Race") + geom_bar(aes(y = 100*(..count..)/sum(..count..)),colour="black", fill="salmon") + ylab("Percentage") +
scale_x_discrete(limits = levels(Adultdata$race)) grid.arrange(p6)
p7 <- ggplot(Adultdata, aes(x=sex)) + ggtitle("Histogram of sex") + xlab("sex") + geom_bar(aes(y = 100*(..count..)/sum(..count..)),colour="black", fill="salmon") + ylab("Percentage") +
scale_x_discrete(limits = levels(sex)) grid.arrange(p7)
p8 <- ggplot(Adultdata, aes(x=native.country)) + ggtitle("Histogram of Native Country") + xlab("Native Country") +
geom_bar(aes(y = 100*(..count..)/sum(..count..)),colour="black", fill="salmon") + ylab("Percentage") +
scale_x_discrete(limits = levels(native.country)) grid.arrange(p8)

# remove the variable: nativecountry Adultdata[["native.country"]] <- NULL

#------------------------------------------
# Correlation between numerical variables and income class
boxplot (log(fnlwgt)~income, data =Adultdata, main = "Log(fnlwgt) for different income levels", xlab = "Income Levels", ylab = "Fnlwgt", col = "salmon")
boxplot (age~income, data =Adultdata, main = "Age for different income levels", xlab = "Income Levels", ylab = "Age", col = "salmon")
boxplot (education.num~income, data =Adultdata, main = "Years of Eduction for different income levels",
xlab = "Income Levels", ylab = "Years of Eduction", col = "salmon")
boxplot (hours.per.week~income, data =Adultdata, main = "hoursperweek for different income levels",
xlab = "Income Levels", ylab = "Hours per week", col = "salmon") # remove the variable: fnlwgt
Adultdata[["fnlwgt"]] <- NULL

#------------------------------------------
# Correlation between categorical variables and income class qplot(income, data = Adultdata, fill = workclass) + facet_grid (. ~ workclass) qplot(income, data = Adultdata, fill = education) + facet_grid (. ~ education)
qplot(income, data = Adultdata, fill = occupation) + facet_grid (. ~ occupation) qplot(income, data = Adultdata, fill = marital.status) + facet_grid (. ~ marital.status)
 
qplot(income, data = Adultdata, fill = relationship) + facet_grid (. ~ relationship) qplot(income, data = Adultdata, fill = race) + facet_grid (. ~ race)
qplot(income, data = Adultdata, fill = sex) + facet_grid (. ~ sex)

dim(Adultdata) ########################################################

#------------------------------------------------------------------------
### association rule colnames(Adultdata)[5] <- "maritalstatus"
colnames(Adultdata)[10] <- "hoursperweek" Adultdata[["education.num"]] <- NULL
Adultdata[["age"]] <- ordered(cut(Adultdata[["age"]], c(15, 25, 45, 65, 100)), labels = c("Young", "Middle-aged","Senior", "Old"))
Adultdata[["hoursperweek"]] <- ordered(cut(Adultdata[["hoursperweek"]],c(0, 25, 40, 60, 168)), labels = c("Part-time", "Full-time","Over-time", "Workaholic")) Adultdata$income=as.factor(as.character(Adultdata$income))
Adult0 <- as(Adultdata, "transactions") summary(Adult0)
itemLabels(Adult0)
rules <- apriori(Adult0, parameter = list(support = 0.01, confidence = 0.7)) summary(rules)
rulesIncomeSmall <- subset(rules, subset = rhs %in% "income= <=50K" &lift > 1.2) rulesIncomeLarge <- subset(rules, subset = rhs %in% "income= >50K" &lift > 1.2) inspect(sort(rulesIncomeSmall, by = "confidence")[1:10]) inspect(sort(rulesIncomeLarge, by = "confidence")[1:10])


#-----------------------------------------------------------------------
########## Dimension Reduction ########## census<-adulttrain
Adult<-adulttrain Adult_test<-adulttest
Adult_test$income<-as.factor(Adult_test$income) str(Adult_test)
summary(Adult)
#------- MCA ---------#
library(gtools)
cont<-c(3,5,11,12,13) #the continuous variables that will be split into quartiles for (i in cont){Adult[,i]<-quantcut(Adult[,i])}
for(i in cont) levels(Adult[,i]) <- paste(colnames(Adult)[i],levels(Adult[,i]))

# Age variable
Adult[,1]<- cut(Adult$age, breaks = c(0,25,35,49,64,75))
 
levels(Adult[,1])<-c("age:25 and under", "age:26 to 35", "age:36 to 49", "age:50 to 64", "age:65 and up")
summary(Adult) str(Adult)

library(PCAmixdata)
x.quanti<-census[,c(1,3,5,11,12,13)]
x.quali<-census[,-c(1,3,5,11,12,13,15)] PCAmix(x.quanti,x.quali)

library(FactoMineR)
res<-MCA(Adult,quali.sup = c(15)) summary(res,ncp=3,nbelements=Inf) plot(res,label=c("var","quali.sup")) # too loaded
plot(res,invisible=c("ind","quali.sup"),autoLab="y",cex=0.7) # plot active variables (categories) plot(res,invisible=c("ind"),cex=0.7,selectMod="contrib 20",unselect="grey90") # 20 variables contributed most
plot(res,invisible=c("ind"),autoLab="y",cex=0.7,selectMod="cos2 20",unselect="grey90") # 20 variables most correlated
plot(res, invisible=c("ind","var")) # illustrative variable (target modalities) dimdesc(res)

# Visualize eigenvalues (scree plot) # eig.val <- res$eig
barplot(eig.val[, 2],
names.arg = 1:nrow(eig.val),
main = "Variances Explained by Dimensions (%)", xlab = "Principal Dimensions",
ylab = "Percentage of variances", col ="steelblue")
lines(x = 1:nrow(eig.val), eig.val[, 2], # Add connected line segments to the plot type = "l",pch = 19,col = "red")

#-------- Variable clustering --------#
x.quanti<-census[,c(1,3,5,11,12,13)]
x.quali<-census[,-c(1,3,5,11,12,13,15)] library(ClustOfVar)
#run variable clustering excluding the target variable (income) variable_tree <- hclustvar(X.quali = x.quali, X.quanti = x.quanti) #plot the dendrogram of variable groups
plot(variable_tree) stability(variable_tree, B=25)
############################################################################## ######################
 
############################################################################## ######################
###########Back to introduction: data understanding,supervised and unsupervised Learning############ ############################################################################## ###################### ############################################################################## ######################
# Display the histogram par(mfrow=c(2,3)) hist(adulttrain$age) hist(adulttrain$education.num) hist(adulttrain$hours.per.week) hist(adulttrain$capital.gain) hist(adulttrain$capital.loss)

par(mfrow=c(2,3)) hist(adulttest$age) hist(adulttest$education.num) hist(adulttest$hours.per.week) hist(adulttest$capital.gain) hist(adulttest$capital.loss)

# Display barplots par(mfrow=c(2,3))
barplot(table(adulttrain$marital.status),main='marital.status') barplot(table(adulttrain$occupation),main='occupation') barplot(table(adulttrain$relationship),main='relationship') barplot(table(adulttrain$race),main='race') barplot(table(adulttrain$sex),main='sex') barplot(table(adulttrain$native.country),main='native.country')

par(mfrow=c(2,3)) barplot(table(adulttest$marital.status),main='marital.status') barplot(table(adulttest$occupation),main='occupation') barplot(table(adulttest$relationship),main='relationship') barplot(table(adulttest$race),main='race') barplot(table(adulttest$sex),main='sex') barplot(table(adulttest$native.country),main='native.country')

length(adulttrain$capital.gain[adulttrain$capital.gain != 0])/length(adulttrain$capital.gain)+length(adulttrain$capital.loss[adulttrain$capital.loss != 0])/length(adulttrain$capital.loss)
length(adulttest$capital.gain[adulttest$capital.gain !=
 
0])/length(adulttest$capital.gain)+length(adulttest$capital.loss[adulttest$capital.loss != 0])/length(adulttest$capital.loss)

# Delete variables adulttrain$education <- NULL adulttrain$native.country <- NULL adulttest$education <- NULL adulttest$native.country <- NULL

adulttrain$capital.change <- adulttrain$capital.gain - adulttrain$capital.loss adulttest$capital.change <- adulttest$capital.gain - adulttest$capital.loss adulttrain$capital.gain <- NULL
adulttrain$capital.loss<-NULL adulttest$capital.gain <- NULL adulttest$capital.loss<-NULL

# swap capital.change and income adulttrain[c(11,12)] <- adulttrain[c(12,11)]
colnames(adulttrain)[11:12] <- colnames(adulttrain)[12:11] adulttest[c(11,12)] <- adulttest[c(12,11)] colnames(adulttest)[11:12] <- colnames(adulttest)[12:11]

adulttrain$income <- as.factor(ifelse(adulttrain$income == ' <=50K',0,1)) adulttest$income <- as.factor(ifelse(adulttest$income == ' <=50K.',0,1)) str(adulttrain)
str(adulttest)

no.outlier<-function(data,x)
{
for (i in x)
{
a<-boxplot.stats(data[,i])$out data<-data[!data[,i]%in%a,] print(length(a))
}
return(data)
}
str(adulttrain)

adulttrain <- no.outlier(adulttrain,c(1,3)) adulttest <- no.outlier(adulttest,c(1,3))

adulttrain$fnlwgt <- NULL adulttest$fnlwgt <- NULL
 
dim(adulttrain) dim(adulttest)
income <- adulttest$income adulttest <- adulttest[,-11]


############## Unsupervised Learning#############################
# 1.---------- K-Prototype Clustering ------------#
# Check for the optimal number of clusters given the data set.seed(123)
wss<-vector()
for (i in 2:15){ wss[i] <- sum(kproto(adulttrain, i)$withinss)} plot(1:15, wss, type="b", xlab="number of clusters",
ylab="withinss",
main="Optimal Number of Clusters", pch=20, cex=2)
# From the plot we conclude that 6 is the best number.

proto<-kproto(adulttrain, k=6) adulttrain$cluster = as.factor(proto$cluster) clprofiles(proto, adulttrain)
plot(adulttrain$age,adulttrain$education.num,col=rainbow(6)[adulttrain$cluster],main='age vs education.num',pch=19) plot(adulttrain$age,adulttrain$hours.per.week,col=rainbow(6)[adulttrain$cluster],main='age vs hours.per.week',pch=19) plot(adulttrain$age,adulttrain$capital.change,col=rainbow(6)[adulttrain$cluster],main='age vs capital.change',pch=19) plot(adulttrain$education.num,adulttrain$hours.per.week,col=rainbow(6)[adulttrain$cluster], main='education.num vs hours.per.week',pch=19) plot(adulttrain$education.num,adulttrain$capital.change,col=rainbow(6)[adulttrain$cluster],m ain='education.num vs capital.change',pch=19) plot(adulttrain$hours.per.week,adulttrain$capital.change,col=rainbow(6)[adulttrain$cluster],m ain='hours.per.week vs capital.change',pch=19)

######################### Supervised Learning #########################
adulttrain$cluster<-NULL set.seed(1)
# 1.---------- Regression Tree------------#
tree <- rpart(income ~ ., data = adulttrain, method = 'class') pred.tree <- predict(tree, newdata =adulttest, type = 'class') confusionMatrix(pred.tree ,income)


# 2.---------- Random Forest ------------#
 
set.seed(123)
rf.adult <- randomForest(adulttrain$income ~ ., data=adulttrain,mtry=sqrt(10),importance=TRUE) 
rf.adult
rf.hpred <- predict(rf.adult,newdata=adulttest,type="class") 
confusionMatrix(rf.hpred,income) 
varImpPlot(rf.adult,type=2)

# 3. #---------- Logistic Regression ------------#
logistfit<- glm(income ~ .,data=adulttrain,family=binomial(link='logit')) summary(logistfit)
null_model<- glm(income ~ 1, data = adulttrain, family = binomial('logit')) # backward selection
fwd_aic <- step(logistfit, trace = F, scope = list(lower=formula(null_model),
upper=formula(logistfit)),
direction = 'forward')
fwd_aic
# Logistic Regression Prediction
preds <- predict(logistfit,newdata=adulttest,type='response') preds <- ifelse(preds > 0.5,1,0)
# Accuracy confusionMatrix(as.factor(preds),income) # 0.8402
#census$capital.change <- census$capital.gain-census$capital.loss #adulttest$capital.change <- adulttest$capital.gain-adulttest$capital.loss #adulttrain<-census[,-c(3,4,11,12,14)]
#adulttest<-adulttest[,-c(3,4,11,12,14)]

# 4. ----------- Naive Bayes -----------#
set.seed(2)
NB_model<-naiveBayes(income~.,data = adulttrain) NB_prediction<-predict(NB_model,adulttest) confusionMatrix(NB_prediction,income )
# 5. ---------- SVM -----------#
set.seed(3)
svm.model<- svm(income~., data = adulttrain,kernel = "radial", cost = 1, gamma = 0.1,scale=TRUE)
svm.predict <- predict(svm.model, adulttest) confusionMatrix(svm.predict,income) #table(svm.predict,Adult.tdummy[,10])
 
# 6. ---------- KNN ------------#
## Convert categorical variables to numerical data (Generate dummy variables) ## ## training set ##
# create dummy variables (training set) # #set.seed()
library(dummies)
Adult.dummy <- dummy.data.frame(adulttrain[,-11], sep = ".") Adult.dummy<-cbind(Adult.dummy,adulttrain$income) colnames(Adult.dummy)[46] <- "income" names(Adult.dummy)
Adult.dummy<-Adult.dummy[,-c(2,10,17,31,37,42)] # remove first dummy #

# scale numerical variables # ## 2 sd ##
#Adult.dummy$age<-(Adult.dummy$age-mean(adulttrain$age))/(sd(adulttrain$age))/2 #Adult.dummy$education.num<-(Adult.dummy$education.num- mean(adulttrain$education.num))/(sd(adulttrain$education.num))/2 #Adult.dummy$hours.per.week<-(Adult.dummy$hours.per.week- mean(adulttrain$hours.per.week))/(sd(adulttrain$hours.per.week))/2
## 1 sd ##
#Adult.dummy$age<-(Adult.dummy$age-mean(adulttrain$age))/sd(adulttrain$age) #Adult.dummy$education.num<-(Adult.dummy$education.num- mean(adulttrain$education.num))/sd(adulttrain$education.num) #Adult.dummy$hours.per.week<-(Adult.dummy$hours.per.week- mean(adulttrain$hours.per.week))/sd(adulttrain$hours.per.week)
## max-min ##
Adult.dummy$age<-(Adult.dummy$age-min(adulttrain$age))/(max(adulttrain$age)- min(adulttrain$age))
Adult.dummy$education.num<-(Adult.dummy$education.num- min(adulttrain$education.num))/(max(adulttrain$education.num)- min(adulttrain$education.num))
Adult.dummy$hours.per.week<-(Adult.dummy$hours.per.week- min(adulttrain$hours.per.week))/(max(adulttrain$hours.per.week)- min(adulttrain$hours.per.week))
Adult.dummy$capital.change<-(Adult.dummy$capital.change- min(adulttrain$capital.change))/(max(adulttrain$capital.change)- min(adulttrain$capital.change))

## test set ##
# create dummy variables (test set) #
Adult.tdummy <- dummy.data.frame(adulttest, sep = ".") #Adult.tdummy<-cbind(Adult.tdummy,income) str(Adult.tdummy)
#colnames(Adult.tdummy)[46] <- "income" income==40
 
names(Adult.tdummy)
Adult.tdummy<-Adult.tdummy[,-c(2,10,17,31,37,42)] # remove first dummy #

# scale numerical variables # ## 2 sd ##
#Adult.tdummy$age<-(Adult.tdummy$age-mean(adulttrain$age))/(sd(adulttrain$age))/2 #Adult.tdummy$education.num<-(Adult.tdummy$education.num- mean(adulttrain$education.num))/(sd(adulttrain$education.num))/2 #Adult.tdummy$hours.per.week<-(Adult.tdummy$hours.per.week- mean(adulttrain$hours.per.week))/(sd(adulttrain$hours.per.week))/2
## 1 sd ##
#Adult.tdummy$age<-(Adult.tdummy$age-mean(adulttrain$age))/sd(adulttrain$age) #Adult.tdummy$education.num<-(Adult.tdummy$education.num- mean(adulttrain$education.num))/sd(adulttrain$education.num) #Adult.tdummy$hours.per.week<-(Adult.tdummy$hours.per.week- mean(adulttrain$hours.per.week))/sd(adulttrain$hours.per.week)
## max-min ##
Adult.tdummy$age<-(Adult.tdummy$age-min(adulttrain$age))/(max(adulttrain$age)- min(adulttrain$age))
Adult.tdummy$education.num<-(Adult.tdummy$education.num- min(adulttrain$education.num))/(max(adulttrain$education.num)- min(adulttrain$education.num))
Adult.tdummy$hours.per.week<-(Adult.tdummy$hours.per.week- min(adulttrain$hours.per.week))/(max(adulttrain$hours.per.week)- min(adulttrain$hours.per.week))
Adult.tdummy$capital.change<-(Adult.tdummy$capital.change- min(adulttrain$capital.change))/(max(adulttrain$capital.change)- min(adulttrain$capital.change))
# K=5 #
library(class) set.seed(5)
knnpred <- knn(Adult.dummy[, -40],Adult.tdummy,Adult.dummy[,40], k = 5) confusionMatrix(knnpred,income)
# k=10 #
set.seed(6)
knnpred <- knn(Adult.dummy[,-40],Adult.tdummy,Adult.dummy[,40], k = 10) confusionMatrix(knnpred,income)
# k=15 #
set.seed(7)
knnpred <- knn(Adult.dummy[,-40],Adult.tdummy,Adult.dummy[,40], k = 15) confusionMatrix(knnpred,income)
# k=the square-root of the number of observations # set.seed(8)
knnpred <- knn(Adult.dummy[,-40],Adult.tdummy,Adult.dummy[,40], k =
 
round(sqrt(nrow(Adult.dummy)))) confusionMatrix(knnpred,income)

# 7. ---------- Neural Network ------------#
set.seed(4) library(nnet)
nn1 <- nnet(income~., data = adulttrain, size = 40, maxit = 500, MaxNWts=2000)
summary(nn1)
nn1.pred <- predict(nn1, newdata = adulttest, type = 'class') nn1.pred <- as.factor(nn1.pred) confusionMatrix(nn1.pred,income)

############################################################################## ##### ############################################################################## #####
#############Conclusion: Evaluate the performance using ROC Curve ################# ############################################################################## ##### ############################################################################## #####

trueYes <- income

# Classification Trees
treepreds <- predict(tree,newdata=adulttest,type="prob") posteriorYes.tree <- treepreds[,2]
ROCres.tree <- roc(posteriorYes.tree,trueYes) tidyROCres.tree <- tidy(ROCres.tree)

# Random Forest
rfpreds <- predict(rf.adult,newdata=adulttest,type="prob") posteriorYes.rf <- rfpreds[,2]
ROCres.rf <- roc(posteriorYes.rf,trueYes) tidyROCres.rf <- tidy(ROCres.rf)

# Logistic Regression
logitpreds <- predict(logistfit,newdata=adulttest,type='response') posteriorYes.logit <- logitpreds
ROCres.logit <- roc(posteriorYes.logit,trueYes) tidyROCres.logit <- tidy(ROCres.logit)

# Naive Bayes
 
nbpreds <-predict(NB_model,adulttest,type='raw') posteriorYes.nb <- nbpreds[,2]
ROCres.nb <- roc(posteriorYes.nb,trueYes) tidyROCres.nb <- tidy(ROCres.nb)

# SVM
set.seed(3)
svm.model1 <- svm(income~., data = adulttrain,kernel = "radial", cost = 1, gamma = 0.1,scale=TRUE,probability=T)
svmpreds <- predict(svm.model1, adulttest,probability = T) 
posteriorYes.svm <- attr(svmpreds, "probabilities")[,2] 
ROCres.svm <- roc(posteriorYes.svm,trueYes) 
tidyROCres.svm <- tidy(ROCres.svm)

# KNN
set.seed(6)
knnpreds <- knn(Adult.dummy[, -40],Adult.tdummy,Adult.dummy[,40], k = 5,prob = T) posteriorYes.knn <- knnpreds
ROCres.knn <- roc(posteriorYes.knn,trueYes) tidyROCres.knn <- tidy(ROCres.knn)

# Neural Network
nnpreds <- predict(nn1, newdata = adulttest, type = 'raw') posteriorYes.nn <- nnpreds
ROCres.nn <- roc(posteriorYes.nn,trueYes) tidyROCres.nn <- tidy(ROCres.nn)
# ROC Curve ggplot() +
geom_line(data=tidyROCres.tree,aes(x=fpr,y=tpr,color='Classification Trees')) +
geom_line(data=tidyROCres.rf,aes(x=fpr,y=tpr,color='Random Forest')) + geom_line(data=tidyROCres.logit,aes(x=fpr,y=tpr,color='Logistic Regression')) + geom_line(data=tidyROCres.nb,aes(x=fpr,y=tpr,color='Naive Bayes')) + geom_line(data=tidyROCres.svm,aes(x=fpr,y=tpr,color='SVM')) + geom_line(data=tidyROCres.knn,aes(x=fpr,y=tpr,color='KNN')) + geom_line(data=tidyROCres.nn,aes(x=fpr,y=tpr,color='Neural Network')) + geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1)) +
geom_abline(slope=1, intercept=0, linetype=2) + ggtitle('ROC Curves for various models')

######################### ROC Curve END ####################### ######################### AUC #########################
 
require(AUC) aucs <-
c(auc(ROCres.tree),auc(ROCres.rf),auc(ROCres.logit),auc(ROCres.nb),auc(ROCres.svm),auc(ROCr es.knn),auc(ROCres.nn))
aucs.df <- data.frame(models = c('Classification Trees','Random Forest','Logistic Regression', 'Naive Bayes','SVM','KNN','Neural Network'),aucs)
aucs.df[order(aucs.df$aucs,decreasing = T),] ######################### AUC END #########################
