bankdata=read.csv(file.choose())
str(bankdata)
# T test for y and Balance:
aggregate(bankdata$balance~bankdata$y,FUN=mean)
t.test(bankdata$balance~bankdata$y)
# Since the p-value is less than 0.05,we REJECT NULL Hypothesis. Statistically Significant
# Chi - square test for y and Job:
chisq.test(table(bankdata$y, bankdata$job))
# Since the p-value is less than 0.05,we REJECT NULL Hypothesis. Statistically Significant

# Binary Logistic Regression #
bankLog=glm(y~.,data=bankdata,family="binomial")
summary(bankLog)
bankpredict=predict(bankLog,type="response")
table(Actual=bankdata$y,Predict=bankpredict>0.5)
Accuracy=(38940+1833)/(38940+1833+982+3456)
Accuracy=90.1838%

# Decision Tree 
bankpart=rpart(y~.,data=bankdata)
summary(bankpart)
plot(bankpart)
text(bankpart,cex=0.50)
rpart.plot(bankpart)
bankpartpredict=predict(bankpart,type="class")
table(Actual=bankdata$y,Predict=bankpartpredict)
(38904+1845)/(38904+1845+1018+3444)
# Accuracy is 90.1307%

# Random Forest (1000Tress) 
bankdatarandomForest=randomForest(y~.,data=bankdata,ntree=1000,do.trace=100)
print(bankdatarandomForest)
plot(bankdatarandomForest)
(38473+2597)/(38473+2597+2692+1449)
# Accuracy = 90.88275%

# GRADIENT BOOSTING MACHINE
bankdata1=bankdata
bankdata1$y=ifelse(bankdata1$y==" True.",1,0)
table(bankdata1$y)
bankgbm=gbm(y~.,data=bankdata1,distribution = "bernoulli",n.trees = 1000,cv.folds = 3)
bestiter=gbm.perf(bankgbm,method="cv")
gbmpredict=predict(bankgbm,bankdata1,bestiter)
table(bankdata1$y,gbmpredict>0.5)
(39125+3029)/(2260+797+39125+3029)
# Accuracy=93.23837%

# NEUTRAL NETWORKS
banknnet=nnet(y~.,data=bankdata,size=10,maxit=100)
print(banknnet)
banknnetpredict=predict(banknnet,type="class")
table(bankdata$y,banknnetpredict)
(39919+4)/(39919+4+3+5285)
# Accuracy=88.30373%
