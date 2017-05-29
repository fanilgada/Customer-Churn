
# Installing all the packages first

install.packages(c("ggplot2", "vegan", "cluster", "ROSE", "randomForest", "class", "e1071"))

# Loading Libraries

library(ggplot2)
library (vegan)
library (cluster)
library(ROSE)
library(randomForest)
library(class)
library(e1071)

#------------------ Loading Churn Data--------------------


# telecom customer data
# stringsAsFactors = FALSE   
# This command helps to convert every character vector to a factor wherever it makes sense.

churn = read.csv("path/churn.csv",stringsAsFactors=FALSE)

  table(churn$Churn.)
  # The customers who will churn are True = 483 and others are False = 2850.
  
  prop.table(table(churn$Churn.))
  # FALSE = 86% TRUE = 14 %
  # SO currently, out of every 100 customers 14 switch to other companies and 86 continue with the same company
  
  
  # dropping state , area code, phone as we don't need them
  
  churn.dropped = churn[,c(2,5:length(churn))]
  View(churn.dropped)
  
  colnames(churn.dropped) = c("AccountLength","IntlPlan","VmailPlan","VmailMessage",
                              "DayMins","DayCalls","DayCharge","EveMins","EveCalls",
                              "EveCharge","NightMins","NightCalls","NightCharge","IntlMins",
                              "IntlCalls","IntlCharge","CustServCalls","Churn")
  
  
  # converting yes/no to boolean values of international plan and voice mail plan
  
  churn.dropped$IntlPlan[churn.dropped$IntlPlan == "yes"] = 1
  churn.dropped$IntlPlan[churn.dropped$IntlPlan == "no"] = 0
  churn.dropped$IntlPlan= as.integer(churn.dropped$IntlPlan)
  churn.dropped$VmailPlan[churn.dropped$VmailPlan == "yes"] = 1
  churn.dropped$VmailPlan[churn.dropped$VmailPlan == "no"] = 0
  churn.dropped$VmailPlan = as.integer(churn.dropped$VmailPlan)
  churn.dropped$Churn[churn.dropped$Churn == "True."] = 1
  churn.dropped$Churn[churn.dropped$Churn == "False."] = 0
  churn.dropped$Churn = as.integer(churn.dropped$Churn)
  
  View(churn.dropped)
  
  #----------- Exploratory Data Analysis ------------------
  
  # As no. of cust serv calls reach 4 its 45% likely that customer will churm and for 5 calls its 61% chance
  table(churn.dropped$CustServCalls,churn.dropped$Churn)
  prop.table(table(churn.dropped$CustServCalls,churn.dropped$Churn),1)
  
  
  # 47% ppl with Intl plan churn
  table(churn.dropped$IntlPlan,churn.dropped$Churn)
  prop.table(table(churn.dropped$IntlPlan,churn.dropped$Churn),1)
  
  
  # Low percentage of churn - try getting people to take voicemail plan
  table(churn.dropped$VmailPlan,churn.dropped$Churn)
  prop.table(table(churn.dropped$VmailPlan,churn.dropped$Churn),1)
  
  
  # Clustering
  
  # Day cluster
  #elbow curve
  
  k.max <- 15 # Maximal number of clusters
  data <- churn.dropped[,5:7]    #taking only morning chracterstics
  
  
  wss <- sapply(1:k.max, 
                function(k){kmeans(data, k, nstart=10 )$tot.withinss}) #you can ignore the warning you get after running this and proceed with next code
  
  plot(1:k.max, wss,
       type="b", pch = 19, frame = FALSE, 
       xlab="Number of clusters K",
       ylab="Total within-clusters sum of squares")
  abline(v = 3, lty =2) # using elbow curve to find optimal number of clusteres in which we find K=3
  
  Clusmor <- kmeans(churn.dropped[,5:7],3, nstart = 20)  #creating three clusters 
  Clusmor$cluster
  Clusmor$tot.withinss
  dis = dist(churn.dropped[,5:7])^2
  
  
  sil = silhouette (Clusmor$cluster, dis)
  
  plot(sil) #ploting silhouette plot to see whether cluster are homogenious or not
  
  charclusmor=as.character(Clusmor$cluster)
  
  newchurn=cbind(churn.dropped,charclusmor)
  
  tapply(newchurn$DayMins, newchurn$charclusmor, mean) #calculating the cluster mean for each variable
  tapply(newchurn$DayCalls, newchurn$charclusmor, mean)
  tapply(newchurn$DayCharge,newchurn$charclusmor,mean)
  
  table(newchurn$charclusmor,newchurn$Churn)  #crerating table to see which customer lie in which cluster
  
  
  # evening cluster
  
  #elbow curve
  # Compute and plot wss for k = 2 to k = 15
  
  k.max <- 15 # Maximal number of clusters
  data <- churn.dropped[,8:10]  
  
  wss=function(k)
  {
    return(kmeans(data, k, nstart=10 )$tot.withinss)
  }
  
  wss <- sapply(1:k.max, 
                function(k){kmeans(data, k, nstart=10 )$tot.withinss}) # you can ignore the warning you get after running this and proceed with next code
  plot(1:k.max, wss,
       type="b", pch = 19, frame = FALSE, 
       xlab="Number of clusters K",
       ylab="Total within-clusters sum of squares")  #calculating optimal number of cluster that is 3 in this case
  abline(v = 3, lty =2)
  
  
  Clusmor <- kmeans(churn.dropped[,8:10], 3, nstart = 20)#using kmean for clustering
  Clusmor$cluster
  Clusmor$tot.withinss
  dis = dist(churn.dropped[,8:10])^2
  
  sil = silhouette (Clusmor$cluster, dis)
  plot(sil) #ploting silhouette plot
  
  
  charcluseve=as.character(Clusmor$cluster)
  
  newchurn=cbind(newchurn,charcluseve)
  
  
  tapply(newchurn$EveMins, newchurn$charcluseve, mean)
  tapply(newchurn$EveCharge, newchurn$charcluseve, mean)
  tapply(newchurn$EveCalls,newchurn$charcluseve,mean)
  
  table(newchurn$charcluseve,newchurn$Churn)
  
  
  # Night cluster
  
  #elbow curve to find optimal number of clusters
  # Compute and plot wss for k = 2 to k = 15
  
  k.max <- 15 # Maximal number of clusters
  data <- churn.dropped[,11:13]
  
  wss=function(k)
  {
    return(kmeans(data, k, nstart=10 )$tot.withinss)
  }
  
  wss <- sapply(1:k.max, 
                function(k){kmeans(data, k, nstart=10 )$tot.withinss})
  plot(1:k.max, wss,
       type="b", pch = 19, frame = FALSE, 
       xlab="Number of clusters K",
       ylab="Total within-clusters sum of squares")
  abline(v = 3, lty =2)      #calculating optimal number of cluster that is 3 in this case
  
  
  Clusmor <- kmeans(churn.dropped[,11:13], 3, nstart = 20) #using kmean for clustering
  Clusmor$cluster
  Clusmor$tot.withinss
  dis = dist(churn.dropped[,11:13])^2
  
  sil = silhouette (Clusmor$cluster, dis)  #plotiing silhouette pl
  plot(sil)
  
  
  charclusnight=as.character(Clusmor$cluster)
  
  newchurn=cbind(newchurn,charclusnight)
  View(newchurn)
  
  tapply(newchurn$NightMins, newchurn$charclusnight, mean)  # tables to find cluster mean
  tapply(newchurn$NightCalls, newchurn$charclusnight, mean)
  tapply(newchurn$NightCharge,newchurn$charclusnight,mean)
  
  table(newchurn$charclusnight,newchurn$Churn)
  
  
  # Correlation matrix
  
  cor(churn.dropped)
  
  # These are the highly correlated variables.
  # Vmail.Plan - Vmail.message = 0.9569
  # Daily charge - Daily mins = 0.9999
  # Eve charge - Eve mins = 0.9999
  # Night charge- night mins = 0.99999
  # Intl charg - Intl mins - 0.99999
  
  
  # We can drop one from each pair of highly correlted variables from our analysis.
  
  churn.dropped = churn.dropped[,c(-4,-5,-8,-11,-14)]
  
  churn.dropped$IntlPlan= as.factor(churn.dropped$IntlPlan)
  churn.dropped$VmailPlan = as.factor(churn.dropped$VmailPlan)
  churn.dropped$Churn = as.factor(churn.dropped$Churn)
  
  
  # ----------------------- Sampling ---------------------------- 
  
  # splitting the data into test and train
  s = sample(x=nrow(churn.dropped), size = 0.8 * nrow(churn.dropped))
  
  # lets divide the data into 80/20 train/test
  train_churn = churn.dropped[s,]
  test_churn = churn.dropped[-s,]
  
  
  # Working with imbalanced dataset
  # Methods used to treat imbalanced datasets Undersampling, oversampling, Synthetic Data Generation & Cost Sensitive Learning
  # We will use Synthetic Data Generation using ROSE package
  
  data.trainChurn = ROSE(Churn ~ ., data = train_churn, seed = 1)$data
  table(data.trainChurn$Churn)
  #    0    1 
  # 1388 1278 
  
  prop.table(table(data.trainChurn$Churn))
  # so now we have 52% False and 48% True
  
  #-------------------- Building Models-----------------------------------
  
  # Lets apply models to our dataset
  
  # Let's start with random forest
  
  #Fitting model
  fit = randomForest(Churn ~ ., data = data.trainChurn,importance = TRUE)
  
  # This is the extractor function for variable importance measures
  importance(fit)
  
  # Dotchart of variable importance as measured by a Random Forest
  varImpPlot (fit)
  summary(fit)
  
  #Predict Output
  predicted= predict(fit,test_churn)
  misclassificationError = mean(predicted != test_churn$Churn)
  misclassificationError
  Accuracy = 1 - misclassificationError
  Accuracy
  
  
  # function to plot confusion matrix. We will use this for all the models built
  
  plotConfusionMatrix = function(actual,predicted,l)
  {
    Actual = factor(c(0,0,1,1))
    Predicted = factor(c(0,1,0,1))
    p = as.vector(table(actual,predicted))
    df = data.frame(Actual,Predicted,p)
    a = ggplot(data =  df, mapping = aes(x = Predicted, y = Actual)) +
      ggtitle(l) +
      geom_tile(aes(fill = p), colour = "white") +
      geom_text(aes(label = sprintf("%1.0f", p)), vjust = 1) +
      scale_fill_gradient(low = "blue", high = "red") +
      theme_bw() + theme(legend.position = "none")
    return(a)
  }
  
  # plotting confusion matrix
  plotConfusionMatrix(test_churn$Churn,predicted,"Random Forest")
  randomFCMatrix = table(test_churn$Churn,predicted)
  randomFCMatrix
  
  # AUC for Random Forest
  roc.curve(test_churn$Churn,predicted)
  
  
  # Let's try KNN - K-nearest neighbours
  
  #Fitting model
  # The value for k is generally chosen as the square root of the number of observations.
  # 1388+1278 = 2666, sqrt(2666) = 51.6, but let's try with k = 10
  
  fit = knn(train = data.trainChurn,test = test_churn,cl = data.trainChurn$Churn,k = 10)
  summary(fit)
  
  # to check this model accuracy
  
  # lets try with k = 50
  # AUC from ROSE package
  roc.curve(test_churn$Churn,fit)
  fit = knn(train = data.trainChurn,test = test_churn,cl = data.trainChurn$Churn,k = 50)
  summary(fit)
  misclassificationError = mean(fit != test_churn$Churn)
  misclassificationError
  Accuracy = 1 - misclassificationError
  Accuracy
  
  # Confusion Matrix
  knnCMatrix = table(test_churn$Churn,fit)
  knnCMatrix
  plotConfusionMatrix(test_churn$Churn,fit,"KNN")
  
  # AUC for KNN 
  roc.curve(test_churn$Churn,fit)
  
  
  # LET'S TRY LOGISTIC REGRESSION
  
  fit = glm(Churn~.,data = data.trainChurn, family = binomial(link = "logit"))
  summary(fit)
  
  #Predict Output
  predicted= predict(fit,test_churn,type = "response")
  predicted = ifelse(predicted>0.5,1,0)
  misclassificationError = mean(predicted != test_churn$Churn)
  misclassificationError
  Accuracy = 1 - misclassificationError
  Accuracy
  
  # plotting confusion matrix
  plotConfusionMatrix(test_churn$Churn,predicted,"Logistic Regression")
  logCMatrix = table(test_churn$Churn,predicted)
  logCMatrix
  
  # AUC for Logistic Regression
  roc.curve(test_churn$Churn,predicted)
  
  
  # Let's try SVM now
  
  # fitting model
  fit = svm(Churn ~ ., data = train_churn, kernel = "radial",gamma =1,cost =1)
  summary(fit)
  tune.out=tune(svm, Churn~., data=train_churn, kernel="radial",
                ranges=list(cost=c(0.1,1,10),
                            gamma=c(0.5,1,2) ))
  summary(tune.out)
  
  #Predict Output
  predicted= predict(tune.out$best.model,test_churn)
  svmCMatrix = table(test_churn$Churn,predicted)
  svmCMatrix
  misclassificationError = mean(predicted != test_churn$Churn)
  misclassificationError
  Accuracy = 1 - misclassificationError
  Accuracy
  
  # plotting confusion matrix
  plotConfusionMatrix(test_churn$Churn,predicted,"SVM")
  
  # AUC for SVM
  roc.curve(test_churn$Churn,predicted)
  
  
  # Let's try naive bayes 
  
  fit = naiveBayes(Churn~., data = data.trainChurn)
  class(fit)# naivebayes
  summary(fit)
  print(fit)
  # The fit has class ?naiveBayes? and the summary tells us that the model provides a-priori 
  # probabilities of no-recurrence and recurrence events as well as conditional probability 
  # tables across all attributes.
  
  predicted = predict(fit, newdata = test_churn)
  misclassificationError = mean(predicted != test_churn$Churn)
  misclassificationError
  Accuracy = 1 - misclassificationError
  Accuracy
  
  # plotting confusion matrix
  plotConfusionMatrix(test_churn$Churn,predicted,"Naive Bayes")
  naiveCMatrix = table(test_churn$Churn,predicted)
  naiveCMatrix
  
  # AUC for Naive Bayes
  roc.curve(test_churn$Churn,predicted)
  
  
  #----------------------- Model Assessment------------------------
  
  
  # Consider two classifiers, A and B, as in the image below. Suppose they were trained on a balanced set. 
  # Let A make its mistakes only through false positives: non-churners(n) predicted to churn(Y), 
  # while B makes its mistake only through false negatives, churners(p), predicted not to churn(N). 
  # Now consider what this looks like on an unbalanced set, where the ps (churners) are much less than the ns (non-churners). 
  # It would seem that B makes far fewer misclassifications based on accuracy than A, and would thus be a better classifier.
  
  # Classifiers should be about the Business End: keeping costs down
  
  # Baseline Classifier - Via Accuracy
  
  # In our churn dataset there are two obvious baselines: assume every customer wont churn, and assume all customers will churn.
  # The former baseline, will on our dataset, straight away give you a 85.5% accuracy. 
  # The other baseline, from an accuracy perspective is less interesting: it would only have a 14.5% correct rate.
  
  # Baseline Classifier - Minimizing Cost or Maximizing Profit
  
  # Lets write a cost matrix to deal with the business situation
  
  # Lets assume we make an offer with an administrative cost of \$3 and an offer cost of \$100, 
  # an incentive for the customer to stay with us. If a customer leaves us, 
  # we lose the customer lifetime value, which is some kind of measure of the lost profit from that customer. 
  # Lets assume this is the average number of months a customer stays with the telecom times the net revenue from the customer per month. 
  # We'll assume 3 years and \$30/month margin per user lost, for roughly a $1000 loss.
  
  
  # Defining a function to calculate the net profit for a model
  # TN=people we predicted not to churn who wont churn. We associate no cost with this as 
  # they continue being our customers, CMatrix[1,1]
  # FP=people we predict to churn. Who wont. Lets associate a admin_cost+offer_cost cost per customer 
  # with this as we will spend some money on getting them not to churn, but we will lose this money.CMatrix[1,2]
  # FN=people we predict wont churn. And we send them nothing. But they will. 
  # This is the big loss, the clv, CMatrix[2,1]
  # TP= people who we predict will churn. And they will. , CMatrix[2,2]
  # These are the people we can do something with. So we make them an offer. 
  # Say a fraction f accept it i.e 20% default value
  # customer lifetime value clv
  
  # Lower the cost better the model
  AvgCostCalculation<-function(CMatrix,f=0.5,admin_cost=3,offer_cost=100,clv=1000)
  {
    
    tnc = 0
    fpc = admin_cost+offer_cost
    fnc = clv
    tpc = f*(offer_cost+admin_cost) + (1 - f)*(admin_cost+offer_cost+clv)
    Cost =  CMatrix[2,2]*tpc + CMatrix[1,1]*tnc + CMatrix[1,2]*fpc + CMatrix[2,1]*fnc  
    
    return(Cost/nrow(test_churn))
  
  }
  
  # Baseline Model Cost - No Customers Churn and thus we send nothing
  
  # New Confusion Matrix 
  predicted = vector(mode = 'numeric',length = nrow(test_churn))
  BaselineCMatrix = table(test_churn$Churn,predicted)
  
  BaselineCost_NoChurn =  BaselineCMatrix[2,1]*1000/nrow(test_churn)
  BaselineCost_NoChurn
  
  # Cost for Random Forest model
  randomForestCost = AvgCostCalculation(randomFCMatrix)
  randomForestCost
  
  # Cost for SVM model
  SVMCost = AvgCostCalculation(svmCMatrix)
  SVMCost
  
  # Cost for KNN model
  KNNCost = AvgCostCalculation(knnCMatrix)
  KNNCost 
  
  # Cost for Logistic Regression model
  LogisticRegressionCost = AvgCostCalculation(logCMatrix)
  LogisticRegressionCost
  
  # Cost for Naive Bayes model
  NaiveBayesCost = AvgCostCalculation(naiveCMatrix)
  NaiveBayesCost
