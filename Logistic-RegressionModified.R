setwd("C:\\Users\\Srujana\\Desktop\\Data_Sets")
train <- read.csv("TrainingDataInsurance.csv")
test <- read.csv("TestDataInsurance.csv")
test_response <- read.csv("test_response.csv")
test <- cbind(test_response,test)

# #test1 <- cbind(test_response,test)
# test[which(test$Avgsizehousehold ==6),] <- 4
# test[which(test$SocialclassD==8),] <- 9
# test[which(test$Numberofhouses==9),] <- 10
# test[which(test$Numberofhouses==10),] <- 5
var <- colnames(test)
for(i in 1:length(var)){
  a <- vector()
  b <- vector()
  no_val <- vector()
  if(length(unique(test[var[i]])[,1])<50){
    a <- sort(unique(test[var[i]])[,1])
    b <- sort(unique(train[var[i]])[,1])
    no_val <- a[!a %in% b]
    if(length(no_val) == 1){
      main_val <- a[!a %in% b]
      print(main_val)
      train_val <- unique(train[var[i]])[,1]
      print(train_val)
      test[which(test[var[i]] == main_val),var[i]] <- sample(train_val,1)
    } else {
      main_val <- a[!a %in% b]
      print(main_val)
      train_val <- unique(train[var[i]])[,1]
      print(train_val)
      test[which(test[var[i]] %in% main_val),var[i]] <- sample(train_val,1)
      
    }
  }
}


library(reshape2)

## weights of evidence and information value - Profiling categorical variables - to find significant categorical variables 
## weightage above 0.1 value indicates that it is significant

iv <- data.frame()
x <- c("CustomerSubtype", "Numberofhouses",    "Avgsizehousehold",    "Avgage",    "Customermaintype",    "Romancatholic",
       "Protestant",    "Otherreligion",    "Noreligion",    "Married",    "Livingtogether",    "Otherrelation",    
       "Singles",    "Householdwithoutchildren",    "Householdwithchildren",    "Highleveleducation",    "Mediumleveleducation",
       "Lowerleveleducation",    "Highstatus",    "Entrepreneur",    "Farmer",    "Middlemanagement",    "Skilledlabourers",    
       "Unskilledlabourers",    "SocialclassA",    "SocialclassB1",    "SocialclassB2",    "SocialclassC",    "SocialclassD",
       "Rentedhouse",    "Homeowners",    "Onecar",    "Twocars",    "Nocar",    "NationalHealthService",    "Privatehealthinsurance",
       "IncomeLessThanThirtyThousand",    "IncomeBetweenThirtyThousandAndFortyFiveThousand",    "IncomeBetweenFortyFiveThousandAndSeventyFiveThousand",
       "IncomeBetweenSeventyFiveThousandAndOneTwentyTwoThousand",    "IncomeGreaterThanOneTwentyThreeThousand",    
       "AverageIncome",    "Purchasingpowerclass",    "Contributionprivatethirdpartyinsurance")
for (i in seq(1,length(x),1)) {
  a <- table(train[,x[i]], train[,"CARAVANNumberofmobilehomepolicies"])
  a <- as.data.frame(a)
  a <- dcast(a, Var1 ~ Var2)
  a[,4] <- a[,2]/sum(a[,2])
  a[,5] <- a[,3]/sum(a[,3])
  a[,6] <- log(a[,4]/a[,5], exp(1))
  a[,7] <- a[,6]*(a[,4] - a[,5])
  a[,8] <- a[,2] + a[,3]
  a <- subset(a, a$"1" >= 1)
  #assign(paste("table",x[i], sep = "_" ), a)
  iv[i,1] <- x[i]
  iv[i,2] <- sum(a$V7)
  print(iv[i,2])
}

## Library required to find Area under the curve

##library(ROCR)

## building model - as.factor function - is to tell R that this is a categorical variable
fit <- glm(CARAVANNumberofmobilehomepolicies ~  as.factor(Customermaintype) + as.factor(Numberofhouses) + as.factor(Avgsizehousehold) + 
             as.factor(Avgage) + as.factor(Customermaintype) + as.factor(Romancatholic) + as.factor(Protestant) + as.factor(Otherreligion) +  as.factor(Noreligion) +
             as.factor(Married) + as.factor(Livingtogether) + as.factor(Otherrelation) + as.factor(Singles) + as.factor(Householdwithoutchildren) +    
             as.factor(Householdwithchildren) + as.factor(Highleveleducation) + as.factor(Mediumleveleducation) + as.factor(Lowerleveleducation) + as.factor(Highstatus) +
             as.factor(Entrepreneur) + as.factor(Farmer) + as.factor(Middlemanagement) + as.factor(Skilledlabourers) + as.factor(Unskilledlabourers) + as.factor(SocialclassA) +
             as.factor(SocialclassB1) + as.factor(SocialclassB2) + as.factor(SocialclassC) + as.factor(SocialclassD) + as.factor(Rentedhouse) +
             as.factor(Onecar)  + as.factor(Twocars) + as.factor(Nocar) + as.factor(NationalHealthService)  +
             as.factor(IncomeLessThanThirtyThousand) + as.factor(IncomeBetweenThirtyThousandAndFortyFiveThousand) + as.factor(IncomeBetweenFortyFiveThousandAndSeventyFiveThousand) +
             as.factor(IncomeBetweenSeventyFiveThousandAndOneTwentyTwoThousand) + as.factor(IncomeGreaterThanOneTwentyThreeThousand)    + as.factor(AverageIncome) +
             as.factor(Purchasingpowerclass) + as.factor(Contributionprivatethirdpartyinsurance) + Contributionthirdpartyinsurancefirms + Contributionthirdpartyinsuraneagriculture + 
             Contributioncarpolicies + Contributiondeliveryvanpolicies + ContributionmotorcycleOrscooterpolicies + Contributionlorrypolicies  + Contributiontrailerpolicies + Contributiontractorpolicies +
             Contributionagriculturalmachinespolicies + Contributionmopedpolicies + Contributionlifeinsurances + Contributionprivateaccidentinsurancepolicies + 
             Contributionfamilyaccidentsinsurancepolicies + Contributiondisabilityinsurancepolicies + Contributionfirepolicies + Contributionsurfboardpolicies + 
             Contributionboatpolicies + Contributionbicyclepolicies + Contributionpropertyinsurancepolicies + Contributionsocialsecurityinsurancepolicies + 
             Numberofprivatethirdpartyinsurance + Numberofthirdpartyinsurancefirms + Numberofthirdpartyinsuraneagriculture + Numberofcarpolicies + 
             Numberofdeliveryvanpolicies + NumberofmotorcycleOrscooterpolicies + Numberoflorrypolicies  + Numberoftrailerpolicies + Numberoftractorpolicies + 
             Numberofagriculturalmachinespolicies + Numberofmopedpolicies + Numberoflifeinsurances + Numberofprivateaccidentinsurancepolicies + 
             Numberoffamilyaccidentsinsurancepolicies + Numberofdisabilityinsurancepolicies + Numberoffirepolicies + Numberofsurfboardpolicies + Numberofboatpolicies +
             Numberofbicyclepolicies    + Numberofpropertyinsurancepolicies + Numberofsocialsecurityinsurancepolicies, family ="binomial", data = train)

## Stepwise Regression
library(MASS)

step <- stepAIC(fit, direction="forward")
step$anova ## display results 

library(ROCR)
## gives response in the form probability 
train$pred<-predict(fit,train,type = "response") 
pred<-prediction(train$pred,train$CARAVANNumberofmobilehomepolicies) 
auc_data <- performance(pred,"auc")@y.values[[1]]
auc_data

#### Pred for test

test$pred <- predict(fit,test,type="response")
pred1<-prediction(test$pred,test$CARAVANNumberofmobilehomepolicies) 
auc_test <- performance(pred1,"auc")@y.values[[1]]
auc_test
