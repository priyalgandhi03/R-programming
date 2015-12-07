setwd("C:\\Users\\Srujana\\Desktop\\Data_Sets")
train <- read.csv("TrainingDataInsurance.csv")
test <- read.csv("TestDataInsurance.csv")
test_response <- read.csv("test_response.csv")
test <- cbind(test_response,test)

var = c("Customermaintype" , "Numberofhouses" , "Avgsizehousehold", "Avgage", "Customermaintype", "Romancatholic", "Protestant", "Otherreligion", "Noreligion" , "Married", "Livingtogether", "Otherrelation", "Singles", "Householdwithoutchildren", "Householdwithchildren" , "Highleveleducation" , "Mediumleveleducation", "Lowerleveleducation", "Highstatus",
        "Entrepreneur" , "Farmer" , "Middlemanagement" , "Skilledlabourers" , "Unskilledlabourers" , "SocialclassA" ,
        "SocialclassB1" , "SocialclassB2" , "SocialclassC" , "SocialclassD" , "Rentedhouse" ,
        "Onecar"  , "Twocars", "Nocar" , "NationalHealthService",
        "IncomeLessThanThirtyThousand" , "IncomeBetweenThirtyThousandAndFortyFiveThousand" , "IncomeBetweenFortyFiveThousandAndSeventyFiveThousand",
        "IncomeBetweenSeventyFiveThousandAndOneTwentyTwoThousand" , "IncomeGreaterThanOneTwentyThreeThousand" , "AverageIncome",
        "Purchasingpowerclass" , "Contributionprivatethirdpartyinsurance")

for(i in 1:length(var)){
  train[,var[i]] <- (train[,var[i]])
}

library("randomForest")

fit <- randomForest(CARAVANNumberofmobilehomepolicies ~  Customermaintype + Numberofhouses + Avgsizehousehold+ Avgage+ Customermaintype+ Romancatholic+ Protestant+ Otherreligion+ Noreligion + Married+ Livingtogether+ Otherrelation+ Singles+ Householdwithoutchildren+ Householdwithchildren + Highleveleducation + Mediumleveleducation+ Lowerleveleducation+ Highstatus+
                      Entrepreneur + Farmer + Middlemanagement + Skilledlabourers + Unskilledlabourers + SocialclassA +
                      SocialclassB1 + SocialclassB2 + SocialclassC + SocialclassD + Rentedhouse +
                      Onecar  + Twocars+ Nocar + NationalHealthService+
                      IncomeLessThanThirtyThousand + IncomeBetweenThirtyThousandAndFortyFiveThousand + IncomeBetweenFortyFiveThousandAndSeventyFiveThousand+
                      IncomeBetweenSeventyFiveThousandAndOneTwentyTwoThousand + IncomeGreaterThanOneTwentyThreeThousand + AverageIncome+
                      Purchasingpowerclass + Contributionprivatethirdpartyinsurance + Contributionthirdpartyinsurancefirms + Contributionthirdpartyinsuraneagriculture + 
                      Contributioncarpolicies + Contributiondeliveryvanpolicies + ContributionmotorcycleOrscooterpolicies + Contributionlorrypolicies  + Contributiontrailerpolicies + Contributiontractorpolicies +
                      Contributionagriculturalmachinespolicies + Contributionmopedpolicies + Contributionlifeinsurances + Contributionprivateaccidentinsurancepolicies + 
                      Contributionfamilyaccidentsinsurancepolicies + Contributiondisabilityinsurancepolicies + Contributionfirepolicies + Contributionsurfboardpolicies + 
                      Contributionboatpolicies + Contributionbicyclepolicies + Contributionpropertyinsurancepolicies + Contributionsocialsecurityinsurancepolicies + 
                      Numberofprivatethirdpartyinsurance + Numberofthirdpartyinsurancefirms + Numberofthirdpartyinsuraneagriculture + Numberofcarpolicies + 
                      Numberofdeliveryvanpolicies + NumberofmotorcycleOrscooterpolicies + Numberoflorrypolicies  + Numberoftrailerpolicies + Numberoftractorpolicies + 
                      Numberofagriculturalmachinespolicies + Numberofmopedpolicies + Numberoflifeinsurances + Numberofprivateaccidentinsurancepolicies + 
                      Numberoffamilyaccidentsinsurancepolicies + Numberofdisabilityinsurancepolicies + Numberoffirepolicies + Numberofsurfboardpolicies + Numberofboatpolicies +
                      Numberofbicyclepolicies    + Numberofpropertyinsurancepolicies + Numberofsocialsecurityinsurancepolicies, data = train, ntree=1000, mtry = 5, node.size = 50, importance=TRUE, proximity=TRUE)

train$pred<-predict(fit,type = "response")
pred<-prediction(train$pred,train$CARAVANNumberofmobilehomepolicies)
auc_data <- performance(pred,"auc")@y.values[[1]]
auc_data

test$pred <- predict(fit,test,type="response")
pred1<-prediction(test$pred,test$CARAVANNumberofmobilehomepolicies) 
auc_test <- performance(pred1,"auc")@y.values[[1]]
auc_test

