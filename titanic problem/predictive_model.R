setwd("E:/Kaggle/titanic problem")

titanic.train <- read.csv(file = "train.csv", stringsAsFactors = FALSE, header = TRUE)
titanic.test <- read.csv(file = "test.csv", stringsAsFactors = FALSE, header = TRUE)

titanic.train$IsTrainSet <- TRUE
titanic.test$IsTrainSet <- FALSE

titanic.test$Survived <- NA

#cleaning the loaded data
titanic.full[titanic.full$Embarked == '', "Embarked"] <- 'S'

#cleaning the loaded data
age.median <- median(titanic.full$Age, na.rm = TRUE)
titanic.full[is.na(titanic.full$Age), "Age"] <- age.median

#cleaning the loaded data
fare.median <- median(titanic.full$Fare, na.rm = TRUE)
titanic.full[is.na(titanic.full$Fare), "Fare"] <- fare.median 

#categorical casting
titanic.full$Pclass <- as.factor(titanic.full$Pclass)
titanic.full$Sex <- as.factor(titanic.full$Sex)
titanic.full$Embarked <- as.factor(titanic.full$Embarked)
  
#Split cleaned dataset
titanic.train <- titanic.full[titanic.full$IsTrainSet == TRUE,]
titanic.test <- titanic.full[titanic.full$IsTrainSet == FALSE,]

titanic.train$Survived <- as.factor(titanic.train$Survived)

Survived.equation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
Survived.formula <- as.formula(Survived.equation)
install.packages("randomForest")
library(randomForest)

titanic.model   <- randomForest(formula = Survived.formula , data = titanic.train, ntree = 500, mtry = 3, nodesize = 0.01 * nrow(titanic.train))

features.equation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
Survived <- predict(titanic.model, newdata = titanic.test)

PassengerId <- titanic.test$PassengerId
Output.df <- as.data.frame(PassengerId)
Output.df$Survived <- Survived

write.csv(Output.df, file="kaggle_submission.csv", row.names = FALSE)
