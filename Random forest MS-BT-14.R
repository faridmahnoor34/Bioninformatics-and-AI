#name file
m<-read.csv("smoking_dataset.csv")

#structure analysis
str(m)

#rectification
m$gender<- as.factor(m$gender)
m$hearing.left.<- as.factor(m$hearing.left.)
m$hearing.right.<-as.factor(m$hearing.right.)
m$Urine.protein<-as.factor(m$Urine.protein)
m$oral<-as.factor(m$oral)
m$tartar<-as.factor(m$tartar)
m$dental.caries<-as.factor(m$dental.caries)
m$smoking<-as.factor(m$smoking)

#structure analysis
str(m)
set.seed(123)
sample_indices <- sample(nrow(m), 0.8 * nrow(m))  # 80% for training, 20% for testing

train_data <- m[sample_indices, ]

test_data <- m[-sample_indices, ]
#random forest
rfNews()
library(randomForest)
set.seed(123)
rf_model <- randomForest(smoking ~ tartar+age+weight.kg.+height.cm.+triglyceride+hemoglobin+ALT+Gtp+dental.caries+gender+tartar, data = train_data,
                         .data = train_data ,ntree = 100)

summary(rf_model)
print(rf_model)
predictions <- predict(rf_model, newdata = test_data)
accuracy <- sum(predictions == test_data$smoking) / nrow(test_data)
print(paste("Accuracy:", accuracy))
