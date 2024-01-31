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

#Treatment of missing values
na.omit(m)

#identification of outliers
library(ggplot2)

ggplot(m,aes(x=height.cm.))+geom_boxplot()
ggplot(m,aes(x=weight.kg.))+geom_boxplot()
ggplot(m,aes(x=age))+geom_boxplot()
ggplot(m,aes(x=waist.cm.))+geom_boxplot()
ggplot(m,aes(x=eyesight.left.))+geom_boxplot()
ggplot(m,aes(x=eyesight.right.))+geom_boxplot()
ggplot(m,aes(x=systolic))+geom_boxplot()
ggplot(m,aes(x=relaxation))+geom_boxplot()
ggplot(m,aes(x=fasting.blood.sugar))+geom_boxplot()
ggplot(m,aes(x=HDL))+geom_boxplot()
ggplot(m,aes(x=LDL))+geom_boxplot()
ggplot(m,aes(x=AST))+geom_boxplot()
ggplot(m,aes(x=ALT))+geom_boxplot()
ggplot(m,aes(x=Cholesterol))+geom_boxplot()
ggplot(m,aes(x=hemoglobin))+geom_boxplot()
ggplot(m,aes(x=serum.creatinine))+geom_boxplot()
ggplot(m,aes(x=triglyceride))+geom_boxplot()
ggplot(m,aes(x=Gtp))+geom_boxplot()

#Treatment of outlier
IQR_age<-IQR(m$age )
IQR_age
lower_bound<-quantile(m$age,0.25)-1.5*IQR_age
upper_bound<-quantile(m$age,0.25)+1.5*IQR_age
x<-m[m$age>=lower_bound&m$age <=upper_bound,]
boxplot(x$age)

IQR_height.cm.<-IQR(m$height.cm. )
IQR_height.cm.
lower_bound<-quantile(m$height.cm.,0.25)-1.5*IQR_height.cm.
upper_bound<-quantile(m$height.cm.,0.25)+1.5*IQR_height.cm.
x<-m[m$height.cm.>=lower_bound&m$height.cm. <=upper_bound,]
boxplot(x$height.cm.)

IQR_weight.kg.<-IQR(m$weight.kg. )
IQR_weight.kg.
lower_bound<-quantile(m$weight.kg.,0.25)-1.5*IQR_weight.kg.
upper_bound<-quantile(m$weight.kg.,0.25)+1.5*IQR_weight.kg.
x<-m[m$weight.kg.>=lower_bound&m$weight.kg. <=upper_bound,]
boxplot(x$weight.kg.)

IQR_eyesight.left.<-IQR(m$eyesight.left. )
IQR_eyesight.left.
lower_bound<-quantile(m$eyesight.left.,0.25)-1.5*IQR_eyesight.left.
upper_bound<-quantile(m$eyesight.left.,0.25)+1.5*IQR_eyesight.left.
x<-m[m$eyesight.left.>=lower_bound&m$eyesight.left. <=upper_bound,]
boxplot(x$eyesight.left.)

IQR_eyesight.right.<-IQR(m$eyesight.right. )
IQR_eyesight.right.
lower_bound<-quantile(m$eyesight.right.,0.25)-1.5*IQR_eyesight.right.
upper_bound<-quantile(m$eyesight.right.,0.25)+1.5*IQR_eyesight.right.
x<-m[m$eyesight.right.>=lower_bound&m$eyesight.right. <=upper_bound,]
boxplot(x$eyesight.right.)

IQR_systolic<-IQR(m$systolic )
IQR_systolic
lower_bound<-quantile(m$systolic,0.25)-1.5*IQR_systolic
upper_bound<-quantile(m$systolic,0.25)+1.5*IQR_systolic
x<-m[m$systolic>=lower_bound&m$systolic <=upper_bound,]
boxplot(x$systolic)

IQR_waist.cm.<-IQR(m$waist.cm.)
IQR_waist.cm.
lower_bound<-quantile(m$waist.cm.,0.25)-1.5*IQR_waist.cm.
upper_bound<-quantile(m$waist.cm.,0.25)+1.5*IQR_waist.cm.
x<-m[m$waist.cm.>=lower_bound&m$waist.cm. <=upper_bound,]
boxplot(x$waist.cm.)

IQR_relaxation<-IQR(m$relaxation )
IQR_relaxation
lower_bound<-quantile(m$relaxation,0.25)-1.5*IQR_relaxation
upper_bound<-quantile(m$relaxation,0.25)+1.5*IQR_relaxation
x<-m[m$relaxation>=lower_bound&m$relaxation <=upper_bound,]
boxplot(x$relaxation)

IQR_fasting.blood.sugar<-IQR(m$fasting.blood.sugar )
IQR_fasting.blood.sugar
lower_bound<-quantile(m$fasting.blood.sugar,0.25)-1.5*IQR_fasting.blood.sugar
upper_bound<-quantile(m$fasting.blood.sugar,0.25)+1.5*IQR_fasting.blood.sugar
x<-m[m$fasting.blood.sugar>=lower_bound&m$fasting.blood.sugar <=upper_bound,]
boxplot(x$fasting.blood.sugar)

IQR_HDL<-IQR(m$HDL )
IQR_HDL
lower_bound<-quantile(m$HDL,0.25)-1.5*IQR_HDL
upper_bound<-quantile(m$HDL,0.25)+1.5*IQR_HDL
x<-m[m$HDL>=lower_bound&m$HDL <=upper_bound,]
boxplot(x$HDL)

IQR_LDL<-IQR(m$LDL )
IQR_LDL
lower_bound<-quantile(m$LDL,0.25)-1.5*IQR_LDL
upper_bound<-quantile(m$LDL,0.25)+1.5*IQR_LDL
x<-m[m$LDL>=lower_bound&m$LDL <=upper_bound,]
boxplot(x$LDL)

IQR_AST<-IQR(m$AST )
IQR_AST
lower_bound<-quantile(m$AST,0.25)-1.5*IQR_AST
upper_bound<-quantile(m$AST,0.25)+1.5*IQR_AST
x<-m[m$AST>=lower_bound&m$AST <=upper_bound,]
boxplot(x$AST)

IQR_ALT<-IQR(m$ALT)
IQR_ALT
lower_bound<-quantile(m$ALT,0.25)-1.5*IQR_ALT
upper_bound<-quantile(m$ALT,0.25)+1.5*IQR_ALT
x<-m[m$ALT>=lower_bound&m$ALT <=upper_bound,]
boxplot(x$ALT)

IQR_hemoglobin<-IQR(m$hemoglobin )
IQR_hemoglobin
lower_bound<-quantile(m$hemoglobin,0.25)-1.5*IQR_hemoglobin
upper_bound<-quantile(m$hemoglobin,0.25)+1.5*IQR_hemoglobin
x<-m[m$hemoglobin>=lower_bound&m$hemoglobin <=upper_bound,]
boxplot(x$hemoglobin)

IQR_serum.creatinine<-IQR(m$serum.creatinine )
IQR_serum.creatinine
lower_bound<-quantile(m$serum.creatinine,0.25)-1.5*IQR_serum.creatinine
upper_bound<-quantile(m$serum.creatinine,0.25)+1.5*IQR_serum.creatinine
x<-m[m$serum.creatinine>=lower_bound&m$serum.creatinine <=upper_bound,]
boxplot(x$serum.creatinine)

IQR_Gtp<-IQR(m$Gtp )
IQR_Gtp
lower_bound<-quantile(m$Gtp,0.25)-1.5*IQR_Gtp
upper_bound<-quantile(m$Gtp,0.25)+1.5*IQR_Gtp
x<-m[m$Gtp>=lower_bound&m$Gtp <=upper_bound,]
boxplot(x$Gtp)

IQR_triglyceride<-IQR(m$triglyceride )
IQR_triglyceride
lower_bound<-quantile(m$triglyceride,0.25)-1.5*IQR_triglyceride
upper_bound<-quantile(m$triglyceride,0.25)+1.5*IQR_triglyceride
x<-m[m$triglyceride>=lower_bound&m$triglyceride <=upper_bound,]
boxplot(x$triglyceride)

IQR_Cholesterol<-IQR(m$Cholesterol )
IQR_Cholesterol
lower_bound<-quantile(m$Cholesterol,0.25)-1.5*IQR_Cholesterol
upper_bound<-quantile(m$Cholesterol,0.25)+1.5*IQR_Cholesterol
x<-m[m$Cholesterol>=lower_bound&m$Cholesterol <=upper_bound,]
boxplot(x$Cholesterol)

#structure analysis after removing outliers
str(x)

#summary
summary(x)

#statistics
sd(x$age)
sd(x$height.cm.)
sd(x$weight.kg.)
sd(x$waist.cm.)
sd(x$eyesight.left.)
sd(x$eyesight.right.)
sd(x$systolic)
sd(x$relaxation)
sd(x$fasting.blood.sugar)
sd(x$Cholesterol)
sd(x$triglyceride)
sd(x$HDL)
sd(x$LDL)
sd(x$hemoglobin)
sd(x$serum.creatinine)
sd(x$AST)
sd(x$ALT)
sd(x$Gtp)

var(x$age)
var(x$height.cm.)
var(x$weight.kg.)
var(x$waist.cm.)
var(x$eyesight.left.)
var(x$eyesight.right.)
var(x$systolic)
var(x$relaxation)
var(x$fasting.blood.sugar)
var(x$Cholesterol)
var(x$triglyceride)
var(x$HDL)
var(x$LDL)
var(x$hemoglobin)
var(x$serum.creatinine)
var(x$AST)
var(x$ALT)
var(x$Gtp)

#Histogram for numeric variables
ggplot(x,aes(x=age))+geom_histogram()
ggplot(x,aes(x=height.cm.))+geom_histogram()
ggplot(x,aes(x=weight.kg.))+geom_histogram()
ggplot(x,aes(x=waist.cm.))+geom_histogram()
ggplot(x,aes(x=eyesight.left.))+geom_histogram()
ggplot(x,aes(x=eyesight.right.))+geom_histogram()
ggplot(x,aes(x=systolic))+geom_histogram()
ggplot(x,aes(x=relaxation))+geom_histogram()
ggplot(x,aes(x=fasting.blood.sugar))+geom_histogram()
ggplot(x,aes(x=HDL))+geom_histogram()
ggplot(x,aes(x=LDL))+geom_histogram()
ggplot(x,aes(x=hemoglobin))+geom_histogram()
ggplot(x,aes(x=Cholesterol))+geom_histogram()
ggplot(x,aes(x=triglyceride))+geom_histogram()
ggplot(x,aes(x=serum.creatinine))+geom_histogram()
ggplot(x,aes(x=AST))+geom_histogram()
ggplot(x,aes(x=ALT))+geom_histogram()
ggplot(x,aes(x=Gtp))+geom_histogram()

#barplot for categoric variable
ggplot(x, aes(x=gender))+geom_bar()
ggplot(x, aes(x=tartar))+geom_bar()
ggplot(x, aes(x=dental.caries))+geom_bar()
ggplot(x, aes(x=hearing.right.))+geom_bar()
ggplot(x, aes(x=hearing.left.))+geom_bar()
ggplot(x, aes(x=Urine.protein))+geom_bar()
ggplot(x, aes(x=smoking))+geom_bar()

#barplot between categoric variables
abc <- table(x$smoking, x$gender)
abc
barplot(abc, legend.text = TRUE)

abc <- table(x$smoking, x$hearing.left.)
abc
barplot(abc, legend.text = TRUE)

abc <- table(x$smoking, x$hearing.right.)
abc
barplot(abc, legend.text = TRUE)

abc <- table(x$smoking, x$tartar)
abc
barplot(abc, legend.text = TRUE)

abc <- table(x$smoking, x$dental.caries)
abc
barplot(abc, legend.text = TRUE)

abc <- table(x$smoking, x$Urine.protein)
abc
barplot(abc, legend.text = TRUE)

#boxplot for relationship between numeric and categoric variables
ggplot(x, aes(y = age)) + geom_boxplot()
ggplot(x, aes(x =smoking , y = age)) + geom_boxplot(fill = 'pink', color = 'black', alpha = 0.7) + theme_minimal()

ggplot(x, aes(y = height.cm.)) + geom_boxplot()
ggplot(x, aes(x =smoking , y = height.cm.)) + geom_boxplot(fill = 'pink', color = 'black', alpha = 0.7) + theme_minimal()

ggplot(x, aes(y = waist.cm.)) + geom_boxplot()
ggplot(x, aes(x =smoking , y = waist.cm.)) + geom_boxplot(fill = 'pink', color = 'black', alpha = 0.7) + theme_minimal()

ggplot(x, aes(y = eyesight.left.)) + geom_boxplot()
ggplot(x, aes(x =smoking , y = eyesight.left.)) + geom_boxplot(fill = 'pink', color = 'black', alpha = 0.7) + theme_minimal()

ggplot(x, aes(y = eyesight.right.)) + geom_boxplot()
ggplot(x, aes(x =smoking , y = eyesight.right.)) + geom_boxplot(fill = 'pink', color = 'black', alpha = 0.7) + theme_minimal()

ggplot(x, aes(y = relaxation)) + geom_boxplot()
ggplot(x, aes(x =smoking , y = relaxation)) + geom_boxplot(fill = 'pink', color = 'black', alpha = 0.7) + theme_minimal()

ggplot(x, aes(y = systolic)) + geom_boxplot()
ggplot(x, aes(x =smoking , y = systolic)) + geom_boxplot(fill = 'pink', color = 'black', alpha = 0.7) + theme_minimal()

ggplot(x, aes(y = Cholesterol)) + geom_boxplot()
ggplot(x, aes(x =smoking , y = Cholesterol)) + geom_boxplot(fill = 'pink', color = 'black', alpha = 0.7) + theme_minimal()

ggplot(x, aes(y = triglyceride)) + geom_boxplot()
ggplot(x, aes(x =smoking , y = triglyceride)) + geom_boxplot(fill = 'pink', color = 'black', alpha = 0.7) + theme_minimal()

ggplot(x, aes(y = fasting.blood.sugar)) + geom_boxplot()
ggplot(x, aes(x =smoking , y = fasting.blood.sugar)) + geom_boxplot(fill = 'pink', color = 'black', alpha = 0.7) + theme_minimal()

#Regression model
model <-glm(smoking~height.cm.+hemoglobin+triglyceride+Gtp+ALT+gender+tartar+dental.caries+weight.kg.,data=x,family=binomial())
summary(model)

# Make predictions on the data
predictions <- predict(model, newdata = x, type = "response")

# Convert predicted probabilities to binary outcomes
predicted_classes <- ifelse(predictions > 0.5, 1, 0)

# Create a confusion matrix (Error Analysis)
conf_matrix <- table(predicted_classes,x$smoking)

#Accuracy (Number of accurate guesses)
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
print(accuracy)
