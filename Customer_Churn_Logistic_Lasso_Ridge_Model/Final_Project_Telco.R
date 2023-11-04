#---------------------------------------------------------------------------------#
#                                                                                 #
#             ALY6015: Final project :  R-code                               #
#---------------------------------------------------------------------------------#
#Importing packages#
library(tidyverse)
library(caret)
library(RColorBrewer)
library(ggcorrplot)
library(corrplot)
library(psych)
library(dplyr)
library(plyr)
library(ggplot2)
library(gtools)
library(ggfortify)
library(GGally)
library(readr)
library(readxl)
library(knitr)
library(modelr)
library(scales)
library(sqldf)
library(car)
library(ggpubr)
library(grid)
library(gridExtra)
library(lattice)
library(hrbrthemes)
library(ISLR)
library(caret)
library(pROC)
library(psych)
library(olsrr)
library(naniar)
library(DataExplorer)
library(formattable)
library(glmnet)
library(Metrics)
library(MLmetrics)
library(GGally)

install.packages("naniar" , "DataExplorer" , "MLMetrics")
#Importing the Dataset#

telco_df <- read.csv("WA_Fn-UseC_-Telco-Customer-Churn.csv",header=TRUE,
                          stringsAsFactors = FALSE,na.string = "")
str(telco_df)

#DATA CLEANING#
#Checking data is clean?#
colSums(is.na(telco_df)) # check & Returns the number of missing values in each column

sum(is.na(telco_df)) # Counts missing values in entire data frame

plot_missing(telco_df, title="Missing Data Profile",geom_label_args = list("size" = 2, "label.padding" = unit(0.1, "lines")))


#Replacing missing value records of Total Charges column with their respective mean#

telco_df$TotalCharges[is.na(telco_df$TotalCharges)]<-round (mean(telco_df$TotalCharges,na.rm=TRUE),2) 

#Dropping column CustomerID as they are not needed#
telco_df <-select(telco_df,-c(customerID))

#Converting Variables to Numeric 
telco_df$tenure  <- as.numeric (telco_df$tenure)

#Recoding the Target variable Churn and converting it to Numeric
telco_df$Churn <- revalue(telco_df$Churn, c("Yes"="1", "No"="0"))
telco_df$Churn  <- as.numeric (telco_df$Churn)

#Recoding value for other attributes and converting it to Numeric
telco_df$Partner <- revalue(telco_df$Partner, c("Yes"="1", "No"="0"))
telco_df$Partner  <- as.numeric (telco_df$Partner)

telco_df$Dependents <- revalue(telco_df$Dependents, c("Yes"="1", "No"="0"))
telco_df$Dependents  <- as.numeric (telco_df$Dependents)

telco_df$PhoneService <- revalue(telco_df$PhoneService, c("Yes"="1", "No"="0"))
telco_df$PhoneService  <- as.numeric (telco_df$PhoneService)

telco_df$PaperlessBilling <- revalue(telco_df$PaperlessBilling, c("Yes"="1", "No"="0"))
telco_df$PaperlessBilling  <- as.numeric (telco_df$PaperlessBilling)

#Descriptive Statistics for entire dataset#

formattable(describe(telco_df), 
            caption = "Descriptive statistics summary of the Telco Customer Churn Dataset")

#Descriptive Statistics by Churn#
describeBy(telco_df, group=telco_df$Churn)

#Lets check the CORRELATION#
telco_df_numeric = telco_df %>%   dplyr::select(where(is.numeric))

corr <- round(cor(telco_df_numeric), 2)

corrplot(corr, type = "upper", , 
         tl.col = "black", tl.cex=0.7, title = "Correlation of all Numeric Attributes ",
         mar=c(0,0,1,0))

ggcorrplot(corr, type = "lower",
           lab = TRUE)

# Correlation Matrix (Table)
View(cor(telco_df_numeric))

############# Subset Method ##############
#Split the dataset into Test and Train data#
set.seed(123)
trainIndex <- createDataPartition(telco_df$Churn, p = 0.70, list = FALSE , times=1 )
telco_df_train <- telco_df[trainIndex,]
telco_df_test <- telco_df[-trainIndex,]


library(leaps)

telco_df_subset_model <- regsubsets(Churn ~ ., data = telco_df_train,nbest=1)
summary_telco_df_subset <- summary(telco_df_subset_model)
summary_telco_df_subset

par(cex.lab = 1.5 , cex.axis=0.7, las=3 )

plot(telco_df_subset_model , scale = "adjr2")

#Reset par
dev.off()

#Final 9 Predictors -
#MultipleLines - No Phone service , OnlineSecurityYes , TechSupportYes , ContractOne year ,
#ContractTwo Year , PaperlessBilling , PaymentMethodElectronic Check ,  MonthlyCharges & Total Charges

##########################################################
#EXPLORATORY DATA ANALYSIS - EDA#
#########################################################
gender_Churn <- table(telco_df$Churn, telco_df$gender)
gender_churn_bp <- barplot(gender_Churn,
                           main = "Gender wise Churn",
                           xlab = "Gender", ylab = "No. of customers",
                           col = c("pink", "red"),
                           legend.text = rownames(gender_Churn),
                           beside = TRUE,
                           args.legend = list(title = "Churn", x = "topright"))

#2. Senior citizen - pie chart? churn rate

sc <- table(telco_df$SeniorCitizen)
sc_perc <- round((sc / nrow(telco_df)) * 100, digits = 2)
sc_perc_df <- data.frame(sc_perc)
colnames(sc_perc_df)[1] <- "Senior_Citizen"
colnames(sc_perc_df)[2] <- "Percentage"

ggplot(sc_perc_df, aes(x = "", y = Percentage, fill = Senior_Citizen)) +
  geom_col() + geom_text(aes(label = paste(Percentage,"%", sep = ""))
                         ,position = position_stack(vjust = 0.5)) +
  guides(fill = guide_legend(title = "Senior Citizen")) +
  coord_polar(theta = "y") +
  theme_void() +
  scale_fill_brewer(palette = "Set2")+
  ggtitle("Percentage of Senior Citizens")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text=element_text(size=12,  family="Comic Sans MS", color= "black"))

sc_Churn <- table(telco_df$Churn, telco_df$SeniorCitizen)
sc_churn_bp <- barplot(sc_Churn,
                       main = "SeniorCitizen vs churn",
                       xlab = "Senior Citizen", ylab = "No. of customers",
                       col = c("lightblue", "blue"),
                       legend.text = rownames(sc_Churn),
                       beside = TRUE,
                       args.legend = list(title = "Churn", x = "topright"))

sc_1 <- round((1393*100/5901),2)
sc_0 <- round((476*100/1142),2)

sc_churn_df <- data.frame (
  Percentage_of_churned_customers = c(23.61,41.68),
  Senior_Citizen = c('No','Yes'))

ggplot(sc_churn_df, aes(x=Senior_Citizen, y=Percentage_of_churned_customers,
                        fill=Senior_Citizen))+
  geom_bar(stat="identity", color="black")+ theme_bw()+
  scale_fill_manual(values=c("#56B4E9", "#E69F00"))+
  ggtitle("Do Senior citizens have more likelihood to churn")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text=element_text(size=12,  family="Comic Sans MS", color= "black")) 

#3. Tenure and churn
ggplot(telco_df, aes(x=tenure, color=Churn)) +
  geom_histogram(fill="white", alpha=0.5, position="identity")+
  scale_color_brewer(palette="Dark2")+
  ggtitle("Relationship between Tenure and Churn")+
  xlab("Tenure (in months)")+
  ylab("No. of customers")+theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text=element_text(size=12,  family="Comic Sans MS", color= "black"))

#4. Which contract type has the most number of churns?
install.packages("hrbrthemes")
library(hrbrthemes)

install.packages("viridis")
library(viridis)

g <- ggplot(telco_df, aes(Contract))
g + geom_bar(aes(fill = Churn))+
  ggtitle("Which contract type has the most number of churns?")+
  xlab("Contract type")+ scale_fill_manual(values=c("#56B4E9", "#E69F00"))+
  ylab("No. of customers")+theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text=element_text(size=12,  family="Comic Sans MS", color= "black"))

ggplot(telco_df, aes(x=tenure, color=Contract, fill=Contract)) +
  geom_histogram()+
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum() +
  facet_grid(. ~ Contract)+
  ggtitle("Which contract type has the most no. of customers with a longer tenure?")+
  xlab("Tenure (in months)")+
  ylab("No. of customers")

#Which internet service results in maximum no. of customer churn?

internet_service <- ggplot(telco_df, aes(InternetService))
internet_service + geom_bar(aes(fill = Churn))+
  ggtitle("Which internet service has the most number of churns?")+
  xlab("Internet Service")+scale_fill_manual(values=c("#69b3a2", "#404080"))+
  ylab("No. of customers")+theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        text=element_text(size=12,  family="Comic Sans MS", color= "black"))

############################################################
#METHODS - CHI-SQUARE & ANOVA#
################################################################

#Chi Square (Test of independence):

#3. Contract type and churn

#H0 : No. of churned customers are independent of the contract type
#H1: No. of churned customers are dependent on the contract type.

table(telco_df$Contract,telco_df$Churn)

#Significance level:0.05
alpha <- 0.05

#Create one vector for each row

r1 <- c(2220,1655)
r2 <- c(1307,166)
r3 <- c(1647,48)

#State the number of rows for the matrix
rows <- 3

#Create a matrix from the rows
mtrx <- matrix(c(r1,r2,r3), nrow = rows,byrow = TRUE)

#Name the rows and columns 
rownames(mtrx) <- c("Month-to-month","One year","Two year")
colnames(mtrx) <- c("No","Yes")

#Run the test
result <- chisq.test(mtrx)

#Compare the p-value to alpha and make a decision
ifelse(result$p.value > alpha, "Fail to reject the null hypothesis",
       "Reject the null hypothesis")


#4. Total charges and Internet service (One-way ANOVA):

#H0 : The mean total charges is the same for all types of internet service.
#H1: The mean total charges is different for all types of internet service.

library("ggpubr")
ggboxplot(telco_df, x = "InternetService", y = "TotalCharges", 
          color = "InternetService", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          ylab = "TotalCharges", xlab = "InternetService")

#Significance level:0.05
alpha <- 0.05

dsl <- sqldf("select * from telco_df where InternetService = 'DSL'")

fo <- sqldf("select * from telco_df where InternetService = 'Fiber optic'")

no <- sqldf("select * from telco_df where InternetService = 'No'")

dsl_df <- data.frame('total_charges' = dsl$TotalCharges,
                     'Internet_Service' = rep('DSL',nrow(dsl)),
                     stringsAsFactors = FALSE)

fo_df <- data.frame('total_charges' = fo$TotalCharges,
                    'Internet_Service' = rep('Fiber optic',nrow(fo)),
                    stringsAsFactors = FALSE)

no_df <- data.frame('total_charges' = no$TotalCharges,
                    'Internet_Service' = rep('No',nrow(no)),
                    stringsAsFactors = FALSE)


#Combine the dataframes into one
df <- rbind(dsl_df,fo_df,no_df)

df$Internet_Service <- as.factor(df$Internet_Service)

#Run the ANOVA test
anova <- aov(total_charges ~ Internet_Service,data = df)

#View the model summary
summary(anova)

#Save summary to an object
a.summary <- summary(anova)

#Determine if we should reject the null hypothesis
ifelse(p.value > alpha, "Fail to reject the null hypothesis",
       "Reject the null hypothesis")

#See differences
TukeyHSD(anova)

par(mar = c(8, 8, 8, 8))
plot(TukeyHSD(anova, conf.level=.95), las = 2)
preparedData <- f1[sapply(f1, class) == 'numeric']
corMatrix <- cor(preparedData)
corrplot(corMatrix,tl.cex=0.6)

#sub mean
f1$TotalCharges[is.na(f1$TotalCharges)] <- 2283.3
#h0: there is no relationship between gender and churn
#h1: there is a realtionship between gender and churn
femal <-  f1[f1$gender == "1",]
femal$Churn <- as.factor(femal$Churn)
summary(femal) #churn yes 939, no 2549
male <-  f1[f1$gender == "0",]
male$Churn <- as.factor(male$Churn)
summary(male) #churn yes 930, no 2625
genderChurn<-matrix(c(939,930,2549,2625),nrow=2, ncol=2)
chisq.test(genderChurn)

#h0: there is no relationship between partner and churn
#h1: there is a realtionship between partner and churn
partnerYes <-  f1[f1$Partner == "1",]
partnerYes$Churn <- as.factor(partnerYes$Churn)
summary(partnerYes) #churn yes 669, no 2733
partnerNo <-  f1[f1$Partner == "0",]
partnerNo$Churn <- as.factor(partnerNo$Churn)
summary(partnerNo) #churn yes 1200, no 2441
partnerChurn<-matrix(c(669,1200,2773,2441),nrow=2, ncol=2)
chisq.test(partnerChurn)

#h0: there is no relationship between PhoneService and churn
#h1: there is a realtionship between PhoneService and churn
phoneYes <-  f1[f1$PhoneService == "1",]
phoneYes$Churn <- as.factor(phoneYes$Churn)
summary(phoneYes) #churn 1699, no 4662
phoneNo <-  f1[f1$PhoneService == "0",]
phoneNo$Churn <- as.factor(phoneNo$Churn)
summary(phoneNo) #churn 170, no 512
phoneServiceChurn<-matrix(c(1699,170,4662,512),nrow=2, ncol=2)
chisq.test(phoneServiceChurn)

#Two-way ANOVA test churn dependent variable 
#SeniorCitizen and Dependents as the independent variables.
#H0 no impact on churn
#H1 impact on churn
f1$SeniorCitizen <- as.factor(f1$SeniorCitizen)
f1$Dependents <- as.factor(f1$Dependents)
anova2 <- aov(Churn~Dependents+SeniorCitizen,data=f1)
summary(anova2)

#H0: There is no difference in the duration of tenure.
#H1: There is a difference in the duration of tenure.
tenureLess <-  f1[f1$tenure <= "32.7",]
tenureLess$Churn <- as.factor(tenureLess$Churn)
summary(tenureLess) #churn 1134, no 1849
tenureMore <-  f1[f1$tenure > "32.7",]
tenureMore$Churn <- as.factor(tenureMore$Churn)
summary(tenureMore) #churn 735, no 3325
tenureChurn<-matrix(c(1134,735,1849,3325),nrow=2, ncol=2)
chisq.test(tenureChurn)
#Total charges and contract(One-way ANOVA):
#H0: The mean total charges for each contract type are the same.
#H1: The mean total charges for at least one contract type is different.
# Significance level: 0.05
alpha <- 0.05

month_to_month <- telco_df[telco_df$Contract == 'Month-to-month', ]
one_year <- telco_df[telco_df$Contract == 'One year', ]
two_year <- telco_df[telco_df$Contract == 'Two year', ]

month_to_month_df <- data.frame('total_charges' = month_to_month$TotalCharges,
                                'Contract' = rep('Month-to-month', nrow(month_to_month)),
                                stringsAsFactors = FALSE)

one_year_df <- data.frame('total_charges' = one_year$TotalCharges,
                          'Contract' = rep('One year', nrow(one_year)),
                          stringsAsFactors = FALSE)

two_year_df <- data.frame('total_charges' = two_year$TotalCharges,
                          'Contract' = rep('Two year', nrow(two_year)),
                          stringsAsFactors = FALSE)

# Combine the dataframes into one
df <- rbind(month_to_month_df, one_year_df, two_year_df)

df$Contract <- as.factor(df$Contract)

# Run the ANOVA test
anova <- aov(total_charges ~ Contract, data = df)

# View the model summary
summary(anova)

# Save summary to an object
a.summary <- summary(anova)

# Determine if we should reject the null hypothesis
p.value <- a.summary[[1]]["Pr(>F)"]

#Determine if we should reject the null hypothesis
ifelse(p.value > alpha, "Fail to reject the null hypothesis",
       "Reject the null hypothesis")


#Total charges and PaymentMethod(One-way ANOVA):
# H0: There is no difference in the mean Total Charges for different Payment Methods.
# Ha: There is a difference in the mean Total Charges for different Payment Methods.
# Significance level: 0.05
alpha <- 0.05

electronic_check <- telco_df[telco_df$PaymentMethod == 'Electronic check', ]
mailed_check <- telco_df[telco_df$PaymentMethod == 'Mailed check', ]
bank_transfer <- telco_df[telco_df$PaymentMethod == 'Bank transfer (automatic)', ]
credit_card <- telco_df[telco_df$PaymentMethod == 'Credit card (automatic)', ]

electronic_check_df <- data.frame('total_charges' = electronic_check$TotalCharges,
                                  'PaymentMethod' = rep('Electronic check', nrow(electronic_check)),
                                  stringsAsFactors = FALSE)

mailed_check_df <- data.frame('total_charges' = mailed_check$TotalCharges,
                              'PaymentMethod' = rep('Mailed check', nrow(mailed_check)),
                              stringsAsFactors = FALSE)

bank_transfer_df <- data.frame('total_charges' = bank_transfer$TotalCharges,
                               'PaymentMethod' = rep('Bank transfer (automatic)', nrow(bank_transfer)),
                               stringsAsFactors = FALSE)

credit_card_df <- data.frame('total_charges' = credit_card$TotalCharges,
                             'PaymentMethod' = rep('Credit card (automatic)', nrow(credit_card)),
                             stringsAsFactors = FALSE)

# Combine the dataframes into one
df <- rbind(electronic_check_df, mailed_check_df, bank_transfer_df, credit_card_df)

df$PaymentMethod <- as.factor(df$PaymentMethod)

# Run the ANOVA test
anova <- aov(total_charges ~ PaymentMethod, data = df)

# View the model summary
summary(anova)

# Save summary to an object
a.summary <- summary(anova)

# Determine if we should reject the null hypothesis
p.value <- a.summary[[1]]["Pr(>F)"]

#Determine if we should reject the null hypothesis
ifelse(p.value > alpha, "Fail to reject the null hypothesis",
       "Reject the null hypothesis")

################################################################
#LOGISTIC MODEL - GLM #
################################################################

#build logistic regression model
preparedData <- f1[sapply(f1, class) == 'numeric']
str(preparedData)

trainIndex <- sort(sample(x = nrow(preparedData), size = nrow(preparedData) * 0.7)) 
train <- preparedData[trainIndex,] 
test <- preparedData[-trainIndex,] 
dim(train)
dim(test)

fit1<- glm(Churn~.,data = train,family = binomial(link = "logit"))
summary(fit1)

fit2<- glm(Churn~tenure+PhoneService+OnlineSecurity+DeviceProtection+
             TechSupport+PaperlessBilling+MonthlyCharges+TotalCharges,
           data = train,family = binomial(link = "logit"))
summary(fit2)

fit3<- glm(Churn~tenure+PhoneService+OnlineSecurity+
             TechSupport+PaperlessBilling+MonthlyCharges+
             SeniorCitizen+DeviceProtection,
           data = train,family = binomial(link = "logit"))
summary(fit3) 

train$Churn= ifelse(train$Churn== "1", TRUE, FALSE)
train$Churn <- as.factor(train$Churn)

probabilities.train <- predict(fit3, newdata=train, type="response")
predicted.classes.min <- as.factor(ifelse(probabilities.train>=0.5, "TRUE", "FALSE"))
confusionMatrix(predicted.classes.min, train$Churn, positive = "TRUE")

ROC1 <- roc(train$Churn, probabilities.train)
plot(ROC1, col="red", ylab="Sensitivity - TP Rate", xlab= "Specificity - Fp Rate")

auc1 <- auc(ROC1)
Auc1
################################################################
#STEPWISE - FORWARD#
################################################################

# forward selection
# Create Train and Test set - random sample (70/30 split) 
trainIndex <- sort(sample(x = nrow(telco_df), size = nrow(telco_df) * 0.7)) 
sample_train <- telco_df[trainIndex,] 
sample_test <- telco_df[-trainIndex,] 

# Create Train and Test set - maintain % of event rate (70/30 split) 
library(caret) 
set.seed(123) 
trainIndex <- createDataPartition(telco_df$Churn, p = 0.7, list = FALSE, times = 1) 
telco_train <- telco_df[ trainIndex,] 
telco_test <- telco_df[-trainIndex,] 

library(MASS)
fullModel = lm(Churn ~ ., data = telco_test) # model with all variables
nullModel = lm(Churn ~ 1, data = telco_test) # model with the intercept only

summary(stepAIC(nullModel, # start with a model containing no variables
                direction = 'forward', # run forward selection
                scope = list(upper = fullModel, # the maximum to consider is a model with all variables
                             lower = nullModel), # the minimum to consider is a model with no variables
                trace = 0)) # do not show the step-by-step process of model selection

#Calculating the AIC and BIC:
library(stats)
stepAIC = stepAIC(nullModel, direction = 'forward', 
                  scope = list(upper = fullModel, lower = nullModel), 
                  trace = 0)
install.packages("Metrics")
library(Metrics)
rmse(telco_test$Churn, predict(stepAIC , telco_test))
rmse(telco_train$Churn, predict(stepAIC , telco_train))

#Calculating the ROC and AUC scores:
library(pROC)
model <- lm(Churn ~ ., data = telco_test) 
predictions <- predict(model, type = "response") 
predictions <- predict(fullModel, type = "response") 
roc_object <- roc(telco_test$Churn, predictions) 
auc(roc_object) # calculate the AUC score
plot(roc_object)

################################################################
#LASSO REGULARIZATION#
################################################################

#Split the dataset into Test and Train data#
set.seed(123)
trainIndex <- createDataPartition(telco_df$Churn, p = 0.70, list = FALSE , times=1 )
telco_df_train <- telco_df[trainIndex,]
telco_df_test <- telco_df[-trainIndex,]

#Define the Target variable for Train and Test Data set
y_train <- telco_df_train$Churn
y_test<- telco_df_test$Churn

#Define the matrix of predictor variables
x_train <- model.matrix(Churn ~ ., telco_df_train)[,-1]
x_test  <- model.matrix(Churn ~ ., telco_df_test)[,-1]

#Perform k-fold cross-validation to find optimal lambda value for Lasso #
set.seed(123)
lasso_cv <- cv.glmnet(x_train, y_train, family="binomial", alpha=1,nfolds = 10)
lasso_cv

#Plot of MSE by Lambda value
plot(lasso_cv)

################################################################
# Optimal Value of Lambda; Minimizes the Prediction Error
# Lambda Min - Minimizes out of sample loss
# Lambda 1SE - Largest value of Lambda within 1 Standard Error of Lambda Min.
################################################################
log(lasso_cv$lambda.min)
log(lasso_cv$lambda.1se)

#Find optimal value that minimize MSE
best_lambda1 <- lasso_cv$lambda.min
best_lambda1

#Fitting the Lasso Regression Model on the Training Set Using Best Lamba value(Min value)#
lasso_model_train_min  <- glmnet(x_train, y_train, alpha = 1, lambda = best_lambda1)
lasso_model_train_min
coef(lasso_model_train_min)
plot(coef(lasso_model_train_min))


#Fitting the Lasso Regression Model on the Training Set Using 1se Lamba value#
lasso_model_train_1se  <- glmnet(x_train, y_train, alpha = 1, lambda = lasso_cv$lambda.1se)
lasso_model_train_1se
coef(lasso_model_train_1se)
plot(coef(lasso_model_train_1se))

#Lasso regression model for making predictions on Train data#
predict_lasso_train <- predict(lasso_model_train_min, newx = x_train)

#RMSE for Lasso regression model against the Training set#
lasso_train_rmse <- rmse(y_train, predict_lasso_train)
lasso_train_rmse

#Lasso regression model for making predictions on Test data#
predict_lasso_test <- predict(lasso_model_train_min, newx = x_test)

#RMSE for Lasso regression model against the Test set#
lasso_test_rmse <- rmse(y_test, predict_lasso_test)
lasso_test_rmse

#Plotting the coefficients#
ggcoef(lasso_model_train_min)


#AIC and BIC scores for Lasso regression

glmnet_cv_bic <- function(fit, lambda = 'lambda.min'){
  whlm <- which(fit$lambda == fit[[lambda]])
  with(fit$glmnet.fit,
       {
         tLL <- nulldev - nulldev * (1 - dev.ratio)[whlm]
         k <- df[whlm]
         n <- nobs
         return(list('BIC' = log(n) * k - tLL))
       })
}

#BIC SCORE#
glmnet_cv_bic(lasso_cv)

#AIC SCORE#
AIC.glmnet <- function(glm_fit) {
  chisqLR <- glm_fit$nulldev - deviance(glm_fit)
  
  chisqLR - 2*glm_fit$df
}
AIC.glmnet(lasso_model_train_min)


# Sum of Squares Total and Error for Training and Test Data
sse_train <- sum((y_train - predict_lasso_train)^2)
sst_train <- sum((y_train - mean(y_train))^2)

#Calculating Training R-square 
training_rsq <- 1 - sse_train / sst_train
training_rsq

sse_test <- sum((y_test - predict_lasso_test)^2)
sst_test <- sum((y_test - mean(y_test))^2)

#Calculating Testing R-square 
testing_rsq <- 1 - sse_test / sst_test
testing_rsq

# Lasso Model performance Metrics
lasso_model_perf <- data.frame(
  RMSE_test = lasso_test_rmse,
  R2_test =testing_rsq , 
  RMSE_train =lasso_train_rmse,
  R2_train=training_rsq)

lasso_model_perf


#ROC & AUC Curve#

ROC_curve <- roc(telco_df_test$Churn ,predict_lasso_test)
plot(ROC_curve, col = "Red", ylab = "Sensitivity - TruePositive Rate",
     xlab = "Sensitivity - FalsePositive Rate" , plot=TRUE , print.auc=TRUE)


################################################################
#RIDGE REGULARIZATION#
################################################################

#Ridge regression:
set.seed(123)
trainIndex <- createDataPartition(telco_df$Churn, p = 0.7, list = FALSE,
                                  times = 1)

train <- telco_df[ trainIndex,]
test <- telco_df[-trainIndex,]

install.packages("glmnet")
library(glmnet)

train_x <- model.matrix(Churn ~., train)[,-1]
test_x <- model.matrix(Churn ~., test)[,-1]

train_y <- train$Churn
test_y <- test$Churn

#Ridge
#Find the best values of lambda 

#Training:
set.seed(123)

cv_model_ridge_train <- cv.glmnet(train_x, train_y, alpha = 0,family="binomial")

cv_model_ridge_train$lambda.min
cv_model_ridge_train$lambda.1se
log(cv_model_ridge_train$lambda.min)
log(cv_model_ridge_train$lambda.1se)
plot(cv_model_ridge_train) 

coef(cv_model_ridge_train, cv_model_ridge_train$lambda.min)

#Fit models based on lambda

model.min.ridge.train <- glmnet(train_x, train_y, alpha = 0,
                                lambda = cv_model_ridge_train$lambda.min)

#Display regression coeff
coef(model.min.ridge.train)

summary(model.min.ridge.train)

#Train set predictions and RMSE
install.packages("Metrics")
library(Metrics)

preds.train.ridge <- predict(model.min.ridge.train, s = cv_model_ridge_train$lambda.min,
                             newx = train_x)

sse <- sum((train_y - preds.train.ridge)^2)
sst <- sum((train_y - mean(train_y))^2)
train_RMSE_ridge = rmse(train_y, preds.train.ridge)
training_rsq <- 1 - (sse / sst)
training_rsq #0.27
train_RMSE_ridge #0.37

#Testing:

preds.test.ridge <- predict(model.min.ridge.train, s = cv_model_ridge_train$lambda.min,
                            newx = test_x)

sse <- sum((test_y - preds.test.ridge)^2)

sst <- sum((test_y - mean(test_y))^2)

test_RMSE_ridge = rmse(test_y, preds.test.ridge)

testing_rsq <- 1 - sse / sst

testing_rsq #0.31
test_RMSE_ridge #0.36

glmnet_cv_aicc <- function(fit, lambda = 'lambda.min'){
  whlm <- which(fit$lambda == fit[[lambda]])
  with(fit$glmnet.fit,
       {
         tLL <- nulldev - nulldev * (1 - dev.ratio)[whlm]
         k <- df[whlm]
         n <- nobs
         return(list('BIC' = log(n) * k - tLL))
       })
}

#AIC and BIC scores for Ridge regression
glmnet_cv_aicc(cv_model_ridge_train)


AIC.glmnet <- function(glm_fit) {
  chisqLR <- glm_fit$nulldev - deviance(glm_fit)
  
  chisqLR - 2*glm_fit$df
}

AIC.glmnet(model.min.ridge.train)

probabilities.test <- predict(model.min.ridge.test, newx = test_x, type = "response")
predicted.classes.min <- as.factor(ifelse(probabilities.test >= 0.5, "Yes", "No"))
library(pROC)
probabilities.test <- predict(model.min.ridge.train, newx = test_x, type = "response")

ROC1 <- roc(test_y,probabilities.test)

plot(ROC1, col = "blue", ylab = "TP Rate", 
     xlab = "FP Rate")

auc <- auc(ROC1)


