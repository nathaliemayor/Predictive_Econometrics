### DATA ANALYTICS - ASSIGNMENT

install.packages("olsrr")
install.packages("skimr")
library(glmnet) 
library(corrplot)
library(olsrr)
library(skimr)

# Setting the Working Directory and importing the data
current_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))

load("house_sales_data.Rdata")
load("house_sales_pred.Rdata")

str(data_clean)
str(data_pred)
skim(data_clean)
skim(data_pred)

 ### Sold Houses 
id_SoldHouses <- as.matrix(data_clean[, 1])
colnames(id_SoldHouses)<- "id_SoldHouses"
y_SoldHouses <- as.matrix(data_clean[, which(colnames(data_clean) == "SalePrice")])
colnames(y_SalePrice) <- "SalePrice"
x_SoldHouses <- as.matrix(data_clean[, -c(1,which(colnames(data_clean) == "SalePrice"))])

### New Houses without price
id_NewHouses<-as.matrix(data_pred[,1])
colnames(id_SoldHouses)<-"id_NewHouses"
x_NewHouses<-as.matrix(data_pred[,-1])

##### 1. Cleaning procedure ####
# Check the shape of the ditribution of y_SoldHouses without transformation

# Cumulative Distribution of Spending
plot(ecdf(y_SoldHouses),xlab = "Houses sale prices in US-Dollars", main = "Cumulative Distribution of Houses Prices",
     ylab = "ecdf", sub = "(Truncated at 800,000 US-Dollars)", xlim= c(0, 800000),verticals = TRUE, do.points = FALSE)
abline(v=mean(y_SoldHouses))
abline(h=0.5)

# additionally histogram (, xlim= c(0, 800'000))
hist(y_SoldHouses, 100, main = "Histogram of Houses Sale Prices", xlim= c(0, 800000),xlab = "Houses sale Prices",col="lightblue")
abline(v=mean(y_SoldHouses),col="purple")


### with log and sqrt transformations
log_y_SoldHouses <- as.matrix(log(y_SoldHouses))
sqrt_y_SoldHouses<-as.matrix(sqrt(y_SoldHouses))

# Cumulative Distribution of Log Houses sale prices
plot(ecdf(log_y_SoldHouses), xlab = "log Houses sale Prices ", ylab = "ecdf", main = "Cumulative Distribution of Log Houses sale prices",verticals = TRUE, do.points = FALSE)
abline(v=mean(log_y_SoldHouses))
abline(h=0.5)

# histogram of log Houses sale price
hist(log_y_SoldHouses, 100, main = "Histogram of Log Houses sale prices",col="lightgreen")
abline(v=mean(log_y_SoldHouses),col="red")

# Cumulative Distribution of square root Houses sale prices
plot(ecdf(sqrt_y_SoldHouses), xlab = "Square root of Houses sale Prices ", ylab = "ecdf", main = "Cumulative Distribution of the square root of Houses sale prices",verticals = TRUE, do.points = FALSE)
abline(v=mean(sqrt_y_SoldHouses))
abline(h=0.5)

# histogram of square root Houses sale price
hist(sqrt_y_SoldHouses, 50, main = "Histogram of the square root of Houses sale prices",col="lightgreen")
abline(v=mean(sqrt_y_SoldHouses),col="red")

# summary of the outcome (basic, log, sqrt)
summary(y_SoldHouses)
summary(log_y_SoldHouses)
summary(sqrt_y_SoldHouses)

##### REGRESSION ANALYSIS ####

# Set seed
set.seed(16122020)

#### Correlation analysis - looking for correlations above 0.8 ####
cormat <- cor(x_SoldHouses)
diag(cormat) <- 0
highposcor <- unique(cormat[((cormat > 0.8)&(cormat < 1.0))])
highnegcor<-unique(cormat[(cormat < -0.8)])

### getting the names of the highly positively and negatively correlated variables

poscornames<- matrix(NA, nrow = length(highposcor), ncol = 1)
negcornames<-matrix(NA, nrow = length(highnegcor), ncol = 1)

for (cor_idx in 1:length(highposcor)) {
  poscornames[cor_idx]<-print(rownames(cormat)[which(cormat == highposcor[cor_idx], arr.ind = TRUE)[1,]])
}

for (ncor_idx in 1:length(highnegcor)) {
  negcornames[ncor_idx]<-print(rownames(cormat)[which(cormat == highnegcor[ncor_idx], arr.ind = TRUE)[1,]])
}


### Using the names obtained above to find the corresponging indexes 

idx_poscor  <-matrix(NA, nrow = length(highposcor), ncol = 1)
idx_negcor  <-matrix(NA, nrow = length(highnegcor), ncol = 1)

for (i in 1:length(highposcor)) {
  idx_poscor[i]<-print(which(colnames(x_SoldHouses)==poscornames[i]))
}

for (i in 1:length(highnegcor)) {
  idx_negcor[i]<-print(which(colnames(x_SoldHouses)==negcornames[i]))
}

idx_negcor2 <- idx_negcor[-1]

x_SH<-x_SoldHouses[,c(-idx_negcor2,-idx_poscor)]


#### dividing the data into training and test for the Sold Houses
# Random selection of observations 
sample_size <- floor(0.7 * nrow(y_SoldHouses))
training_set <- sample(seq_len(nrow(y_SoldHouses)), size = sample_size, replace = FALSE)

# Composition of training and test data for the sold houses
training_y    <- log_y_SoldHouses[training_set, ]
test_y        <- log_y_SoldHouses[-training_set, ]

taining_y1<-y_SoldHouses[training_set,]
test_y1<-y_SoldHouses[-training_set,]

training_id   <- id_SoldHouses[training_set,]
test_id       <-id_SoldHouses[-training_set,]
training_x    <-x_SH[training_set,]
test_x        <-x_SH[-training_set,]

training      <-data_clean_whc[training_set,]
test          <-data_clean_whc[-training_set,]

##### OLS #####

### with training data set
training_x    <-as.data.frame(training_x)
ols           <- lm(training_y ~ ., data = training_x) 
summary(ols)

AIC(ols)
BIC(ols)

ols_plot_resid_qq(ols)
ols_plot_resid_hist(ols)

### whole data set
x_SH<-as.data.frame(x_SH)
ols2          <-lm(log_y_SoldHouses~.,data = x_SH)
summary(ols2)

# Calculate the MSE
test_x        <-as.data.frame(test_x)
test$predols  <- predict(ols, newdata = test_x)

predMSEols    <- mean((test_y - test$predols)^2) 
print(round(predMSEols,4))


MAEols        <- mean(abs(test_y - test$predols))

R2ols         <- 1 - (sum((test_y - test$predols)^2)/sum((test_y -mean(test_y))^2))



### Prediction for the new Houses 
data_pred_cor <- as.data.frame(data_pred[,-1])
data_pred$predols<-predict(ols,newdata = data_pred_cor)
data_pred$predols2<-predict(ols2,newdata = data_pred_cor)

OLS_Predicted_Prices<-exp(data_pred$predols)
OLS_Predicted_Prices2<-exp(data_pred$predols2)
hist(OLS_Predicted_Prices2,100,col = "pink",main = "Histogram of OLS predicted prices",xlab = "OLS predicted prices")


##### LASSO REGRESSION #####
set.seed(01012021)
lasso.cv <- cv.glmnet(as.matrix(x_SoldHouses[training_set,]), training_y, type.measure = "mse", family = "gaussian", nfolds = 10, alpha = 1)
coef_lasso1 <- coef(lasso.cv, s = "lambda.min") 
print(coef_lasso1)
plot(lasso.cv)
sort(coef_lasso1)
lasso.cv$lambda
lasso.cv$lambda.min
lasso.cv$lambda.1se

fitLasso <- predict(lasso.cv, new = as.matrix(x_SoldHouses[-training_set,]), s = lasso.cv$lambda.min)
MSE_lasso <- mean((test_y - fitLasso)^2)

R2_Lasso <- 1 - (sum((test_y - fitLasso)^2)/sum((test_y -mean(test_y))^2))

fitNH<-  predict(lasso.cv, new = as.matrix(x_NewHouses), s = lasso.cv$lambda.min)

hist(exp(fitNH),200,col = "lightblue",main = "Historam of forcasted new houses prices (Lasso)",xlab = "forcasted new houses prices (Lasso)")
abline(v=mean(LassoNewHousesPrices))

LassoNewHousesPrices<-exp(fitNH)
colnames(LassoNewHousesPrices)<-c("Predicted Prices")

write.csv(LassoNewHousesPrices,"/Users/nathaliemayor/Documents/MiQEF/Courses/Compulsory (32)/DA1/HA/nathalie_mayor.csv",sep = ",")

summary(LassoNewHousesPrices)


##### RIDGE REGRESSION  #####
set.seed(01012021)
Ridge.cv <- cv.glmnet(as.matrix(x_SoldHouses[training_set,]), training_y, type.measure = "mse", family = "gaussian", nfolds = 10, alpha = 0)
coef_Ridge1 <- coef(Ridge.cv, s = "lambda.min") 
print(coef_Ridge1)
plot(Ridge.cv)

fitRidge <- predict(Ridge.cv, new = as.matrix(x_SoldHouses[-training_set,]), s = Ridge.cv$lambda.min)
MSE_Ridge <- mean((test_y - fitRidge)^2)
print(MSE_Ridge)

R2_log_ridge <- 1 - (sum((test_y - fitRidge)^2)/sum((test_y -mean(test_y))^2))

fitNHRidge<-  predict(Ridge.cv, new = as.matrix(x_NewHouses), s = Ridge.cv$lambda.min)

hist(exp(fitNHRidge),100,col = "purple",main = "Historam of forcasted new houses prices (Ridge)",xlab = "forcasted new houses prices (Ridge)")
abline(v=mean(RidgeNewHousesPrices))

RidgeNewHousesPrices<-exp(fitNHRidge)


