### Assignment 3 ###

install.packages(c("grf","DiagrammeR","glmnet"))
library(grf)
library(DiagrammeR)
library(glmnet)

current_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))

browser2006<-read.csv("browser_2006.csv",header=TRUE,sep = ",")
browsernew<-read.csv("browser_new.csv",header=TRUE,sep = ",")

### Data preparation
# browser 2006 data
y_2006 <- as.matrix(browser2006[, 2])
x_2006 <- as.matrix(browser2006[, c(3:ncol(browser2006))])
id_2006 <- as.matrix(browser2006[, 1])

sum(y_2006[4,])

# browser new data
x_new <- as.matrix(browsernew[, c(2:ncol(browsernew))])
id_new <- as.matrix(browsernew[, 1])

### 1 - online average spending in 2006
mean(y_2006)
### The average spending in 2006 is 1959.92 USD per household

### 2 - most frequented website for id 1297
highx<-max(x_2006[1,])
colnames(x_2006)[which(x_2006==highx,arr.ind = TRUE)]
### weather.com with 16.28118

### 3 - LASSO
set.seed(27112019)
lasso.cv <- cv.glmnet(x_2006, y_2006, type.measure = "mse", family = "gaussian", alpha = 1,dfmax=2,intercept=FALSE)
coef_lasso1 <- coef(lasso.cv, s = "lambda.min")
print(coef_lasso1)
sort(coef_lasso1)



### The two best linear predictors are officedepot.com and staples.com 

x_2006[1,1000]



