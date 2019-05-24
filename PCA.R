#### question 2
mydata = read.table("prostate.txt", header=T)

############# a

index.na=apply(is.na(mydata), 1, any)
## identify cases having NA values.
newdata=mydata[index.na==FALSE,] ## drop cases with NA entries.
any(is.na(newdata)) ## this should return FALSE -- no NA in data.s
dim(newdata) ## this should return 374 6: 374 cases, 6 variables.

Y = newdata[,1]
X1 = ratio  = newdata[,2]
X2 = age  = newdata[,3]
X3 =  gender = newdata[,4]
X4 =  frame = newdata[,5]
X5 =  bp.1s = newdata[,6]
X=cbind(rep(1,374),X1,X2,X3,X4,X5)##design matrix X
dim(X)
###convert gender to numeric value
library(plyr) #using revalue or mapvalue
X3n <- revalue(newdata$gender, c("male"=0, "female"=1))
X3n
x3u=as.numeric(X3n)
x3u
hist(x3u)

###convert categorical qualitative value to indivcator
n=374
Xsmall=vector()# creat empty vector
for(i in 1:n){
  if(frame[i]=='small'){
    Xsmall[i] = 1
  }
  else{Xsmall[i]=0}
}
Xmedium = vector()
for(i in 1:n){
  if(frame[i]=='medium'){
    Xmedium[i] = 1
  }
  else{Xmedium[i]=0}
}

X=cbind(X,Xsmall,Xmedium) # add new xsmall and xmedium to old data - Lynna typed 

###factor to convert categorical to indicator
xnew = factor(newdata$frame)
xnew

4n <- revalue(newdata$frame, c("small"=1 , "medium"=2, "large"=3))#This is not indicator function
x4n
x4u=as.numeric(x4n)
x4u
############ b 
hist(Y)
hist(X1) ## heavly skewed to the right
hist(X2) ## slightly skewed to the right
##hist(X3) ##hist(X4) Qualitative variable no histogram
hist(X5) ## Skewed ro the right

pairs(newdata)

#Pie chart for X3 Gender ???? we dont know how to count male or female to see which one is bigger
mytable = table(x3u)
lbls <- c("female", "male")
pie(mytable, labels = lbls, main="Pie Chart Gender")

#Pie chart for X4 Frame Size  
mytable = table(X4)
lbls <- c("Small", "Medium", "Large")
pie(mytable, labels = lbls, main="Pie Chart Frame Size")

############### C

# install.packages("MASS")
library(MASS)
eY = (Y)^-1 ##transformation for Y
boxcox(lm(eY~X1+X2+X3+X4+X5)) ##to find if we need transformation  exponentioal y  - best one because its closes to one
hist(eY)
logY = log(Y) ##transformation for Y
boxcox(lm(logY~X1+X2+X3+X4+X5)) ##to find if we need transformation   reverse y
hist(logY)
squarerootY = (Y)^(1/2) ##transformation for Y
boxcox(lm(squarerootY~X1+X2+X3+X4+X5)) ##to find if we need transformation   square root of y
hist(squarerootY)

################# D
#for original glybh no transformation
plot(Y,X1) 
plot(Y,X2)
plot(Y,X5)

summary(lm(Y~X1+X2+X5))$adj.r.squared  

#for transformed Y
plot(eY,X1)
plot(eY,X2)
plot(eY,X5)

summary(lm(eY~X1+X2+X5))##$adj.r.squared   - only shows adj r square from summary 

################## E
#for original glybh no transformation - 
boxplot(Y~X3) # male vs female
boxplot(Y~X4) # Frame size

#for transformed Y  - when we used transformed model the glybh number become smaller, it turns out the larger frame has lower Glycosolated Hemoglobin compare to original witout transformatio 
boxplot(eY~X3)
boxplot(eY~X4)

############### Diagnostic and remedy F
### fitted value vs resedual & QQ plot  model 1 - Y
fit <- lm(Y~X1+X2+X3+X4+X5)
par(mfrow=c(2,2))
plot(fit)   ### model assumption doesnt hold in my point view 

summary(fit)

############### G 
library(MASS)
boxcox(lm(eY~X1+X2+X3+X4+X5)) ##to find if we need transformation  exponentioal y  - best one because its closes to one
eY = (Y)^-1 ##transformation for Y   

############### H 
### fitted value vs resedual & QQ plot model 2 - eY
fit <- lm(eY~X1+X2+X5)
par(mfrow=c(2,2))
plot(fit)   ### model assumption hold linearity compare the original form without transformation  

confint(fit,level = 0.95)

############# I building regression model

summary(lm(eY~X1+X2+x3u+x4u+X5)) # we will iluminate X4 b/c has small F value so we fail to reject null hypothesis which is X4=0
anova(lm(eY~X1+X2+X5))# new regression model Y= 0.2891146-.0086356X1-0.0011328X2

library(leaps)##doing best subset sellection
newdata2 = cbind(rep(1,374),X1,X2,x3u,Xsmall,Xmedium, X5)

leaps1 = regsubsets(newdata2,eY,nbest=1)
leaps1
cor(newdata2)
#obtain the best model for each value of p
summary(leaps1)

#obtain the cp for each best model
Cp = summary(leaps1)$cp
Cp

#obtain the rsquared for each best model
Rp = summary(leaps1)$rsq
Rp
#obtain the adjusted r squared for each best model
Rap = summary(leaps1)$adjr2
Rap
#obtain the AIC and BIC for each best model
SSE = summary(leaps1)$rss
n = dim(newdata)[1]
p = rowSums(summary(leaps1)$which)
AIC = n*log(SSE)-n*log(n)+2*p
BIC = n*log(SSE)-n*log(n)+p*log(n)
AIC  
BIC
############ j


anova(lm(eY~X1+X2+X3+X5)) # so we droped the x4 and then we checked our model and it's still X3 f value is small so we iluminate X3 too
summary(lm(eY~X1+X2+X3+X5))

