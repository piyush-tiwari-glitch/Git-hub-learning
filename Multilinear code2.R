Computer<- read.csv(file.choose()) 

View(Computer)
attach(Computer)

Computer<-Computer[,-1] #Computer$X<- Null both function to use remove column or row data.

library(plyr)     # use this package to change the character variable to numeric

Computer$cd <-as.numeric(revalue(Computer$cd,c("no"="0","yes"="1")))
Computer$multi <- as.numeric(revalue(Computer$multi,c("no"=0,"yes"="1")))
Computer$premium <- as.numeric(revalue(Computer$premium,c("no"=0,"yes"=1)))
View(Computer)

Computer<-(Computer)
attach(Computer)

summary(Computer)


boxplot(price,horizontal =TRUE)
boxplot(speed,horizontal = TRUE)
boxplot(hd,horizontal = TRUE)
boxplot(ram,horizontal = TRUE)
boxplot (screen,horizontal = TRUE)
boxplot(cd,horizontal = TRUE)
boxplot(multi,horizontal = TRUE)
boxplot(premium,horizontal = TRUE)
boxplot(ads,horizontal = TRUE)
Computer$trend <- NULL
plot(Computer)

pairs(Computer)  #find the correleation b/w output


# 8. Correlation Coefficient matrix - Strength & Direction of Correlation##always take and check down of the digonal
cor(Computer)

### Partial Correlation matrix - Pure Correlation  b/n the varibles
#install.packages("corpcor")
library(corpcor)

cor2pcor(cor(Computer))

# The Linear Model of interest with all the columns
model.Price <- lm (price ~ speed+hd+ram+screen+cd+multi+premium+ads) #all are significant but R^2 value is less to incerae to use Log

summary(model.Price)
model.Price1 <- lm (log(price) ~ log(speed)+log(hd)+log(ram)+log(screen)+(cd)+log(multi)+log(premium)+log(ads))
summary(model.Price1)


# It is better to delete influential observations not entire column for to check influentia install library(car)

library(car) 

influence.measures(model.Price)#Influence model function enter 1st lm function model

influenceIndexPlot(model.Price,id.n=3) #Index plot for data measure as per thumb rule 81 is more than 0.05 so we can delete it(no.). 
influencePlot(model.Price,id.n=3)   

model.Price2 <-lm (price ~ speed+hd+ram+screen+cd+multi+premium+ads,data = Computer[901,1102,])
summary(model.Price2) 
influencePlot(model.Price2,id.n=3)   



##  check the collinearity we can use vif>10 then there exists collinearity among all the variables (Vif-variance inflation factor)
library(MASS)

vif(model.Price)

avPlots(model.Price,id.n=3,id.cex=0.7) #added variable plot check correlation b/w  variables and o/p variables

model.pricefinal  <- lm (price ~ speed+hd+ram+screen+premium+ads,data = Computer[901,1102,]) # remove multi&cd from the model 
model.pricefinallog  <- lm (log(price) ~ log(speed)+log(hd)+log(ram)+log(screen)+log(premium)+log(ads),data = Computer[901,1102,]) # 
summary(model.pricefinallog)
#Evaluate LINE model assumptions ( linear assumptions- error are independly disributed)
plot(model.pricefinallog)

#################################################################

