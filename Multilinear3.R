Computer <- read.csv(file.choose()) # choose the Cars.csv data set

View(Computer)
Computer<-Computer[,-1]

library(plyr)

Computer$cd <- as.numeric(revalue(Computer$cd,c("yes"=1,"no"=0)))
Computer$multi <- as.numeric(revalue(Computer$multi,c("yes"=1,"no"=0)))
Computer$premium <- as.numeric(revalue(Computer$premium,c("yes"=1,"no"=0)))

View(Computer)
summary(Computer)

attach(Computer)
boxplot(Computer) ##it shows all data with together if we wants to look single data do (plot)


# 8. Correlation Coefficient matrix - Strength & Direction of Correlation##always take and check down of the digonal
cor(Computer)

### Partial Correlation matrix - Pure Correlation  b/n the varibles
#install.packages("corpcor")
library(corpcor)

cor2pcor(cor(Computer))

# The Linear Model of interest with all the columns
model.Price <- lm(price~speed+hd+ram+screen+cd+multi+premium+ads+trend)

summary(model.Price)

# Multicollinearity check
# Model based on only Doors 
model.price1<-lm(price~hd)
summary(model.price1) # Does became significant
model.price2 <-lm(price~price)
summary(model.price2)

library(car)
influence.measures(model.Price)#Influence model
influenceIndexPlot(model.Price,id.n=3) 
model.price4<-lm (log(price) ~log(speed)+log(hd)+log(ram)+log(screen)+log(cd)+log(multi)+log(premium)+log(ads)+log(trend),data = Computer[-c(1441,1701),])#deleted number from rows1441,1701
summary(model.price4)


## vif>10 then there exists collinearity among all the variables 
library(MASS)
vif(model.Price)
avPlots(model.Price,id.n=3,id.cex=0.7) #added variable plot check correlation b/w  variables and o/p variables
model.pricefinal  <- lm (price~ speed+hd+ram+screen+cd+multi+premium+ads+trend,data=Computer[-c(1441,1701),])
summary(model.pricefinal)
confint(model.pricefinal,level = 0.95)
predict(model.pricefinal,level = 0.95)
#Evaluate LINE model assumptions
plot(model.pricefinal)

