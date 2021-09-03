
Housing <- read.csv("D:/R/Q6 Quantative Analysis R1-12/housing.csv", sep=",", header=TRUE) 
names(Housing)
str(Housing)

results = lm(Price ~ Stories + Bedrooms + Lot, data=Housing)


#A. Studying the Variables
pairs(~ Price + Stories + Bedrooms + Lot, data=Housing)

# B. Residuals and Leverage
results = lm(Price ~ Stories + Bedrooms + Lot, data=Housing)
?hat()
lev = hat(model.matrix(results))
plot(lev)

Housing[lev > 0.2,]
Housing[lev > 0.05,]

#' To study the influence of this particular point we can 
#' make scatter plots and specifically mark this point using the points() function.

plot(Housing$Stories, Housing$Price)
points(Housing[369,]$Stories, Housing[369,]$Price, col='red')
plot(Housing$Lot, Housing$Price)
points(Housing[369,]$Lot, Housing[369,]$Price, col='red')

plot(results$res, Housing$Price)
plot(Housing$Price, results$res)
abline(h=0)

results = lm(Price ~ Stories + Bedrooms + Lot, data=Housing)

# Prior to studying the residuals it is common to standardize them 
# to compensate for differences in leverage. The studentized residuals are given by
r = rstudent(results)
mean(r)
sd(r)

# An influential point is one if removed from the data would 
# significantly change the fit. An influential point may either be 
# an outlier or have large leverage, or both

# Typically, points with Ci greater than 1 are classified as being influential
cook = cooks.distance(results)
plot(cook,ylab="Cooks distances")
points(369,cook[369],col='red')


# if we need to remove an observation from the data set and refit the model, we can use the command
Housing2 = Housing[-64,]


# C. Residual Plots
attach(Housing)
par(mfrow=c(1,3))
plot(Stories, results$res)
plot(Lot, results$res)
plot(results$fitted, results$res)

r = rstudent(results)
plot(Stories, r)
plot(Lot, r)
plot(results$fitted, r)

qqnorm(results$res)
qqline(results$res)
hist(results$res)



