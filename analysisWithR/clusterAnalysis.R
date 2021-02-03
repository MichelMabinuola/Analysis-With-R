library(tidyverse)
library(datasets)
head(iris)
plot(iris$Species)
plot(iris$Species, iris$Sepal.Length)
plot(iris$Petal.Length)
plot(iris)
plot(iris$Petal.Length, iris$Petal.Width,
     col='red',
     pch=19,
     main="Iris: Petal Length vs Petal Width",
     xlab= 'Petal Length',
     ylab = 'Petal Width')

head(mtcars)
cylindars <- table(mtcars$cyl)
barplot(cylindars)
plot(cylindars)

hist(iris$Sepal.Length)
hist(iris$Sepal.Width)
hist(iris$Petal.Length)
hist(iris$Petal.Width)

par(mfrow = c(3,1))
hist(iris$Sepal.Length)
hist(iris$Sepal.Width)
hist(iris$Petal.Length)

hist(iris$Petal.Width[iris$Species == 'setosa'],
     main = 'Iris Setossa',
     col = 'red',
     xlim = c(0,3),
     xlab = '',
     breaks = 9)

hist(iris$Petal.Width[iris$Species == 'versicolor'],
     main = 'Iris Versicolor',
     col = 'purple',
     xlim = c(0,3),
     xlab = '',
     breaks = 9)

hist(iris$Petal.Width[iris$Species == 'virginica'],
     main = 'Iris virginica',
     col = 'green',
     xlim = c(0,3),
     xlab = '',
     breaks = 9)

par(mfrow = c(1,1))

plot(mtcars$wt, mtcars$mpg, 
     pch = 19,
     cex = 1.5,
     col = '#ccc000',
     xlab = 'weight',
     ylab = 'MPG',
     main = 'MPG as a function of weight of cars')

head(lynx)
hist(lynx,
     breaks = 14,
     freq = FALSE,
     col = 'thistle1',
     main = paste('Histogram of Annual',
                  'Trappings, 1821-1934'))

curve(dnorm(x, mean = mean(lynx), sd = sd(lynx)), lwd = 2,
      add = TRUE)
lines(density(lynx), col='red', lwd = 2)
lines(density(lynx, adjust = 3), col='purple', lwd = 2)
rug(lynx, col='gray', lwd=2)

summary(iris$Species)
summary(iris)
summary(iris$Sepal.Length)

install.packages('psych')
library(psych)

describe(iris$Sepal.Length)
describe(iris)

hist(iris$Sepal.Length)

#select variables
cars <- mtcars[, c(1:4, 6:7, 9:11)]
cars

# %>% this is a pipe. Take the result of one step
# feed it directly as the input data of the next step
hc <- cars  %>%
  dist %>%
  hclust
plot(hc)
rect.hclust(hc, k=2, border='gray')
rect.hclust(hc, k=3, border='blue')
rect.hclust(hc, k=4, border='green')
rect.hclust(hc, k=5, border='darkred')

pc <- prcomp(cars, center = T, scale = T)
summary(pc)
plot(pc)
pc
predict(pc) %>% round(2)

# graph
biplot(pc)

data <- USJudgeRatings
head(data)
x <- as.matrix(data[-12]) #read all data except the 12
y <- data[,12]

reg1 <- lm(y ~ x)
# specify variables individually
#reg1 <- lm(RTEN ~ CONT + ING + DMNR,
           #data = USJudgeRatings)


reg1
summary(reg1)
anova(reg1)
coef(reg1)
confint(reg1)
hist(residuals(reg1))

#lars, caret
# convetional stepwise regression
#stepwise <- lars(x,y,type='stepwise')
#stagewise <- lars(x,y,type='forward.stagewise')
#lar <- lars(x,y,type='lar')
#lasso <- lars(x,y,type='lasso')



























