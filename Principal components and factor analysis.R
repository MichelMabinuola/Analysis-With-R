#; 3.1 PCA of a two-variable matrix

library(readr)
boxes <- read_csv("D:/R/Q6 Quantitative Analysis R1-12/boxes.csv")

# boxes.pca -- principal components analysis of Davis boxes data
boxes.matrix <- data.matrix(cbind(boxes[,1],boxes[,4]))
dimnames(boxes.matrix) <- list(NULL, cbind("long","diag"))
plot (boxes.matrix)

cor(boxes.matrix)
#' the princomp() function from the stats package. 
# The loadings() function extracts the loadings or the correlations 
# between the input variables and the new components, and 
# the the biplot() function creates a biplot a single figure 
# that plots the loadings as vectors and the component scores as points represented by the observation numbers.

boxes.pca <- princomp(boxes.matrix, cor=T)
boxes.pca

summary(boxes.pca)
print(loadings(boxes.pca),cutoff=0.0)
biplot(boxes.pca)

#' ote the angle between the vectors–the correlation between two variables is 
#' l to the cosine of the angle between the vectors (θ), or r = cos(θ). 
#'  the angle is 24.3201359, which is found by the following R code:  
acos(cor(boxes.matrix[,1],boxes.matrix[,2]))/((2*pi)/360)

# The components can be drawn on the scatter plot as follows,

# get parameters of component lines (after Everitt & Rabe-Hesketh)
load <- boxes.pca$loadings
slope <- load[2,]/load[1,]
mn <- apply(boxes.matrix,2,mean)
intcpt <- mn[2]-(slope*mn[1])

# scatter plot with the two new axes added
par(pty="s") # square plotting frame
xlim <- range(boxes.matrix) # overall min, max
plot(boxes.matrix, xlim=xlim, ylim=xlim, pch=16, col="purple") # both axes same length
abline(intcpt[1],slope[1],lwd=2) # first component solid line
abline(intcpt[2],slope[2],lwd=2,lty=2) # second component dashed
legend("right", legend = c("PC 1", "PC 2"), lty = c(1, 2), lwd = 2, cex = 1)

# projections of points onto PCA 1
y1 <- intcpt[1]+slope[1]*boxes.matrix[,1]
x1 <- (boxes.matrix[,2]-intcpt[1])/slope[1]
y2 <- (y1+boxes.matrix[,2])/2.0
x2 <- (x1+boxes.matrix[,1])/2.0
segments(boxes.matrix[,1],boxes.matrix[,2], x2, y2, lwd=2,col="purple")

## 3.2 A second example using the large-cites data set
cities <- read_csv("D:/R/Q6 Quantitative Analysis R1-12/cities.csv")
head(cities)
cities.matrix <- data.matrix(cities[, 2:12])
cities.matrix

rownames(cities.matrix) <- cities[,1] # add city names as row labels  ??????????????

plot(cities[,2:12], pch=16, cex=0.6)
cor(cities[,2:12])

library(corrplot)
corrplot(cor(cities[,2:12]), method="ellipse")
# An alternative is simply fill each cell with an appropriate color and shade.

corrplot(cor(cities[,2:12]), method="color")

##/ 3.2.2 PCA of the cities data
# Here’s the principal components analysis of the cities data:
  
cities.pca <- princomp(cities.matrix, cor=T)
cities.pca
summary(cities.pca)
screeplot(cities.pca)
loadings(cities.pca)
biplot(cities.pca, col=c("black","red"), cex=c(0.7,0.8))
# An alternative visualization of the principal component and their relationship with the original variables
qg.pca <- qgraph.pca(cities[,2:12], factors=2, rotation="none")

## 3.3 “Rotation” of principal components
library(psych)
cities.pca.unrot <- principal(cities.matrix, nfactors=2, rotate="none")
cities.pca.unrot
summary(cities.pca.unrot)
biplot(cities.pca.unrot, labels=rownames(cities.matrix), cex=0.5, col=c("black","red"))
qg.pca <- qgraph(cities.pca.unrot)  # ????

#  Here’s the result with rotated components:

cities.pca.rot <- principal(cities.matrix, nfactors=2, rotate="varimax")
cities.pca.rot
summary(cities.pca.rot)

biplot.psych(cities.pca.rot, labels=rownames(cities.matrix), col=c("black","red"), cex=c(0.7,0.8),
             xlim.s=c(-3,3), ylim.s=c(-2,4))

### 4.1 Example of a factor analysis
# cities.fa1 -- factor analysis of cities data -- no rotation
cities.fa1 <- factanal(cities.matrix, factors=2, rotation="none", scores="regression")
cities.fa1
