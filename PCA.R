install.packages("psych", dependencies=TRUE)
library(psych)
hozdata <-read.delim(file=file.choose()) #the required file is available to download named grnt_fem.dat
# put the data into a matrix of correlations
hozdatamatrix <- cor(hozdata)
# print out the correlation matrix but ask for numbers to 4 decimal places
round(hozdatamatrix,4)
# bartlett test - want a small p value here to indicate c0rrelation matrix not zeros
cortest.bartlett(hozdata)
# unable to calculate the kmo
# but can do the determinant need it to be above 0.00001
# to be able to continue
det(hozdatamatrix)
# appropriate value therefore can continue
# do a pca analysis use the principal function in the psych package
model1<- principal(hozdata, nfactors = 6, rotate = "none")
model1
# get the scree plot
plot(model1$values, type = "b")
# now know how many components we want to extract = 2
# rerun the anylsis specifying this
model2 <- principal(hozdata, nfactors = 2, rotate = "none")
model2
# can find the reproduced correlations and the communalities (the diagonals)
factor.model(model2$loadings)
# can also find the differences between the observed and model estimated correlations
# the diagonals represent the uniqueness values (1- R squared):
residuals <- factor.residuals(hozdatamatrix, model2$loadings)
residuals
# nice to plot the residuals to check there are normally distributed
hist(residuals)
# now to the rotation
model3 <- principal(hozdata, nfactors = 2, rotate = "varimax")
model3
# can get the loading matrix to stop printing out loading below
# a specific value say 0.3 cna also get it sorted by size of loading
# h2 is the communality; u2 is the uniqueness
print.psych(model3, cut = 0.3 , sort = TRUE)
# now to do a principal axis factor analysis
# fa means factoring method; rotate options=none/varimax/blimin/promax etc.
model4 <- fa(hozdata, nfactors = 2,fm = "pa", rotate = "none")
model4
# repeat the analysis with a varimax rotation
model5 <- fa(hozdata, nfactors = 2,fm = "pa", rotate = "varimax")
model5
# repeat the analysis with a promax rotation (correlated factors)
model6 <- fa(hozdata, nfactors = 2,fm = "pa",rotate="promax")
model6
# the factor loadings in the above are not the same as that in SPSS
# this is because SPSS scales the values using something called Kaiser normalisation
# the psych package provides a function to do this
# best to input the non rotated form into the function (info. from help file)
model7 <- kaiser(model4, rotate="promax")
model7
#the above output is slight more like that of SPSS
# to obtain factor scores
# the for PCA we just add scores = true
model3a <- principal(hozdata, nfactors = 2, rotate = "varimax", scores = TRUE)
model3a
# to print out all the scores:
model3a$scores
# to print out just the top 10 scores:
head(model3a$scores, 10)
# to save the above values we need to add them to a dataframe
factorscores <- cbind(model3a$scores)
# then we can produce a plot of the scores:
plot(factorscores)