#install packages
install.packages("corpcor")
install.packages("GPArotation")
install.packages("psych")
install.packages("IDPmisc")

#load in libraries
library("corpcor")
library("GPArotation")
library("psych")
library("IDPmisc")

#load in data
head(studentSurvey)

#subset data for areas 1-12
studentSurvey1 <- studentSurvey[,31:42]

#remove all NA values 
studentSurvey2 <- NaRV.omit(studentSurvey1)

#test assumptions
#sample size is 500 

#absence of multicollinearity
studentSurveyMatrix <- cor(studentSurvey2)
#error is still being thrown, inspecting data
str(studentSurvey2)
ss3 <- data.matrix(studentSurvey2)
#used data.matrix to transpose columns to numeric from factor
print(ss3)
#let's try to run the matrix again
studentSurveyMatrix1 <- cor(ss3)
#viewing the matrix with 2 decimal places
View(studentSurveyMatrix1)
#running bartlett's test 
cortest.bartlett(studentSurveyMatrix1)
#chi-sq 696.06, p.value 6.32e-106
#the p-value is significant, there are suitable correlations to continue

#check determinants
det(studentSurveyMatrix1)
#sufficient correlation to continue with output at 0.0006161103 

#initial pass to determine approximate number of factors
pcModel1 <- principal(studentSurveyMatrix1, nfactors = 10, rotate = "none")
pcModel1
#eigenvalues for PC1 are 7.02 and need to evaluated further

#examining the scree plot
plot(pcModel1$values, type="b")
#there appears to be two factors at play

#second pass to test the suspected number of factors
pcModel2 <- principal(studentSurveyMatrix1, nfactors = 2, rotate = "none")
pcModel2

#examine the model fit through residuals
residuals <- factor.residuals(studentSurveyMatrix1, pcModel2$loadings)
residuals <- as.matrix(residuals[upper.tri(residuals)])
largeResid <- abs(residuals) > .05
sum(largeResid) #sum is 26
sum(largeResid/nrow(residuals))
#39% of the residuals are large, so having only two factors is a pretty good model fit for the data

#oblique rotation 
{r}
pcModel3 <- principal(ss3, nfactors = 2, rotate = "oblimin")
pcModel3
#I am not sure how to figure out what the two factors are at play even with knowing what loads are high
print.psych(pcModel3, cut = .3, sort=TRUE)
#well, the math still looks right, but I am not sure how to interpret this

#orthogonal rotation 
pcModel4 <- principal(ss3, nfactors = 2, rotate = "varimax")
print.psych(pcModel4, cut=.3, sort=TRUE)

#before calculating reliability, I need to determine how to explain what factors are at play
#I am going to try a method I saw on youtube... 
install.packages("REdaS")
library("REdaS")
bart_spher(ss3)
### p-value is significant
###trying the kaiser-meyer-olkin measure, it needs to be above .7
KMO(ss3)
###overall MSA = .95
fa(ss3, 12, rotate = "oblimin")
#### there are 4 factors that are significant at over 1, but I think it should probably be fixed to two
### fixing to 2 factors
fa(ss3, nfactors = 2, rotate = "oblimin")
#if loading is above .5, then it is valid and highly loaded
###save the analysis as object
fa(ss3, nfactors = 2, rotate = "oblimin")
m2 <- fa(ss3, nfactors = 2, rotate = "oblimin")
fa.diagram(m2, main = "Student Survey Factor Analysis")
###MR1 - Student support and guidelines
###MR2 - Curriculum and teaching 
#####conclusion: there are two subscales that should be considered in future surveys on student satisfaction.


#calculating reliability
#is my survey reliable?

#subsetting data
studentSupport <- ss3[c(1:8)]
#I don't think that worked... creating a character vector
curriculumSupportReverse = c("V39","V40", "V41", "V42")
#Maybe this will work...
reversed.curriculumSupportReverse[,curriculumSupportReverse] <- 6-curriculumSupportReverse[,curriculumSupportReverse]
#error incorrect number of dimensions
head(curriculumSupportReverse)
#well, maybe let's start a new script, erase the enviroment and restart.

#reverse coding curriculumSupport
reverse_cols = c(9:12)
curriculumSupport(, reverse_cols) = 5 - curriculumSupport(, reverse_cols)
