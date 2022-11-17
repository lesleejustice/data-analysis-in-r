library("rcompanion")
install.packages("fastR2")
library("fastR2")
library("car")

#data - the effect of eating breakfast on weight loss
#question - Overall, regardless of whether participants ate breakfast or not, did people in this study show improvement in their resting metabolic rate?   

# the IV or x will be the time factor baseline or follow up. The y or DV will be the change in resting metabolic rate from baseline to follow up. As with all anovas - IV will be categorical and DV will be continuous. 

# Subsetting the data to get rid of unnecessary rows and columns

breakfast1 <- breakfast[1:33,1:7]

keeps <- c("Participant.Code", "Treatment.Group", "Age..y.", "Sex", "Height..m.", "Baseline.Resting.Metabolic.Rate..kcal.d.", "Follow.Up.Resting.Metabolic.Rate..kcal.d.")
breakfast2 <- breakfast1[keeps]

# Getting the data in the right shape for the baseline measure.
breakfast3 <- breakfast2[,1:5]
breakfast3$repdat <- breakfast2$Baseline.Body.Mass..kg.
breakfast3$contrasts <- "T1"

# Getting the data in the right shape for the folow-up measure.
breakfast4 <- breakfast2[,1:5]
breakfast4$repdat <- breakfast2$Follow.Up.Body.Mass..kg.
breakfast4$contrasts <- "T2"

# Then smoosh 'em back together with binding
breakfast5 <- rbind(breakfast3, breakfast4)

# Testing for Normality

plotNormalHistogram(breakfast1$Baseline.Body.Mass..kg.)
plotNormalHistogram(breakfast1$Follow.Up.Body.Mass..kg.)

# They look approximately normal, so don't need transformation

# Testing for Homogeneity of Variance

leveneTest(repdat ~ Treatment.Group*contrasts, data=breakfast5)

# It was not significant, which means this assumption has been met

RManova2 <- aov(repdat~contrasts+Error(Participant.Code), breakfast5)
summary(RManova2)

# Nothing was significant here either!

#Honey Production Hands On 
library("rcompanion")
library("fastR2")
library("car")
library("dplyr")

#Load in Data
honey.df <- read.csv("honey.csv")

#Data Wrangling
honey.df$year <- as.character(honey.df$year)
honey.df$year <- as.factor(honey.df$year)

#Postively skewed
plotNormalHistogram(honey.df$totalprod)

#Log transformation looks great
plotNormalHistogram(log(honey.df$totalprod))

honey.df$totalprodLOG <- log(honey.df$totalprod)

#Check for Assumptions

#Passed assumption of homogenity of variance for normally distributed variable
leveneTest(totalprodLOG ~ year, data=honey.df)

#Run the Analysis
RManova <- aov(totalprodLOG~year+Error(state), honey.df)
summary(RManova)

RManova <- aov(log(totalprod)~year, honey.df)
summary(RManova)



