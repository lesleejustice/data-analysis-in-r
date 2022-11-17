#mixed measure anovas
library('rcompanion')
library('car')
library('IDPmisc')
library('dplyr')

#load in data

#question set up: You will determine whether suicide rates (suicides/100k pop) has changed over the years (year), and see if the generation has any influence
#IV or x : generation
#2nd IV or x: year
#DV or y: suicide rates

#wrangling data is done the same way as repeated measure ANOVAS
NaRV.omit(suicide)

#change data shape
keeps <- c("Participant.Code", "Treatment.Group", "Age..y.", "Sex", "Height..m.", "Baseline.Resting.Metabolic.Rate..kcal.d.", "Follow.Up.Resting.Metabolic.Rate..kcal.d.")
breakfast2 <- breakfast1[keeps]
#why is there are an error here? 

#test assumptions: same as repeated measure ANOVA with difference in sample size because there are two IVs


#sample size: mixed measure ANOVA requires a sample size of at least 20 per independent varaible/time factor. So for this question, you need 40. There are only n=33. If you don't have enough, typically, you would simplify the model, choose a different analysis or run boot strapping 

#Analysis: use aov() function and add arguments
RManova1 <- aov(repdat~(Treatment.Group*contrasts)+Error(Participant.Code/(contrasts)), breakfast5)
summary(RManova1)



