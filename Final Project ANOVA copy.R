#Load in Libraries
library("dplyr")
library("rcompanion")
library("car")

Top_YouTube_Channels_Data_

#Question: Is there a difference in subribers among the YouTube categories of music, shows and gaming? 

#Data Wrangling
#Filter the Data and Remove Missing Values
apps <- na.omit(Top_YouTube_Channels_Data_ %>% filter(category %in% c("Music", "Shows", "Gaming")))

#Test Assumptions
#Normality
plotNormalHistogram(apps$subscribers)
#Looks like it isn't normal- it is positively skewed.

#Try to transform subscribers by square rooting or cube rooting the column.
apps$subscribersSQRT <- sqrt(apps$subscribers)
plotNormalHistogram(apps$subscribersSQRT)
#A little better, but not great.

#Try cubing it:
apps$subscribersCUBE <- apps$subscribers ^ 3
plotNormalHistogram(apps$subscribersCUBE)
#Looks like neither of these are really any better than the original, 
#so  might as well keep the original data to ease interpretation. 

#Homogeneity of Variance
##Bartlett's Test
bartlett.test(subscribers ~ category, data=apps)
#p-value >.05 so we have not violated homogeneity of variance.

##Fligner's Test
fligner.test(subscribers ~ category, data=apps)
#p-value also >.05.

#Computing ANOVAs with Equal Variance (Met Homogeneity of Variance Assumption)
appsANOVA <- aov(apps$subscribers ~ apps$category)
summary(appsANOVA)

#The p value is above 0.05 and there's no star next it, so it is not significant.
