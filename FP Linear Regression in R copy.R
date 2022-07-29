install.packages("ggplot2")
install.packages("dplyr")
install.packages("broom")
install.packages("ggpubr")

library(ggplot2)
library(dplyr)
library(broom)
library(ggpubr)
library(tidyverse)

#Linear Regression Analysis

df=Top_YouTube_Channels_Data_

summary(df)

#Rename Video views to videoviews

colnames(df)[colnames(df) == "video views"] = "videoviews"

model <- lm(subscribers ~ videoviews, data = df)
model

summary(model)


ggplot(df, aes(videoviews, subscribers)) +
  geom_point() +
  stat_smooth(method = lm)
 