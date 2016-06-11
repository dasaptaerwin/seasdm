#load libraries
install.packages("ggplot2")
library(ggplot2)
#open and summarize data
df = data.frame(read.csv("ta_tss.csv"))
summary(df)
hist("feb")
