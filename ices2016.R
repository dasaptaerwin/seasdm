#load libraries
install.packages("ggplot2")
library(ggplot2)
library(gridExtra)

#open and summarize data
df = data.frame(read.csv("ta_tss.csv"))
summary(df)

#evaluate data
## histograms
histFeb <- ggplot(df, aes(x=Feb)) +
  geom_histogram(binwidth=.5, colour="black", fill="white")
histMar <- ggplot(df, aes(x=Mar)) +
  geom_histogram(binwidth=.5, colour="black", fill="white")
histApr <- ggplot(df, aes(x=Apr)) +
  geom_histogram(binwidth=.5, colour="black", fill="white")
histMay <- ggplot(df, aes(x=May)) +
  geom_histogram(binwidth=.5, colour="black", fill="white")
histJun <- ggplot(df, aes(x=Jun)) +
  geom_histogram(binwidth=.5, colour="black", fill="white")
histJul <- ggplot(df, aes(x=Jul)) +
  geom_histogram(binwidth=.5, colour="black", fill="white")
histAug <- ggplot(df, aes(x=Aug)) +
  geom_histogram(binwidth=.5, colour="black", fill="white")

grid.arrange(histFeb, histMar, histApr, 
             histMay, histJun, histJul, 
             histAug, ncol=3)

## regressions
lmFeb <- ggplot(df, aes(x=Feb, y=z)) +
  geom_point(shape=1) +    # Use hollow circle
  geom_smooth(method=lm) 
lmMar <- ggplot(df, aes(x=Mar, y=z)) +
  geom_point(shape=1) +    # Use hollow circle
  geom_smooth(method=lm) 
lmApr <- ggplot(df, aes(x=Apr, y=z)) +
  geom_point(shape=1) +    # Use hollow circle
  geom_smooth(method=lm) 
lmMay <- ggplot(df, aes(x=May, y=z)) +
  geom_point(shape=1) +    # Use hollow circle
  geom_smooth(method=lm) 
lmJun <- ggplot(df, aes(x=Jun, y=z)) +
  geom_point(shape=1) +    # Use hollow circle
  geom_smooth(method=lm) 
lmJul <- ggplot(df, aes(x=Jul, y=z)) +
  geom_point(shape=1) +    # Use hollow circle
  geom_smooth(method=lm) 
lmAug <- ggplot(df, aes(x=Aug, y=z)) +
  geom_point(shape=1) +    # Use hollow circle
  geom_smooth(method=lm) 

grid.arrange(lmFeb, lmMar, lmApr, 
             lmMay, lmJun, lmJul, 
             lmAug, ncol=3)

## bubble plots
ggplot(df, 
       aes(x=Station, 
           y=z, 
           size=Feb, 
           #label=state#),
       guide=FALSE)+ theme_bw()
       
       
       
  geom_point(colour="white", 
             fill="red", 
             shape=21)
  
  
  
  
  # + 
  scale_size_area(max_size = 15)+
  scale_x_continuous(name="Murders per 1,000 population", limits=c(0,12))+
  scale_y_continuous(name="Burglaries per 1,000 population", limits=c(0,1250))+
  geom_text(size=4)+
  theme_bw()
ggplot(df, 
       aes(x=murder, 
           y=burglary, 
           size=population, 
           label=state),
       guide=FALSE)+
  geom_point(colour="white", 
             fill="red", 
             shape=21) + 
  scale_size_area(max_size = 15)+
  scale_x_continuous(name="Murders per 1,000 population", limits=c(0,12))+
  scale_y_continuous(name="Burglaries per 1,000 population", limits=c(0,1250))+
  geom_text(size=4)+
  theme_bw()
