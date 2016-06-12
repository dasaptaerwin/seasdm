#load libraries
install.packages("ggplot2")
library(ggplot2)
library(gridExtra)
install.packages("cowplot")
library(cowplot)

#open and summarize data
df = data.frame(read.csv("ta_tss.csv"))
dfw = data.frame(read.csv("weda_tss.csv"))
summary(df)
summary(dfw)

#evaluate data
## histograms
histFeb = ggplot(df, aes(x=Feb)) +
  geom_histogram(binwidth=.5, colour="black", fill="white")
histMar = ggplot(df, aes(x=Mar)) +
  geom_histogram(binwidth=.5, colour="black", fill="white")
histApr = ggplot(df, aes(x=Apr)) +
  geom_histogram(binwidth=.5, colour="black", fill="white")
histMay = ggplot(df, aes(x=May)) +
  geom_histogram(binwidth=.5, colour="black", fill="white")
histJun = ggplot(df, aes(x=Jun)) +
  geom_histogram(binwidth=.5, colour="black", fill="white")
histJul = ggplot(df, aes(x=Jul)) +
  geom_histogram(binwidth=.5, colour="black", fill="white")
histAug = ggplot(df, aes(x=Aug)) +
  geom_histogram(binwidth=.5, colour="black", fill="white")

grid.arrange(histFeb, histMar, histApr, 
             histMay, histJun, histJul, 
             histAug, ncol=3)

## regressions
lmFeb = ggplot(df, aes(x=Feb, y=z)) +
  geom_point(shape=1) +    # Use hollow circle
  geom_smooth(method=lm) +
  ylab("depth (m)") +
  theme_gray()
lmFeb = ggdraw(switch_axis_position(lmFeb, axis = 'x'))

lmMar = ggplot(df, aes(x=Mar, y=z)) +
  geom_point(shape=1) +    # Use hollow circle
  geom_smooth(method=lm) +
  ylab("depth (m)")  +
  theme_gray()
lmMar = ggdraw(switch_axis_position(lmMar, axis = 'x'))

lmApr = ggplot(df, aes(x=Apr, y=z)) +
  geom_point(shape=1) +    # Use hollow circle
  geom_smooth(method=lm) +
  ylab("depth (m)") +
  theme_gray() 
lmApr = ggdraw(switch_axis_position(lmApr, axis = 'x'))

lmMay = ggplot(df, aes(x=May, y=z)) +
  geom_point(shape=1) +    # Use hollow circle
  geom_smooth(method=lm)  +
  ylab("depth (m)") +
  theme_gray()
lmMay = ggdraw(switch_axis_position(lmMay, axis = 'x'))

lmJun = ggplot(df, aes(x=Jun, y=z)) +
  geom_point(shape=1) +    # Use hollow circle
  geom_smooth(method=lm) +
  ylab("depth (m)") +
  theme_gray() 
lmJun = ggdraw(switch_axis_position(lmJun, axis = 'x'))

lmJul = ggplot(df, aes(x=Jul, y=z)) +
  geom_point(shape=1) +    # Use hollow circle
  geom_smooth(method=lm) +
  ylab("depth (m)") +
  theme_gray() 
lmJul = ggdraw(switch_axis_position(lmJul, axis = 'x'))

lmAug = ggplot(df, aes(x=Aug, y=z)) +
  geom_point(shape=1) +    # Use hollow circle
  geom_smooth(method=lm)  +
  ylab("depth (m)") +
  theme_gray()
lmAug = ggdraw(switch_axis_position(lmAug, axis = 'x'))

grid.arrange(lmFeb, lmMar, lmApr, 
             lmMay, lmJun, lmJul, 
             lmAug, ncol=3)

dev.off()




## bubble plots
bf = ggplot(df, 
               aes(x=x,
                   y=y, 
                   size=Feb, 
                   label=z),
               guide=FALSE) +
  geom_point(colour="blue", 
             fill="blue", 
             shape=21)+
  theme_gray()

bm = ggplot(df, 
               aes(x=x,
                   y=y, 
                   size=Mar, 
                   label=z),
               guide=FALSE) +
  geom_point(colour="green", 
             fill="green", 
             shape=21)+
  theme_gray()

bap = ggplot(df, 
               aes(x=x,
                   y=y, 
                   size=Apr, 
                   label=z),
               guide=FALSE) +
  geom_point(colour="yellow", 
             fill="yellow", 
             shape=21)+
  theme_gray()

bmy = ggplot(df, 
               aes(x=x,
                   y=y, 
                   size=May, 
                   label=z),
               guide=FALSE) +
  geom_point(colour="orange", 
             fill="orange", 
             shape=21)+
  theme_gray()

bjn = ggplot(df, 
               aes(x=x,
                   y=y, 
                   size=Jun, 
                   label=z),
               guide=FALSE) +
  geom_point(colour="red", 
             fill="red", 
             shape=21)+
  theme_gray()

bjl = ggplot(df, 
               aes(x=x,
                   y=y, 
                   size=Jul, 
                   label=z),
               guide=FALSE) +
  geom_point(colour="black", 
             fill="black", 
             shape=21)+
  theme_gray()

bag = ggplot(df, 
               aes(x=x,
                   y=y, 
                   size=Aug, 
                   label=z),
               guide=FALSE) +
  geom_point(colour="gray", 
             fill="gray", 
             shape=21)+
  theme_gray()

grid.arrange(bf, bm, bap, 
             bmy, bjn, bjl, 
             bag, ncol=3)      
       
      

#### df2
df2 = data.frame(read.csv("ta_tss2.csv"))
