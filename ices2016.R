#load libraries
install.packages("ggplot2")
library(ggplot2)
library(gridExtra)
install.packages("cowplot")
library(cowplot)
library(mgcv)

#open and summarize data
dfa = data.frame(read.csv("ta_tss.csv"))
dfa1 = data.frame(read.csv("ta_tss_1.csv"))
dfw = data.frame(read.csv("weda_tss.csv"))
summary(df)
summary(dfw)

#evaluate data
## histograms
### histograms total ambon and weda 
hist_total = ggplot(df, aes(x=tss)) +
  geom_histogram(binwidth=.5, colour="black", fill="black") + 
  ylim(0, 10) +
  xlim(0, 2100) +
  theme_gray() +
  xlab("TSS Ambon Bay (ppm)")

hist_total_weda = ggplot(dfw, aes(x=tss)) +
  geom_histogram(binwidth=.5, colour="black", fill="black") + 
  ylim(0, 10) +
  xlim(0, 2100) +
  theme_gray() +
  xlab("TSS Weda (ppm)")

grid.arrange(hist_total, hist_total_weda, ncol=1)
dev.off()

### histograms ambon bay per month
histFeb = ggplot(dfa, aes(x=Feb)) +
  geom_histogram(binwidth=.5, colour="black", fill="black") + 
  ylim(0, 5) +
  xlim(0, 2100) +
  theme_gray() +
  xlab("TSS Feb Ambon Bay (ppm)")

histMar = ggplot(dfa, aes(x=Mar)) +
  geom_histogram(binwidth=.5, colour="black", fill="black") + 
  ylim(0, 5) +
  xlim(0, 2100) +
  theme_gray() +
  xlab("TSS Mar Ambon Bay (ppm)")

histApr = ggplot(dfa, aes(x=Apr)) +
  geom_histogram(binwidth=.5, colour="black", fill="black") + 
  ylim(0, 5) +
  xlim(0, 2100) +
  theme_gray() +
  xlab("TSS Apr Ambon Bay (ppm)")

histMay = ggplot(dfa, aes(x=May)) +
  geom_histogram(binwidth=.5, colour="black", fill="black") + 
  ylim(0, 5) +
  xlim(0, 2100) +
  theme_gray() +
  xlab("TSS May Ambon Bay (ppm)")

histJun = ggplot(dfa, aes(x=Jun)) +
  geom_histogram(binwidth=.5, colour="black", fill="white") + 
  ylim(0, 5) +
  xlim(0, 2100) +
  theme_gray() +
  xlab("TSS Jun Ambon Bay (ppm)")

histJul = ggplot(dfa, aes(x=Jul)) +
  geom_histogram(binwidth=.5, colour="black", fill="white") + 
  ylim(0, 5) +
  xlim(0, 2100) +
  theme_gray() +
  xlab("TSS Jul Ambon Bay (ppm)")

histAug = ggplot(dfa, aes(x=Aug)) +
  geom_histogram(binwidth=.5, colour="black", fill="white") + 
  ylim(0, 5) +
  xlim(0, 2100) +
  theme_gray() +
  xlab("TSS Aug Ambon Bay (ppm)")

grid.arrange(histFeb, histMar, histApr, 
             histMay, histJun, histJul, 
             histAug, ncol=3)

## regressions ambon bay
### total
lm_ab = 
  ggplot(dfa1, aes(x=tss, y=z)) +
  geom_point(alpha=.5) +  
  geom_smooth(method=lm) +
  ylab("depth (m)") + 
  ylim(0, -75) +
  xlim(0, 2100) +
  xlab("TSS Ambon Bay (ppm)") +
  ylab("Sampling depth (m)") +
  theme_gray()

lm_weda = ggplot(dfw, aes(x=tss, y=z)) +
  geom_point(alpha=.5) +  
  geom_smooth(method=lm) +
  ylab("depth (m)") + 
  ylim(0, -75) +
  xlim(0, 2100) +
  xlab("TSS Weda (ppm)") +
  ylab("Sampling (m)") +
  theme_gray()
  
lm_ab = ggdraw(switch_axis_position(lm_ab, axis = 'x'))
lm_weda = ggdraw(switch_axis_position(lm_weda, axis = 'x')) 

grid.arrange(lm_ab, lm_weda, ncol=1)


### monthly
lmFeb = ggplot(dfa, aes(x=Feb, y=z)) +
  geom_point(alpha=.5) +  
  geom_smooth(method=lm) +
  ylab("depth (m)") + 
  ylim(0, -75) +
  xlim(0, 2100) +
  theme_gray()
lmFeb = ggdraw(switch_axis_position(lmFeb, axis = 'x'))

lmMar = ggplot(dfa, aes(x=Mar, y=z)) +
  geom_point(alpha=.5) +  
  geom_smooth(method=lm) +
  ylab("depth (m)")   + 
  ylim(0, -75) +
  xlim(0, 2100) +
  theme_gray()
lmMar = ggdraw(switch_axis_position(lmMar, axis = 'x'))

lmApr = ggplot(dfa, aes(x=Apr, y=z)) +
  geom_point(alpha=.5) +  
  geom_smooth(method=lm) +
  ylab("depth (m)")  + 
  ylim(0, -75) +
  xlim(0, 2100) +
  theme_gray() 
lmApr = ggdraw(switch_axis_position(lmApr, axis = 'x'))

lmMay = ggplot(dfa, aes(x=May, y=z)) +
  geom_point(alpha=.5) +  
  geom_smooth(method=lm)  +
  ylab("depth (m)")  + 
  ylim(0, -75) +
  xlim(0, 2100) +
  theme_gray()
lmMay = ggdraw(switch_axis_position(lmMay, axis = 'x'))

lmJun = ggplot(dfa, aes(x=Jun, y=z)) +
  geom_point(alpha=.5) +  
  geom_smooth(method=lm) +
  ylab("depth (m)")  + 
  ylim(0, -75) +
  xlim(0, 2100) +
  theme_gray() 
lmJun = ggdraw(switch_axis_position(lmJun, axis = 'x'))

lmJul = ggplot(dfa, aes(x=Jul, y=z)) +
  geom_point(alpha=.5) +  
  geom_smooth(method=lm) +
  ylab("depth (m)")  + 
  ylim(0, -75) +
  xlim(0, 2100) +
  theme_gray() 
lmJul = ggdraw(switch_axis_position(lmJul, axis = 'x'))

lmAug = ggplot(dfa, aes(x=Aug, y=z)) +
  geom_point(alpha=.5) +  
  geom_smooth(method=lm)  +
  ylab("depth (m)") + 
  ylim(0, -75) +
  xlim(0, 2100) +
  theme_gray()
lmAug = ggdraw(switch_axis_position(lmAug, axis = 'x'))

grid.arrange(lmFeb, lmMar, lmApr, 
             lmMay, lmJun, lmJul, 
             lmAug, ncol=3)

## regressions weda
lmweda = ggplot(dfw, aes(x=tss, y=z)) +
  geom_point(shape=1) +
  geom_smooth(method=lm)  +
  ylab("depth (m)") +
  theme_gray()
lmweda = ggdraw(switch_axis_position(lmweda, axis = 'x'))
lmweda





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
