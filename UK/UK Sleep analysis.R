install.packages(c("ggplot2", "ggthemes","lubridate", "tidyr","ez"))
  library(ggplot2)
  library(ggthemes)
  library(glmulti)
  library(corrgram)
  library(lubridate)
  library(dplyr)
  library(tidyr)
  library(ez)

##assumes we have already processed the data 

###load the data
setwd("C:\\Users\\cas\\OneDrive - Goldsmiths College\\Projects\\Baby Laughter\\BabySleepBrazilUK\\UK")
#setwd("C:\\Users\\pss02ca\\Dropbox\\Baby Laughter\\Pampers\\UK Sleep study")

babies <- readRDS("babies.rds")
diaries <- readRDS("diaries.rds")
babies= read_spss("babies.sav")


hist(babies$daysOld)
hist(diaries$happiness)
hist(as.numeric(na.omit(diaries$hourssleep)))
hist(as.numeric(na.omit(diaries$relativebedtime)))
hist(as.numeric(na.omit(diaries$parenthourssleep)))


## let's start with a quick look at all the correlations at the diary level
cols<-c('relativebedtime','hourssleep','feed',
        'numwakes','happiness','energy', 
        'parenthappiness','parentenergy',
        'parentsleepquality','parenttotalwakes','parentssleepefficiency',
        'parenthourssleep','parentrelativesleeptime')
lbls<-c('Bed\nTime','Baby\nSleep','Num\nFeeds',
        'Num\nWakes','Happiness','Energy', 
        'Parent\nHappiness','Parent\nEnergy',
        'Parent\nSleep Quality','Parent\nWakes','Parents\nSleep Efficiency',
        'Parent\nSleep Hours','Parent\nSleep Time')


corrgram(diaries[,cols],order=FALSE, lower.panel=panel.conf,
         upper.panel=panel.pie, text.panel=panel.txt,
         diag.panel= NULL,labels = lbls,
         main="Correlations between sleep factors")


rmsq <- function(d){sqrt(mean(d*d))}

#calculate some average values for each baby
bedvars <- aggregate(bedvariations ~ Resp_id, diaries, FUN = rmsq)
babies<-left_join(x = babies, y = bedvars, by = "Resp_id")

bedvars <- aggregate(parentbedvariations ~ Resp_id, diaries, FUN = rmsq)
babies<-left_join(x = babies, y = bedvars, by = "Resp_id")

bedvarsweek <- aggregate(bedvariationsbyweek ~ product + Resp_id, diaries, FUN = rmsq)
babies<-left_join(x = babies, y = bedvarsweek, by = "Resp_id")

sleepvars <- aggregate(sleepvariations ~ Resp_id, diaries, FUN = rmsq)
babies<-left_join(x = babies, y = sleepvars, by = "Resp_id")

sleepvars <- aggregate(parentsleepvariations ~ Resp_id, diaries, FUN = rmsq)
babies<-left_join(x = babies, y = sleepvars, by = "Resp_id")

sleepvarsweek <- aggregate(sleepvariationsbyweek ~ product + Resp_id, diaries, FUN = rmsq)
babies<-left_join(x = babies, y = sleepvarsweek, by = "Resp_id")

sleep <- aggregate(hourssleep ~ Resp_id, diaries, FUN = mean)
babies<-left_join(x = babies, y = sleep, by = "Resp_id")

bedtime <- aggregate(relativebedtime ~ Resp_id, diaries, FUN = mean)
babies<-left_join(x = babies, y = bedtime, by = "Resp_id")

en <- aggregate(energy ~ Resp_id, diaries, FUN = mean)
babies<-left_join(x = babies, y = en, by = "Resp_id")

hp <- aggregate(happiness ~ Resp_id, diaries, FUN = mean)
babies<-left_join(x = babies, y = hp, by = "Resp_id")

en <- aggregate(parentenergy ~ Resp_id, diaries, FUN = mean)
babies<-left_join(x = babies, y = en, by = "Resp_id")

hp <- aggregate(parenthappiness ~ Resp_id, diaries, FUN = mean)
babies<-left_join(x = babies, y = hp, by = "Resp_id")

sleep <- aggregate(parenthourssleep ~ Resp_id, diaries, FUN = mean)
babies<-left_join(x = babies, y = sleep, by = "Resp_id")

#sleeping arrangments
diaries$sleeparrangements <-    babies[match(diaries$Resp_id, babies$Resp_id),"preuse.Q12"]
do1 <- aggregate(daysOld ~ sleeparrangements, diaries, FUN = mean)
nw1 <- aggregate(numwakes ~ sleeparrangements, diaries, FUN = mean)
sl1 <- aggregate(hourssleep ~ sleeparrangements, diaries, FUN = mean)
hp1 <- aggregate(happiness ~ sleeparrangements, diaries, FUN = mean)
en1 <- aggregate(energy ~ sleeparrangements, diaries, FUN = mean)

##higher level correlations
cols<-c('daysOld','hourssleep','relativebedtime',
        'sleepvariations',
        'SURGENCY','NEG_AFFECT','EFFORTFUL',
        'energy','happiness',
        'parentenergy','parenthappiness','parenthourssleep','parentsleepvariations')
lbls<-c('Baby\nAge','Baby\nSleep','Bed\nTime','Sleep\nVariations',
        'Surgency','Negative\nAffect','Effortful', 
        'Energy','Happiness','Parent\nEnergy','Parent\nHappiness',
        'Parent\nHours Sleep', 'Parent\nVariations')

corrgram(babies[,cols],order=FALSE, lower.panel=panel.conf,
         upper.panel=panel.pie, text.panel=panel.txt,
         diag.panel= NULL,labels = lbls,
         main="Grand average correlations")


#break down of averages per condition
aggregate(hourssleep ~ product * week, diaries,mean)
aggregate(happiness ~ product * week, diaries,mean)
aggregate(energy ~ product * week, diaries,mean)

aggregate(hourssleep ~ week, diaries,mean)
aggregate(happiness ~ week,  diaries,mean)
aggregate(energy ~ week,     diaries,mean)

aggregate(hourssleep ~ product, diaries,mean)
aggregate(happiness ~ product,  diaries,mean)
aggregate(energy ~ product,     diaries,mean)

aggregate(sleepvariations ~ product,     diaries,mean)


##are these significant?
h1 <- diaries %>% filter(!is.na(hourssleep))  %>% group_by(Resp_id,week) %>% select(Resp_id,week,product,hourssleep)  %>% summarise(product=first(product),avsleep=mean(hourssleep))
h1 <-data.frame(h1)
ezANOVA(h1,dv=avsleep,wid=.(Resp_id),within = .(product))

h1 <- diaries %>% filter(!is.na(happiness))  %>% group_by(Resp_id,week) %>% select(Resp_id,product,happiness)  %>% summarise(product=first(product),avhappiness=mean(happiness))
h1 <-data.frame(h1)
ezANOVA(h1,dv=avhappiness,wid=.(Resp_id),within = .(product))

h1 <- diaries %>% filter(!is.na(energy))  %>% group_by(Resp_id,week) %>% select(Resp_id,product,energy)  %>% summarise(product=first(product),avenergy=mean(energy))
h1 <-data.frame(h1)
ezANOVA(h1,dv=avenergy,wid=.(Resp_id),within = .(product))


table(diaries$diary.Q10_1, diaries$product )
table(diaries$diary.Q10_2, diaries$product )
table(diaries$diary.Q10_3, diaries$product )
table(diaries$diary.Q10_4, diaries$product )


sleeplm<-lm(hourssleep ~ product + (1/Resp_id),data=diaries)
summary.lm(sleeplm)

happylm<-lm(happiness ~ product + (1/Resp_id),data=diaries)
summary.lm(happylm)

energylm<-lm(energy ~ product + (1/Resp_id),data=diaries)
summary.lm(energylm)



#in separate room?
aggregate(DaysOld ~ Q13, FUN=mean, data=babies)
aggregate(hourssleep ~ Q13, FUN=mean, data=babies)
aggregate(numwakes ~ Q13, FUN=mean, data=babies)
aggregate(happiness ~ Q13, FUN=mean, data=babies)
aggregate(energy ~ Q13, FUN=mean, data=babies)
aggregate(giggly ~ Q13, FUN=mean, data=babies)
wheresleep<-aggregate(cbind(DaysOld,hourssleep,numwakes,happiness,energy,giggly) ~ Q13, FUN=mean, data=datahead)


##################
## GRAPHS
##################
theme_set(theme_few(base_size = 18))

ggplot(diaries, aes(product,energy )) + stat_boxplot()

ggplot(diaries,aes(hourssleep,happiness)) +
  aes(color=as.factor(product)) +
  geom_smooth(method=lm,size=1.5) +
  facet_grid(.~product) + 
  xlab("Hours of sleep") + ylab("Happiness score") +
  theme(legend.position = "none")

ggplot(diaries,aes(hourssleep,energy)) +
  aes(color=as.factor(product)) +
  geom_smooth(method=lm,size=1.5) +
  facet_grid(.~product) + 
  xlab("Hours of sleep") + ylab("Energy score") +
  theme(legend.position = "none")


bed.lm <-lm(hourssleep ~ 1 + relativebedtime+ (1/Resp_id), 
            data = diaries)
summary(bed.lm)


glmulti.lm.out <-
  glmulti(hourssleep ~ daysOld  +relativebedtime + product + numwakes +  feed  + changed + (1/Resp_id),
          data = diaries,
          level = 1,               # No interaction considered
          method ="g",             # "h" = Exhaustive approach, "l" = fast branch bound
          crit = "aic",            # AIC as criteria
          confsetsize = 5,         # Keep 5 best models
          plotty = F, report = F,  # No plot or interim reports
          fitfunction = "lm")      # lm function

## Show 5 best models (Use @ instead of $ for an S4 object)
glmulti.lm.out@formulas
summary(glmulti.lm.out@objects[[1]])


glmulti.lm.out <-
  glmulti(happiness ~ daysOld  +relativebedtime + product + numwakes +  feed  + changed + (1/Resp_id),
          data = diaries,
          level = 2,               # No interaction considered
          method ="g",             # "h" = Exhaustive approach, "l" = fast branch bound
          crit = "aic",            # AIC as criteria
          confsetsize = 5,         # Keep 5 best models
          plotty = F, report = F,  # No plot or interim reports
          fitfunction = "lm")      # lm function

## Show 5 best models (Use @ instead of $ for an S4 object)
glmulti.lm.out@formulas
summary(glmulti.lm.out@objects[[1]])


glhappiness <-
  glmulti(happiness ~ daysOld  + hourssleep + product + numwakes +  feed + diaperstate + changed + (1/Resp_id),
          data = diaries,
          level = 1,               # No interaction considered
          method = "1",            # Exhaustive approach
          crit = "aic",            # AIC as criteria
          confsetsize = 5,         # Keep 5 best models
          plotty = F, report = F,  # No plot or interim reports
          fitfunction = "lm")      # lm function

## Show 5 best models (Use @ instead of $ for an S4 object)
glhappiness@formulas
summary(glhappiness@objects[[1]])

glenergy <-
  glmulti(energy ~ daysOld  + hourssleep + product + numwakes +  feed + diaperstate + changed + (1/Resp_id),
          data = diaries,
          level = 1,               # No interaction considered
          method = "1",            # Exhaustive approach
          crit = "aic",            # AIC as criteria
          confsetsize = 5,         # Keep 5 best models
          plotty = F, report = F,  # No plot or interim reports
          fitfunction = "lm")      # lm function

## Show 5 best models (Use @ instead of $ for an S4 object)
glenergy@formulas
summary(glenergy@objects[[1]])


ggplot(diaries,aes(energy~diaperstate)) +
  aes(color=as.factor(diaperstate)) +
  facet_grid(.~product) + 
  xlab("Hours of sleep") + ylab("Energy score") +
  theme(legend.position = "none")

aggregate(happiness ~ diaperstate , diaries,mean)


###########################################
## BRASIL STYLE DIAGRAMS
###########################################

g1<-ggplot(diaries,aes(hourssleep))
(g1+geom_density()
+ aes(fill=as.factor(product))
+ geom_density(alpha=.3)
+ xlim(c(4,14))
+ xlab("Hours sleep per night")
+ ggtitle("Average hours sleep")
+ facet_grid(product~.) 
+ theme(legend.position = "none",
        strip.text.y=element_text(size = 24),
        axis.title.y=element_blank(),
        axis.ticks = element_blank(), 
        axis.text.y = element_blank(),
        axis.text=element_text(size=24),
        axis.title=element_text(size=18))
)

(ggplot(diaries,aes(happiness))
+ geom_histogram(stat="count", bins )
+ ggtitle("Happiness")
+ aes(fill=as.factor(product))
+ geom_density(alpha=.3)
+ xlim(c(1,10))
+ xlab("Happiness")
+ facet_grid(product~.) 
+ theme(legend.position = "none",
        strip.text.y=element_text(size = 24),
        axis.title.y=element_blank(),
        axis.ticks = element_blank(), 
        axis.text.y = element_blank(),
        axis.text=element_text(size=24),
        axis.title=element_text(size=18)))



(ggplot(diaries,aes(energy))
+ geom_density()
+ ggtitle("Energy")
+ aes(fill=as.factor(product))
+ geom_density(alpha=.3)
+ xlim(c(1,10))
+ xlab("Energy")
+ facet_grid(product~.) 
+ theme(legend.position = "none",
        strip.text.y=element_text(size = 24),
        axis.title.y=element_blank(),
        axis.ticks = element_blank(), 
        axis.text.y = element_blank(),
        axis.text=element_text(size=24),
        axis.title=element_text(size=18))
)



ggplot(diaries,aes(relativebedtime,hourssleep)) +
  geom_jitter() +
  geom_smooth(method=lm) +
  scale_x_continuous(breaks = c(6,8,10,12), labels=c("6" = "6 PM", "8" = "8 PM","10"= "10 PM","12" = "12 AM")) +
  ylab("Hours of sleep") + xlab("Bedtime") +
  theme(legend.position = "none")
cor.test(diaries$hourssleep,diaries$relativebedtime,use = "pairwise.complete.obs")



ggplot(diaries,aes(hourssleep,parenthourssleep)) +
  geom_jitter() +
  geom_smooth(method=lm) +
  ylab("Parent hours of sleep") + xlab("Baby hours of sleep") +
  theme(legend.position = "none")
cor.test(diaries$hourssleep,diaries$parenthourssleep,use = "pairwise.complete.obs")


ggplot(diaries,aes(numwakes,parentsleepquality)) +
  geom_jitter() +
  geom_smooth(method=lm) +
  ylab("Parent Sleep Quality") + xlab("Baby wakes per night") +
  theme(legend.position = "none")
cor.test(diaries$numwakes,diaries$parentsleepquality,use = "pairwise.complete.obs")


  
h2 <- diaries %>% filter(!is.na(diary.Q9) & diary.Q9>0)  
(ggplot(diaries,aes(x=happiness))+stat_bin(binwidth = 1,origin=0)
 + facet_grid(diary.Q9~.)
 + ggtitle("Happiness vs wetness"))

(ggplot(diaries,aes(x=energy))+stat_bin(binwidth = 1,origin=0)
+ facet_grid(diary.Q9~.)
+ ggtitle("Energy vs wetness"))

theme_set(theme_few(base_size = 18))
### 1 ###
#how happy was your baby
diaries$over8<-ifelse(diaries$happiness>7,"blue","skyblue")
table(diaries$over8)
ggplot(diaries,aes(happiness, fill = over8)) +
  scale_x_discrete(limits=1:10) +
  geom_bar(binwidth=1,origin=0.5) +
  xlab("Happiness") +
  ggtitle("On a scale of 1 to 10 how happy\n was your baby this morning?") +
  annotate("text",x=9,y=140,label="64%",size =18) +
  theme(legend.position = "none",
        strip.text.y=element_text(size = 24),
        axis.title.y=element_blank(),
        axis.ticks = element_blank(), 
        axis.text.y = element_blank(),
        axis.text=element_text(size=18),
        axis.title=element_text(size=18))

#how energetic.
diaries$over82<-ifelse(diaries$energy>7,"blue","skyblue")
t(table(diaries$over82))
t(table(diaries$energy))
write.csv(table(diaries$energy),"energy.csv")
ggplot(diaries,aes(energy, fill = over82)) +
  scale_x_discrete(limits=1:10) +
  geom_bar(binwidth=1,origin=0.5) +
  xlab("Energy") +
  ggtitle("On a scale of 1 to 10 how energetic\n was your baby this morning?") +
  annotate("text",x=9,y=95,label="57%",size =18) +
  theme(legend.position = "none",
        strip.text.y=element_text(size = 24),
        axis.title.y=element_blank(),
        axis.ticks = element_blank(), 
        axis.text.y = element_blank(),
        axis.text=element_text(size=18),
        axis.title=element_text(size=18))

