#Clear console, load packages, set working directory, and load data
rm(list = ls())
library(tidyverse)
library(readxl)
library(effsize)
library(gridExtra)
library(psy)
library(scales)
library(Rmisc)
library(knitr)
library(kableExtra)
library(broom)
library(SDMTools)
library(xtable)
library(lm.beta)
library(psych)
library(PropCIs)
setwd("/Users/geoff/Google Drive/Neuromyths OSF")
neuromyths <- read.csv("/Users/geoff/Google Drive/Neuromyths OSF/Neuromyths Data.csv")

#1. Overall results

##1.1. Agreement with at least one neuromyth and agreement with every neuromyth, by study
neuromyths.SUSA <- filter(neuromyths, neuromyths$method == "SUSA")
wt.mean((neuromyths.SUSA$totalneuroyn != 0), neuromyths.SUSA$dichotomized750weights)
wt.mean((neuromyths.SUSA$totalneuroyn == 9), neuromyths.SUSA$dichotomized750weights)

neuromyths.MTurk <- filter(neuromyths, neuromyths$method == "MTurk")
wt.mean((neuromyths.MTurk$totalneuroyn != 0), neuromyths.MTurk$dichotomized750weights)
wt.mean((neuromyths.MTurk$totalneuroyn == 9), neuromyths.MTurk$dichotomized750weights)

##1.2. Agreement mean and SD, by study
wt.mean(neuromyths.SUSA$totalneuroyn, neuromyths.SUSA$dichotomized750weights)
wt.sd(neuromyths.SUSA$totalneuroyn, neuromyths.SUSA$dichotomized750weights)

wt.mean(neuromyths.MTurk$totalneuroyn, neuromyths.MTurk$dichotomized750weights)
wt.sd(neuromyths.MTurk$totalneuroyn, neuromyths.MTurk$dichotomized750weights)

##1.3. Figure 1
neuromyths$method <- relevel(neuromyths$method, "SUSA")
ggplot(neuromyths, aes(x = totalneuroyn, y = (..prop..)*100, weight = dichotomized750weights, fill=factor(method))) +
  geom_bar(width = .6, stat="count", position="dodge") +
  labs(fill="Survey Method") +
  scale_y_continuous(limits=c(0, 23), expand = c(0,0), breaks = seq(0, 20, 5)) +
  scale_x_discrete(limits=c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)) +
  ylab('% endorsing that many neuromyths') +
  xlab('Number of neuromyths endorsed') +
  theme(text = element_text(size=10, color = "black")) +
  theme(axis.text = element_text(size=10)) +
  theme(legend.text = element_text(size=10)) +
  theme(legend.position = c(0.85, 0.8))

#2. Item-level results

##2.1. Overall agreement with each myth, by study (for reporting ranges)
wt.mean(neuromyths.SUSA$practiceyn, neuromyths.SUSA$dichotomized750weights)
wt.mean(neuromyths.SUSA$practiceyn, neuromyths.SUSA$dichotomized750weights)
wt.mean(neuromyths.SUSA$phoneyn, neuromyths.SUSA$dichotomized750weights)
wt.mean(neuromyths.SUSA$subliminalyn, neuromyths.SUSA$dichotomized750weights)
wt.mean(neuromyths.SUSA$instinctsyn, neuromyths.SUSA$dichotomized750weights)
wt.mean(neuromyths.SUSA$percentyn, neuromyths.SUSA$dichotomized750weights)
wt.mean(neuromyths.SUSA$staringyn, neuromyths.SUSA$dichotomized750weights)
wt.mean(neuromyths.SUSA$hemiyn, neuromyths.SUSA$dichotomized750weights)
wt.mean(neuromyths.SUSA$mozartyn, neuromyths.SUSA$dichotomized750weights)
wt.mean(neuromyths.SUSA$vaccinesyn, neuromyths.SUSA$dichotomized750weights)

wt.mean(neuromyths.MTurk$practiceyn, neuromyths.MTurk$dichotomized750weights)
wt.mean(neuromyths.MTurk$phoneyn, neuromyths.MTurk$dichotomized750weights)
wt.mean(neuromyths.MTurk$subliminalyn, neuromyths.MTurk$dichotomized750weights)
wt.mean(neuromyths.MTurk$instinctsyn, neuromyths.MTurk$dichotomized750weights)
wt.mean(neuromyths.MTurk$percentyn, neuromyths.MTurk$dichotomized750weights)
wt.mean(neuromyths.MTurk$staringyn, neuromyths.MTurk$dichotomized750weights)
wt.mean(neuromyths.MTurk$hemiyn, neuromyths.MTurk$dichotomized750weights)
wt.mean(neuromyths.MTurk$mozartyn, neuromyths.MTurk$dichotomized750weights)
wt.mean(neuromyths.MTurk$vaccinesyn, neuromyths.MTurk$dichotomized750weights)

##2.2. Mean rate of abstention, by study

###2.2.1. Create long format SUSA dataframe
neuromyths.SUSA.Long <- gather(neuromyths.SUSA, 'percentl', 'phonel','vaccinesl','practicel','staringl',
                               'mozartl','hemil','instinctsl','subliminall', key = "myth", value = "agreement")
###2.2.2. Create SUSA abstention variable
neuromyths.SUSA.Long <- neuromyths.SUSA.Long %>%
  mutate(dunno = ifelse(agreement == 5, 1, 0))
###2.2.3. Weighted SUSA rate of abstention
wt.mean(neuromyths.SUSA.Long$dunno, neuromyths.SUSA.Long$dichotomized750weights)

###2.2.4. Create long format MTurk dataframe
neuromyths.MTurk.Long <- gather(neuromyths.MTurk, 'percentl', 'phonel','vaccinesl','practicel','staringl',
                                'mozartl','hemil','instinctsl','subliminall', key = "myth", value = "agreement")
###2.2.5. Create MTurk abstention variable
neuromyths.MTurk.Long <- neuromyths.MTurk.Long %>%
  mutate(dunno = ifelse(agreement == 5, 1, 0))
###2.2.6. Weighted MTurk rate of abstention
wt.mean(neuromyths.MTurk.Long$dunno, neuromyths.MTurk.Long$dichotomized750weights)

##2.3. Figure 2

###2.3.1.Create columns for weighted responses, by item and overall
neuromyths <- mutate(neuromyths, percentW = dichotomized750weights * percentyn,
                     phoneW = dichotomized750weights *  phoneyn, vaccinesW = dichotomized750weights *	vaccinesyn,	
                     practiceW =  dichotomized750weights *practiceyn, staringW =  dichotomized750weights *	staringyn, 
                     mozartW =  dichotomized750weights *	mozartyn, hemiW =  dichotomized750weights *	hemiyn,
                     instinctsW =  dichotomized750weights * instinctsyn, subliminalW =  dichotomized750weights *	subliminalyn,	
                     totalneuroW =  dichotomized750weights * totalneuroyn)

###2.3.2 Create long format dataframe for easier figure-makingneuromyths <- mutate(neuromyths, percentW = dichotomized750weights * percentyn,
neuromyths.Long <- gather(neuromyths, 'percentW', 'phoneW','vaccinesW','practiceW','staringW',
                          'mozartW','hemiW','instinctsW','subliminalW', key = "myth", value = "agreement")

###2.3.3 Create aggregated dataframe, where agreement is weighted percent (i.e., sums of responses weighted to 750, divided by 750, times 100)
neuromyths.means <- aggregate(neuromyths.Long$agreement/7.5, by=list(neuromyths.Long$myth, neuromyths.Long$method), FUN=sum)

###2.3.4. Figure
neuromyths.means$Group.2 <- factor(neuromyths.means$Group.2, 
                                   levels=levels(neuromyths.means$Group.2)[order(levels(neuromyths.means$Group.2), decreasing = FALSE)])
wMeans <- ggplot(neuromyths.means, aes(x=reorder(Group.1, x), y=x, fill=factor(Group.2)))
wMeans <- wMeans + geom_bar(width = .8, stat="identity", position="dodge")
wMeans + coord_flip() +
  ylab('% Agreement') +
  xlab('') +
  theme(text = element_text(size=10, color = "black")) +
  theme(axis.text = element_text(size=10, color = "black")) +
  theme(legend.position = c(0.8, 0.15)) +
  labs(fill="Survey Method") +
  scale_y_continuous(limits=c(0, 100), breaks = c(0, 20, 40, 60, 80, 100)) +
  scale_x_discrete(breaks=c("practiceW", "phoneW", "subliminalW", "instinctsW", "percentW", "staringW", "hemiW", "mozartW", "vaccinesW"),
                   labels=c("Anyone can become an expert", "Hands-free phones are safer", "Subliminal messages sell", "Instincts yield better decisions",
                            "You only use 10% of your brain", "You can sense someone staring", "Left-brained or right-brained", 
                            "Mozart makes you smarter", "Vaccines cause autism")) +
  scale_fill_discrete(guide = guide_legend(reverse=TRUE)) +
  theme(axis.title.y = element_text(angle = 0, vjust = 1, size = 10)) +
  theme(panel.grid.minor = element_blank())

#3. Overall rates of agreement with each myth, averaged across studies
##Note: because each sample was weighted to a nominal size of 750 participants 
## with identical demographics, overall rates of agreement were calcualted as 
## the average of the rates of agreement in the two samples of nominal size 750

##3.1. Means only

wt.mean(neuromyths.SUSA$practiceyn, neuromyths.SUSA$dichotomized750weights)
wt.mean(neuromyths.MTurk$practiceyn, neuromyths.MTurk$dichotomized750weights)

wt.mean(neuromyths.SUSA$phoneyn, neuromyths.SUSA$dichotomized750weights)
wt.mean(neuromyths.MTurk$phoneyn, neuromyths.MTurk$dichotomized750weights)

wt.mean(neuromyths.SUSA$subliminalyn, neuromyths.SUSA$dichotomized750weights)
wt.mean(neuromyths.MTurk$subliminalyn, neuromyths.MTurk$dichotomized750weights)

wt.mean(neuromyths.SUSA$instinctsyn, neuromyths.SUSA$dichotomized750weights)
wt.mean(neuromyths.MTurk$instinctsyn, neuromyths.MTurk$dichotomized750weights)

wt.mean(neuromyths.SUSA$percentyn, neuromyths.SUSA$dichotomized750weights)
wt.mean(neuromyths.MTurk$percentyn, neuromyths.MTurk$dichotomized750weights)

wt.mean(neuromyths.SUSA$staringyn, neuromyths.SUSA$dichotomized750weights)
wt.mean(neuromyths.MTurk$staringyn, neuromyths.MTurk$dichotomized750weights)

wt.mean(neuromyths.SUSA$hemiyn, neuromyths.SUSA$dichotomized750weights)
wt.mean(neuromyths.MTurk$hemiyn, neuromyths.MTurk$dichotomized750weights)

wt.mean(neuromyths.SUSA$mozartyn, neuromyths.SUSA$dichotomized750weights)
wt.mean(neuromyths.MTurk$mozartyn, neuromyths.MTurk$dichotomized750weights)

wt.mean(neuromyths.SUSA$vaccinesyn, neuromyths.SUSA$dichotomized750weights)
wt.mean(neuromyths.MTurk$vaccinesyn, neuromyths.MTurk$dichotomized750weights)

##3.2. Means with CIs
CI1 <- exactci(wt.mean(neuromyths.MTurk$hemiyn, neuromyths.MTurk$dichotomized750weights)*750, 750, .95)
CI2 <- exactci(wt.mean(neuromyths.MTurk$instinctsyn, neuromyths.MTurk$dichotomized750weights)*750, 750, .95)
CI3 <- exactci(wt.mean(neuromyths.MTurk$mozartyn, neuromyths.MTurk$dichotomized750weights)*750, 750, .95)
CI4 <- exactci(wt.mean(neuromyths.MTurk$percentyn, neuromyths.MTurk$dichotomized750weights)*750, 750, .95)
CI5 <- exactci(wt.mean(neuromyths.MTurk$phoneyn, neuromyths.MTurk$dichotomized750weights)*750, 750, .95)
CI6 <- exactci(wt.mean(neuromyths.MTurk$practiceyn, neuromyths.MTurk$dichotomized750weights)*750, 750, .95)
CI7 <- exactci(wt.mean(neuromyths.MTurk$staringyn, neuromyths.MTurk$dichotomized750weights)*750, 750, .95)
CI8 <- exactci(wt.mean(neuromyths.MTurk$subliminalyn, neuromyths.MTurk$dichotomized750weights)*750, 750, .95)
CI9 <- exactci(wt.mean(neuromyths.MTurk$vaccinesyn, neuromyths.MTurk$dichotomized750weights)*750, 750, .95)

CI10 <- exactci(wt.mean(neuromyths.SUSA$hemiyn, neuromyths.SUSA$dichotomized750weights)*750, 750, .95)
CI11 <- exactci(wt.mean(neuromyths.SUSA$instinctsyn, neuromyths.SUSA$dichotomized750weights)*750, 750, .95)
CI12 <- exactci(wt.mean(neuromyths.SUSA$mozartyn, neuromyths.SUSA$dichotomized750weights)*750, 750, .95)
CI13 <- exactci(wt.mean(neuromyths.SUSA$percentyn, neuromyths.SUSA$dichotomized750weights)*750, 750, .95)
CI14 <- exactci(wt.mean(neuromyths.SUSA$phoneyn, neuromyths.SUSA$dichotomized750weights)*750, 750, .95)
CI15 <- exactci(wt.mean(neuromyths.SUSA$practiceyn, neuromyths.SUSA$dichotomized750weights)*750, 750, .95)
CI16 <- exactci(wt.mean(neuromyths.SUSA$staringyn, neuromyths.SUSA$dichotomized750weights)*750, 750, .95)
CI17 <- exactci(wt.mean(neuromyths.SUSA$subliminalyn, neuromyths.SUSA$dichotomized750weights)*750, 750, .95)
CI18 <- exactci(wt.mean(neuromyths.SUSA$vaccinesyn, neuromyths.SUSA$dichotomized750weights)*750, 750, .95)

neurointervals <- rbind(CI1, CI2, CI3, CI4, CI5, CI6, CI7, CI8, CI9, CI10, CI11, CI12, CI13, CI14, CI15, CI16, CI17, CI18)

meanswithCI <- cbind(neuromyths.means, neurointervals)

#4. Table 2

#Age t-test
neuroSUSA.ageRange <- t.test(totalneuroyn ~ ageRange, data = neuromyths.SUSA)
neuroMTurk.ageRange <- t.test(totalneuroyn ~ ageRange, data = neuromyths.MTurk)
neuromyths.ageRange <- map_df(list(neuroSUSA.ageRange, neuroMTurk.ageRange), tidy)

#Sex t-test
neuroSUSA.sex <- t.test(totalneuroyn ~ sex, data = neuromyths.SUSA)
neuroMTurk.sex <- t.test(totalneuroyn ~ sex, data = neuromyths.MTurk)
neuromyths.sex <- map_df(list(neuroSUSA.sex, neuroMTurk.sex), tidy)

#Race F-Test
neuroSUSA.race <- aov(totalneuroyn ~ race, data = neuromyths.SUSA)
neuroMTurk.race <- aov(totalneuroyn ~ race, data = neuromyths.MTurk)
neuromyths.race <- map_df(list(neuroSUSA.race, neuroMTurk.race), tidy)

#Region F-test
neuroSUSA.region <- aov(totalneuroyn ~ region, data = neuromyths.SUSA)
neuroMTurk.region <- aov(totalneuroyn ~ region, data = neuromyths.MTurk)
neuromyths.region <- map_df(list(neuroSUSA.region, neuroMTurk.region), tidy)

#Income F-test
neuroSUSA.income <- aov(totalneuroyn ~ income, data = neuromyths.SUSA)
neuroMTurk.income <- aov(totalneuroyn ~ income, data = neuromyths.MTurk)
neuromyths.income <- map_df(list(neuroSUSA.income, neuroMTurk.income), tidy)

#Age
summarySE(neuromyths.SUSA, measurevar="totalneuroyn", groupvars="ageRange")
summarySE(neuromyths.MTurk, measurevar="totalneuroyn", groupvars="ageRange")

#Sex
summarySE(neuromyths.SUSA, measurevar="totalneuroyn", groupvars="sex")
summarySE(neuromyths.MTurk, measurevar="totalneuroyn", groupvars="sex")

#Race
summarySE(neuromyths.SUSA, measurevar="totalneuroyn", groupvars="race")
summarySE(neuromyths.MTurk, measurevar="totalneuroyn", groupvars="race")

#Income
summarySE(neuromyths.SUSA, measurevar="totalneuroyn", groupvars="income")
summarySE(neuromyths.MTurk, measurevar="totalneuroyn", groupvars="income")

#Region
summarySE(neuromyths.SUSA, measurevar="totalneuroyn", groupvars="region")
summarySE(neuromyths.MTurk, measurevar="totalneuroyn", groupvars="region")

#Full regressions for each study
##SUSA
susa.lm.full <- lm(totalneuroyn ~ edunum + age + sexnum + incomenum + racenum + regionnum, data = neuromyths.SUSA)
summary(susa.lm.full)
susa.lm.full

susa.lm.full.beta <- lm.beta(susa.lm.full)
coef(susa.lm.full.beta)

##MTurk
mturk.lm.full <- lm(totalneuroyn ~ edunum + age + sexnum + incomenum + racenum + regionnum, data = neuromyths.MTurk)
summary(mturk.lm.full)
mturk.lm.full

mturk.lm.full.beta <- lm.beta(mturk.lm.full)
coef(mturk.lm.full.beta)

#5. Figure 3
##Regressions corresponding to Figure 3
neuromyths.split = neuromyths %>% group_by(method) %>%
  do(neuromyths.edunum.glm = glm(totalneuroyn ~ edunum, data = .))
neuromyths.glm.edunumresults = tidy(neuromyths.split, neuromyths.edunum.glm)
neuromyths.glm.edunumresults

neuromyths.split = neuromyths %>% group_by(method) %>%
  do(neuromyths.classes.glm = glm(totalneuroyn ~ classes, data = .))
neuromyths.glm.classesresults = tidy(neuromyths.split, neuromyths.classes.glm)
neuromyths.glm.classesresults

neuromyths.split = neuromyths %>% group_by(method) %>%
  do(neuromyths.books.glm = glm(totalneuroyn ~ books, data = .))
neuromyths.glm.booksresults = tidy(neuromyths.split, neuromyths.books.glm)
neuromyths.glm.booksresults

neuromyths.split = neuromyths %>% group_by(method) %>%
  do(neuromyths.multi.glm = glm(totalneuroyn ~ edunum + classes + books, data = .))
neuromyths.glm.multiresults = tidy(neuromyths.split, neuromyths.multi.glm)
neuromyths.glm.multiresults

multi.MTurk.lm <- lm(neuromyths.MTurk$totalneuroyn ~ neuromyths.MTurk$books + neuromyths.MTurk$classes + neuromyths.MTurk$books)
summary(multi.MTurk.lm)

#Figure 2
###a:EFFECT OF EDUCATION ON TOTAL AGREEMENT
#Figure 2
###a:EFFECT OF EDUCATION ON TOTAL AGREEMENT
neuromyths <- neuromyths[!is.na(neuromyths$edunum),]
neuromyths <- mutate(neuromyths, edunumrev = 5 - edunum)
neuromyths.edunumrev <- summarySE(neuromyths, measurevar="totalneuroyn", groupvars=c("method", "edunumrev"))
Figure2a <- ggplot(neuromyths.edunumrev, aes(x=edunumrev, y=totalneuroyn, color=method)) + 
  geom_line (stat="identity", size = .7) +
  geom_errorbar(aes(ymin=totalneuroyn-ci, ymax=totalneuroyn+ci), width = .05, size = .7) +
  ggtitle('A') +
  theme(title = element_text(size=10, face = "bold")) +
  ylab(NULL) +
  xlab('Education Level') +
  theme(text = element_text(size=10, color = "black")) +
  theme(axis.text.x = element_text(size=10)) +
  theme(legend.position="none") +
  scale_y_continuous(limits=c(0, 9), expand = c(0,0), breaks = seq(0, 9, 1)) +
  scale_x_continuous(expand = c(0,.3), breaks=c(1, 2, 3, 4), minor_breaks = c(1, 2, 3, 4),
                     labels=c("No \n College", "Some \n College", "College \n Grad", "Grad \n School"))

neuromyths <- neuromyths[!is.na(neuromyths$classes),]
neuromyths.classes <- summarySE(neuromyths, measurevar="totalneuroyn", groupvars=c("method", "classes"))
Figure2b <-  ggplot(neuromyths.classes, aes(x=classes, y=totalneuroyn, color=method)) + 
  geom_line (stat="identity", size = .7) +
  geom_errorbar(aes(ymin=totalneuroyn-ci, ymax=totalneuroyn+ci), width = .05, size = .7) +
  ggtitle('B') +
  theme(title = element_text(size=10, face = "bold")) +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y = element_blank()) +
  xlab('Psychology Classes Taken') +
  labs(fill="Survey Method") +
  theme(text = element_text(size=7, color = "black")) +
  theme(axis.text = element_text(size=10)) +
  theme(legend.text = element_text(size=10)) +
  theme(legend.text = element_text(size=7)) +
  labs(color="Survey \nMethod") +
  theme(legend.position = c(.375, .2)) +
  labs(color="Survey \nMethod") +
  scale_y_continuous(limits=c(0, 9), expand = c(0,0), breaks = seq(0, 9, 1)) +
  scale_x_continuous(expand = c(0,.3), breaks=c(1, 2, 3, 4), minor_breaks = c(1, 2, 3, 4),
                     labels=c("0 \n", "1 \n", "2 \n", "3+ \n"))

neuromyths <- neuromyths[!is.na(neuromyths$books),]
neuromyths.books <- summarySE(neuromyths, measurevar="totalneuroyn", groupvars=c("method", "books"))
Figure2c <- ggplot(neuromyths.books, aes(x=books, y=totalneuroyn, color=method)) + 
  geom_line (stat="identity", size = .7) +
  geom_errorbar(aes(ymin=totalneuroyn-ci, ymax=totalneuroyn+ci), width = .05, size = .7) +
  ggtitle('C') +
  theme(title = element_text(size=10, face = "bold")) +
  theme(axis.title.y = element_blank()) +
  theme(axis.text.y = element_blank()) +
  xlab('Psychology Books Read') +
  theme(text = element_text(size=10, color = "black")) +
  theme(axis.text = element_text(size=10)) +
  theme(legend.position="none") +
  scale_y_continuous(limits=c(0, 9), expand = c(0,0), breaks = seq(0, 9, 1)) +
  scale_x_continuous(expand = c(0,.3), breaks=c(1, 2, 3, 4), minor_breaks = c(1, 2, 3, 4),
                     labels=c("0 \n", "1 \n", "2 \n", "3+ \n"))

multiplot(Figure2a, Figure2b, Figure2c, cols=3) +
  theme(legend.text = element_text(size=10)) +
  theme(legend.position="bottom") +
  labs(color="Survey \nMethod")

#6. BY STUDY - Correlations of each belief with overall belief in the other 8 items

###Create the basic matrices and the comined matrix
neurocorr.SUSA <- neuromyths.SUSA[, c(30, 28, 35, 34, 27, 31, 33, 32, 29)]
SUSAcorr.matrix <- round(cor(neurocorr.SUSA),2)

neurocorr.MTurk <- neuromyths.MTurk[, c(30, 28, 35, 34, 27, 31, 33, 32, 29)]
MTurkcorr.matrix <- round(cor(neurocorr.MTurk),2)

combinedcorr.matrix <- lowerUpper(MTurkcorr.matrix, SUSAcorr.matrix)
cor(neuromyths.SUSA$practiceyn, (neuromyths.SUSA$totalneuroyn - neuromyths.SUSA$practiceyn))
cor(neuromyths.SUSA$phoneyn, (neuromyths.SUSA$totalneuroyn - neuromyths.SUSA$phoneyn))
cor(neuromyths.SUSA$subliminalyn, (neuromyths.SUSA$totalneuroyn - neuromyths.SUSA$subliminalyn))
cor(neuromyths.SUSA$instinctsyn, (neuromyths.SUSA$totalneuroyn - neuromyths.SUSA$instinctsyn))
cor(neuromyths.SUSA$percentyn, (neuromyths.SUSA$totalneuroyn - neuromyths.SUSA$percentyn))
cor(neuromyths.SUSA$staringyn, (neuromyths.SUSA$totalneuroyn - neuromyths.SUSA$staringyn))
cor(neuromyths.SUSA$hemiyn, (neuromyths.SUSA$totalneuroyn - neuromyths.SUSA$hemiyn))
cor(neuromyths.SUSA$mozartyn, (neuromyths.SUSA$totalneuroyn - neuromyths.SUSA$mozartyn))
cor(neuromyths.SUSA$vaccinesyn, (neuromyths.SUSA$totalneuroyn - neuromyths.SUSA$vaccinesyn))

cor(neuromyths.MTurk$practiceyn, (neuromyths.MTurk$totalneuroyn - neuromyths.MTurk$practiceyn))
cor(neuromyths.MTurk$phoneyn, (neuromyths.MTurk$totalneuroyn - neuromyths.MTurk$phoneyn))
cor(neuromyths.MTurk$subliminalyn, (neuromyths.MTurk$totalneuroyn - neuromyths.MTurk$subliminalyn))
cor(neuromyths.MTurk$instinctsyn, (neuromyths.MTurk$totalneuroyn - neuromyths.MTurk$instinctsyn))
cor(neuromyths.MTurk$percentyn, (neuromyths.MTurk$totalneuroyn - neuromyths.MTurk$percentyn))
cor(neuromyths.MTurk$staringyn, (neuromyths.MTurk$totalneuroyn - neuromyths.MTurk$staringyn))
cor(neuromyths.MTurk$hemiyn, (neuromyths.MTurk$totalneuroyn - neuromyths.MTurk$hemiyn))
cor(neuromyths.MTurk$mozartyn, (neuromyths.MTurk$totalneuroyn - neuromyths.MTurk$mozartyn))
cor(neuromyths.MTurk$vaccinesyn, (neuromyths.MTurk$totalneuroyn - neuromyths.MTurk$vaccinesyn))


