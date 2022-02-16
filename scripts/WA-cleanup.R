library(readxl)
library(tidyverse)
source('http://psych.colorado.edu/~jclab/R/mcSummaryLm.R')
library(car)
library(ggpubr)
library(rstatix)
source("summarySE.R")
library(ggsignif)
library(nlme)
library(wesanderson)
library(dplyr)
library(WRS2)

common <- read.csv('association/FROM-KENDRA/BIGC-COMMON-FROMKENDRA.csv')
free <- read.csv('association/FROM-KENDRA/BIGC-FREE-FROMKENDRA.csv')
uncommon <- read.csv('association/FROM-KENDRA/BIGC-UNCOMMON-FROMKENDRA.csv')

common <- common %>% mutate(condition = 'common') %>% gather(item, response, FORK:RAVEN)
free <- free %>% mutate(condition = 'free') %>% gather(item, response, FORK:RAVEN)
uncommon <- uncommon %>% mutate(condition = 'uncommon') %>% gather(item, response, FORK:RAVEN)

#merge
bigC.WA <- Reduce(function(x, y) merge(x, y, all = TRUE), list(common, uncommon, free))

#save
write.csv(bigC.WA, file = "association/bigC_WA_ALL_FROMKENDRA.csv")

WA.SemDis <- read.csv("WA_SemDis_FROMKENDRA.csv")
WA.SemDis.lm <- lm(SemDis_MEAN ~ group + condition + group*condition, WA.SemDis)
mcSummary(WA.SemDis.lm)

WA.SemDis.complete <- WA.SemDis[complete.cases(WA.SemDis), ]

means <- WA.SemDis.complete %>%
  group_by(group, condition) %>%
  summarize(count = n(),
    mean = mean(SemDis_MEAN),
            se = sqrt(var(SemDis_MEAN)/length(SemDis_MEAN)))

meansCond <- WA.SemDis.complete %>%
  group_by(condition) %>%
  summarize(count = n(),
            mean = mean(SemDis_MEAN),
            se = sqrt(var(SemDis_MEAN)/length(SemDis_MEAN)))

meansGroup <- WA.SemDis.complete %>%
  group_by(group) %>%
  summarize(count = n(),
            mean = mean(SemDis_MEAN),
            se = sqrt(var(SemDis_MEAN)/length(SemDis_MEAN)))

#ope we failed levene's test
leveneTest(SemDis_MEAN ~ group*condition, data = WA.SemDis.complete)

#two way anova
WA.SemDis.aov <- aov(SemDis_MEAN ~ group + condition + group*condition, data = WA.SemDis.complete)
WA.Anova <- Anova(WA.SemDis.aov, type = "III")
WA.Tukey <- TukeyHSD(WA.SemDis.aov, which = 'group:condition')
compareMeans <- compare_means(SemDis_MEAN ~ condition, data = WA.SemDis.complete, group.by = "group")

#normality? YES
resid <- WA.SemDis.aov$residuals

#ROBUST#
WA.SemDis.complete <- WA.SemDis.complete %>% group_by(group, condition)
WA.SemDis.complete$group <- factor(WA.SemDis.complete$group)
WA.SemDis.complete$condition <- factor(WA.SemDis.complete$condition)
WA_robust <- t2way(SemDis_MEAN ~ group * condition, WA.SemDis.complete, tr = 0.2)
postBigC <- mcp2atm(SemDis_MEAN ~ group * condition, data = WA.SemDis.complete)

#effect sizes and adjust p
t1way(SemDis_MEAN ~ group, WA.SemDis.complete, tr = 0.2)
lincon(SemDis_MEAN ~ group, WA.SemDis.complete, tr = 0.2)
p.adjust(WA_robust$A.p.value, "bonferroni", n = length(WA_robust$A.p.value))

t1way(SemDis_MEAN ~ condition, WA.SemDis.complete, tr = 0.2)
lincon(SemDis_MEAN ~ condition, WA.SemDis.complete, tr = 0.2)
p.adjust(WA_robust$B.p.value, "bonferroni", n = length(WA_robust$B.p.value))

#post hoc intx effects
postCommon <- subset(WA.SemDis.complete, condition == 'common')
t1way(SemDis_MEAN ~ group, postCommon, tr = 0.2)
lincon(SemDis_MEAN ~ group, postCommon, tr = 0.2)

postUncommon <- subset(WA.SemDis.complete, condition == 'uncommon')
t1way(SemDis_MEAN ~ group, postUncommon, tr = 0.2)
lincon(SemDis_MEAN ~ group, postUncommon, tr = 0.2)

postFree <- subset(WA.SemDis.complete, condition == 'free')
t1way(SemDis_MEAN ~ group, postFree, tr = 0.2)
lincon(SemDis_MEAN ~ group, postFree, tr = 0.2)

#Figure 2
WAsum <- summarySE(WA.SemDis.complete, measurevar = "SemDis_MEAN", groupvars = c("group", "condition"))
WAsum$condition <- factor(WAsum$condition)
figure2 <- ggplot(WAsum, aes(x = condition, y = SemDis_MEAN, fill = group)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin = SemDis_MEAN - se, ymax = SemDis_MEAN + se), width = .2, position = position_dodge(.9)) +
  coord_cartesian(ylim = c(.6, .9)) +
  theme_bw() + xlab("Condition") + ylab("Mean SemDis Score") + labs(fill = "Group") +
  theme(text = element_text(size = 20))

#Figure 1 a and b

oldvals <- c("artists", "control", "scientists")
newvals <- factor(c("VIS", "SCG", "SCI"))
WA.SemDis.complete$group <- newvals[ match(WA.SemDis.complete$group, oldvals)]

cond_dist <- ggplot(WA.SemDis.complete, aes(x = condition, y = SemDis_MEAN)) +
  geom_violin(aes(fill = condition)) +
  geom_boxplot(width = .1, fill = "white", outlier.color = NA) +
  stat_summary(fun.y = median, geom = "point", fill = "black", shape = 21, size = 2.5) +
  scale_fill_manual(values = wes_palette("FantasticFox1")) +
  theme_bw() + xlab("Condition") + ylab("Mean SemDis Score") + 
  theme(text = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        legend.position = "none") + 
  coord_cartesian(ylim = c(0, 1.1))

group_dist <- ggplot(WA.SemDis.complete, aes(x = group, y = SemDis_MEAN)) +
  geom_violin(aes(fill = group)) +
  geom_boxplot(width = .1, fill = "white", outlier.color = NA) +
  stat_summary(fun.y = median, geom = "point", fill = "black", shape = 21, size = 2.5) +
  scale_fill_manual(values = wes_palette("GrandBudapest2")) +
  theme_bw() + xlab("Group") + ylab("Mean SemDis Score") + 
  theme(text = element_text(size = 20),
        axis.text.x = element_text(size = 18), 
        legend.position = "none") + 
  coord_cartesian(ylim = c(0, 1.1))

figure1ab <- ggarrange(cond_dist, group_dist, 
                     labels = c("A", "B"),
                     ncol = 1, nrow = 2)

#dists  
ggplot(WA.SemDis.complete, aes(x = SemDis_MEAN)) +
  geom_histogram(aes(color = group, fill = group)) +
  facet_wrap(~condition)

#level counts
nlevels(as.factor(WA.SemDis.complete$id))
