library(readxl)
library(tidyverse)
source('http://psych.colorado.edu/~jclab/R/mcSummaryLm.R')
library(car)
library(ggpubr)
library(rstatix)
source("summarySE.R")
library(ggsignif)

common.artists <- read_xlsx('association/common/data/BIGC WA_COMMON_visual artists.xlsx')
common.scientists <- read_xlsx('association/common/data/BIGC WA_COMMON_scientists.xlsx')
common.control <- read_xlsx('association/common/data/BIGC WA_COMMON_comparison.xlsx')

free.artists <- read_xlsx('association/free response/data/BIGC WA_FREE RESPONSE_visual artists.xlsx')
free.scientists <- read_xlsx('association/free response/data/BIGC WA_FREE RESPONSE_scientists.xlsx')
free.control <- read_xlsx('association/free response/data/BIGC WA_FREE RESPONSE_comparisons.xlsx')

uncommon.artists <- read_xlsx('association/individual/BIGC WA_INDIVIDUAL_visual artists.xlsx')
uncommon.scientists <- read_xlsx('association/individual/BIGC WA_INDIVIDUAL_scientists.xlsx')
uncommon.control <- read_xlsx('association/individual/BIGC WA_INDIVIDUAL_comparisons.xlsx')

common.artists <- common.artists %>% gather(item, response, FORK:RAVEN) %>%
  rename(id = PTID) 
common.scientists <- common.scientists %>% gather(item, response, FORK:RAVEN) %>%
  rename(id = PTID)
common.control <- common.control %>% gather(item, response, FORK:RAVEN) %>%
  rename(id = PTID)

uncommon.artists <- uncommon.artists %>% gather(item, response, FORK:RAVEN) %>%
  rename(id = PTID)
uncommon.scientists <- uncommon.scientists %>% gather(item, response, FORK:RAVEN) %>%
  rename(id = PTID)
uncommon.control <- uncommon.control %>% gather(item, response, FORK:RAVEN) %>%
  rename(id = PTID)

free.artists <- free.artists %>% gather(item, response, FORK:RAVEN) %>%
  rename(id = PTID)
free.scientists <- free.scientists %>% gather(item, response, FORK:RAVEN) %>%
  rename(id = PTID)
free.control <- free.control %>% gather(item, response, FORK:RAVEN) %>%
  rename(id = PTID)

common.artists <- common.artists %>% mutate(group = 'artists', condition = 'common')
common.scientists <- common.scientists %>% mutate(group = 'scientists', condition = 'common')
common.control <- common.control %>% mutate(group = 'control', condition = 'common')

uncommon.artists <- uncommon.artists %>% mutate(group = 'artists', condition = 'uncommon')
uncommon.scientists <- uncommon.scientists %>% mutate(group = 'scientists', condition = 'uncommon')
uncommon.control <- uncommon.control %>% mutate(group = 'control', condition = 'uncommon')

free.artists <- free.artists %>% mutate(group = 'artists', condition = 'free')
free.scientists <- free.scientists %>% mutate(group = 'scientists', condition = 'free')
free.control <- free.control %>% mutate(group = 'control', condition = 'free')

#merge
bigC.WA <- Reduce(function(x, y) merge(x, y, all = TRUE), list(common.artists, common.control, common.scientists, 
     free.artists, free.control, free.scientists, 
     uncommon.artists, uncommon.control, uncommon.scientists))

#save
write.csv(bigC.WA, file = "association/bigC_WA_ALL.csv")

WA.SemDis <- read.csv("WA_SemDis.csv")
WA.SemDis.lm <- lm(SemDis_MEAN ~ group + condition + group*condition, WA.SemDis)
mcSummary(WA.SemDis.lm)

WA.SemDis.complete <- WA.SemDis[complete.cases(WA.SemDis), ]

means <- WA.SemDis.complete %>%
  group_by(group, condition) %>%
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

#mixed effects model
library(nlme)

#robust aov
library(WRS2)
WA.SemDis.complete <- WA.SemDis.complete %>% group_by(group, condition)
WA.SemDis.complete$group <- factor(WA.SemDis.complete$group)
WA.SemDis.complete$condition <- factor(WA.SemDis.complete$condition)
WA_robust <- t2way(SemDis_MEAN ~ group * condition, WA.SemDis.complete, tr = 0.2)
postBigC <- mcp2atm(SemDis_MEAN ~ group * condition, data = WA.SemDis.complete)

#effect sizes and adjust p
t1way(SemDis_MEAN ~ group, WA.SemDis.complete, tr = 0.2)
p.adjust(WA_robust$A.p.value, "bonferroni", n = length(WA_robust$A.p.value))
t1way(SemDis_MEAN ~ condition, WA.SemDis.complete, tr = 0.2)
p.adjust(WA_robust$B.p.value, "bonferroni", n = length(WA_robust$B.p.value))

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
library(wesanderson)

cond_dist <- ggplot(WA.SemDis.complete, aes(x = condition, y = SemDis_MEAN)) +
  geom_boxplot(aes(fill = condition)) +
  scale_fill_manual(values = wes_palette("FantasticFox1")) +
  theme_bw() + xlab("Condition") + ylab("Mean SemDis Score") + theme(legend.position = "none") + coord_cartesian(ylim = c(0, 1.25))
  
group_dist <- ggplot(WA.SemDis.complete, aes(x = group, y = SemDis_MEAN)) +
  geom_boxplot(aes(fill = group)) +
  scale_fill_manual(values = wes_palette("GrandBudapest2")) +
  theme_bw() + xlab("Group") + ylab("Mean SemDis Score") + theme(legend.position = "none") + coord_cartesian(ylim = c(0, 1.25))

figure1ab <- ggarrange(cond_dist, group_dist, 
                     labels = c("A", "B"),
                     ncol = 1, nrow = 2)

#dists  
ggplot(WA.SemDis.complete, aes(x = SemDis_MEAN)) +
  geom_histogram(aes(color = group, fill = group)) +
  facet_wrap(~condition)

