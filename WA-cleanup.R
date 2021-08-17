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

WA.SemDis.aov <- aov(SemDis_MEAN ~ group + condition + group*condition, data = WA.SemDis.complete)
WA.Anova <- Anova(WA.SemDis.aov, type = "III")
WA.Tukey <- TukeyHSD(WA.SemDis.aov, which = 'group:condition')
compareMeans <- compare_means(SemDis_MEAN ~ condition, data = WA.SemDis.complete, group.by = "group")

#plot that thing
WAsum <- summarySE(WA.SemDis.complete, measurevar = "SemDis_MEAN", groupvars = c("group", "condition"))
WAsum$condition <- factor(WAsum$condition)
ggplot(WAsum, aes(x = condition, y = SemDis_MEAN, fill = group)) +
  geom_bar(position = position_dodge(), stat = "identity") +
  geom_errorbar(aes(ymin = SemDis_MEAN - se, ymax = SemDis_MEAN + se), width = .2, position = position_dodge(.9)) +
  coord_cartesian(ylim = c(.6, .9)) +
  theme_bw() + xlab("Condition") + ylab("Mean SemDis Score") + labs(fill = "Group")
  


