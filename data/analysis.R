require(ggplot2)
require(dplyr)
require(lme4)
require(lmerTest)
se <- function(x) {sd(x) / sqrt(length(x))};
dodge <- position_dodge(width=0.9)

setwd('/Users/adam/Me/Psychology/Projects/causality/git/data')
path = 'thomas_as/v2/'
data = read.csv(paste0(path, 'data.csv')) %>% arrange(subject)
df.graph = data %>% group_by(focal_high, alt_high) %>% summarise(rating.mean = mean(rating), rating.se = se(rating))
df.bs = data %>% filter(trial_ind == 0)
df.graph2 = df.bs %>% group_by(focal_high, alt_high) %>% summarise(rating.mean = mean(rating), rating.se = se(rating))

## supersession
ggplot(df.graph, aes(x = focal_high, y = rating.mean, group = alt_high, fill = alt_high)) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymax = rating.mean + rating.se, ymin = rating.mean - rating.se), width = .5, position = dodge) 

model1 = lmer(rating ~ focal_high * alt_high + (1 | subject), data = data)
summary(model1)

# first answer
ggplot(df.graph2, aes(x = focal_high, y = rating.mean, group = alt_high, fill = alt_high)) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymax = rating.mean + rating.se, ymin = rating.mean - rating.se), width = .5, position = dodge) 

model2 = lm(rating ~ focal_high * alt_high, data = df.bs)
summary(model2)

## abnormal selection
ggplot(df.graph, aes(x = alt_high, y = rating.mean, group = focal_high, fill = focal_high)) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymax = rating.mean + rating.se, ymin = rating.mean - rating.se), width = .5, position = dodge) 

model1.as = lmer(rating ~ focal_high * alt_high + (1 | subject), data = data)
summary(model1.as)

ggplot(df.graph2, aes(x = alt_high, y = rating.mean, group = focal_high, fill = focal_high)) +
  geom_bar(stat = "identity", position = dodge) +
  geom_errorbar(aes(ymax = rating.mean + rating.se, ymin = rating.mean - rating.se), width = .5, position = dodge) 
model2.as = lm(rating ~ focal_high * alt_high, data = df.bs)
summary(model2.as)