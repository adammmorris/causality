require(ggplot2)
require(dplyr)
require(lme4)
require(lmerTest)
require(lattice)
se <- function(x) {sd(x) / sqrt(length(x))};
dodge <- position_dodge(width=0.9)

setwd('/Users/adam/Me/Psychology/Projects/causality/git/data')
path = 'thomas_fire/v4/'
data = read.csv(paste0(path, 'data.csv')) %>% arrange(subject) %>%
  mutate(focal_fac = factor(focal_high, c(0,1), c('Key rare', 'Key common')),
         alt_fac = factor(alt_high, c(0,1), c('Lock rare', 'Lock common')),
         alt2_fac = factor(alt2_high, c(0,1), c('Friend rarely remembers to bring food', 'Friend usually remembers to bring food')))

demo = read.csv(paste0(path, 'demo.csv')) %>% arrange(subject) %>% mutate(total_time_actual = total_time / 60000)
naive = (demo %>% filter(experienced != 'Yes'))$subject
ccpass = (demo %>% filter(cc1 == 'Yes' & cc2 == 'No'))$subject

for (i in 1:nrow(demo)) {
  subj = demo$subject[i]
  j = which(as.character(data$subject) == as.character(subj))
  demo$cc3pass[i] = ifelse(data$alt2_high[j], ifelse(demo$cc3[i] == 'Yes', T, F), ifelse(demo$cc3[i] == 'Yes', F, T))
}
cc3pass = (demo %>% filter(cc3pass))$subject

training = read.csv(paste0(path, 'training.csv')) %>% arrange(subject) %>% group_by(subject) %>%
  summarize(pctCorrect = mean(correct))
trainingpass = (training %>% filter(pctCorrect >= .8))$subject

df.graph = data %>% filter(subject %in% ccpass & subject %in% cc3pass & subject %in% trainingpass) %>% 
  group_by(focal_fac, alt_fac, alt2_fac) %>% summarise(rating.mean = mean(rating), rating.se = se(rating))

ggplot(data = df.graph, aes(x = focal_fac, y = rating.mean, colour = alt_fac, group = alt_fac)) +
  geom_line() +
  geom_errorbar(aes(ymax = rating.mean + rating.se, ymin = rating.mean - rating.se), width = .1) +
  facet_wrap(~ alt2_fac)


actual = c()
predicted = c()
for (px in 1:10) {
  for (pa in 1:10) {
      if (!(px == 10 & pa == 10)) {
        actual = c(actual, df.graph$rating.mean[df.graph$focal_high == px & df.graph$alt_high == pa])
        predicted = c(predicted, ((1-px/10) * pa/10) / ((1-px/10) * pa/10 + 1 - pa/10))
      }
  }
}

cor.test(actual, predicted)