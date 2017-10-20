require(ggplot2)
require(dplyr)
require(lme4)
require(lmerTest)
require(lattice)

se <- function(x) {sd(x) / sqrt(length(x))};
dodge <- position_dodge(width=0.9)

setwd('/Users/adam/Me/Psychology/Projects/causality/git/data')
path = 'thomas_boxes/v2/'
data = read.csv(paste0(path, 'data.csv')) %>% arrange(subject) %>%
  mutate(focal_fac = factor(focal_high, c(0,1), c('X is rare', 'X is common')),
         alt_fac = factor(alt_high, c(0,1), c('A is rare', 'A is common')),
         alt2_fac = factor(alt2_high, c(1,0), c('B is rare', 'B is common')))

demo = read.csv(paste0(path, 'demo.csv')) %>% arrange(subject) %>% mutate(total_time_actual = total_time / 60000)
naive = (demo %>% filter(experienced != 'Yes'))$subject
ccpass = (demo %>% filter(cc1 == 'No' & cc2 == 'Yes'))$subject

df.graph = data %>% filter(subject %in% ccpass) %>% 
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