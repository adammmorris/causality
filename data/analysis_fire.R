require(ggplot2)
require(dplyr)
require(lme4)
require(lmerTest)
require(lattice)
se <- function(x) {sd(x) / sqrt(length(x))};
dodge <- position_dodge(width=0.9)

setwd('/Users/adam/Me/Psychology/Projects/causality/git/data')
path = 'thomas_fire/v6/'
data = read.csv(paste0(path, 'data.csv')) %>% arrange(subject) %>%
  mutate(focal_fac = factor(focal_high, c(0,1), c('Bad at math', 'Good at math')),
         alt_fac = factor(alt_high, c(0,1), c('Bad at English', 'Good at English')),
         alt2_fac = factor(alt2_high, c(0,1), c('Award for English', 'Award for math')))

demo = read.csv(paste0(path, 'demo.csv')) %>% arrange(subject) %>% mutate(total_time_actual = total_time / 60000)
naive = (demo %>% filter(experienced != 'Yes'))$subject
ccpass = (demo %>% filter(cc1pass & cc2pass & cc3pass & cc4pass))$subject

df.graph = data %>% filter(subject %in% ccpass) %>% 
  group_by(alt2_fac) %>%
  summarise(rating.mean = mean(rating), rating.se = se(rating),
            rating2.mean = mean(rating2), rating2.se = se(rating2),
            rating3.mean = mean(rating3), rating3.se = se(rating3))

ggplot(data = df.graph, aes(x = alt2_fac, y = rating2.mean)) +
  geom_line() +
  geom_errorbar(aes(ymax = rating2.mean + rating2.se, ymin = rating2.mean - rating2.se), width = .1)

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