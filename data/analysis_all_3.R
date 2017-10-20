require(ggplot2)
require(dplyr)
require(lme4)
require(lmerTest)
require(lattice)
se <- function(x) {sd(x) / sqrt(length(x))};
dodge <- position_dodge(width=0.9)

setwd('/Users/adam/Me/Psychology/Projects/causality/git/data')
path = 'thomas_all_3/v1/'
split = function(x) {ifelse(x > 5, 'high', 'low')}
splitmid = function(x) {ifelse(x == 5, 'middle', ifelse(x > 5, 'high', 'low'))}

data = read.csv(paste0(path, 'data.csv')) %>% arrange(subject) %>%
  mutate(focal_fac = factor(split(focal_high), c('low', 'middle', 'high')),
         alton_high = ifelse(greenSecond, alt_high, alt2_high), altoff_high = ifelse(greenSecond, alt2_high, alt_high),
         alton_fac = factor(splitmid(alton_high), c('low', 'middle', 'high')),
         altoff_fac = factor(splitmid(altoff_high), c('low', 'middle', 'high')))

df.graph2 = data %>% group_by(focal_fac, alton_fac, altoff_fac) %>% summarise(rating.mean = mean(rating), rating.se = se(rating))

ggplot(data = df.graph2, aes(x = focal_fac, y = rating.mean, colour = alton_fac, group = alton_fac)) +
  geom_line() +
  geom_errorbar(aes(ymax = rating.mean + rating.se, ymin = rating.mean - rating.se), width = .1) +
  facet_wrap(~ altoff_fac)

# 3d
df.graph = data %>% group_by(focal_high, alton_high, altoff_high) %>% summarise(rating.mean = mean(rating), rating.se = se(rating))

wireframe(rating.mean ~ alton_high * focal_high | altoff_fac, data = df.graph, colorkey = TRUE, drape = TRUE,  screen=list(z=130, x=-60, y=0)) 

model = lmer(rating ~ focal_high * alt_high + (1 + focal_high + alt_high | subject), data = data)
summary(model)

# fit

smax = function(x,y) {exp(x) / (exp(x) + exp(y))}
f = function(x,a,b) {((1 - x)/x)*((x*(a + b) + a*b*(1 - 2*x))/(1 - x*(a + b) - a*b*(1 - 2*x)))}
thomas = function(x,a,b) {(1-x) + x*(a+b-a*b)}
sp = function(x,a,b) {(1-x)*(a+b-2*a*b)}
pe = function(x,a,b) {((1 - x)* (1 - a) * b + (1 - x) *(1 - b) * a)/((1 - x) *(1 - a) * b + (1 - x) *(1 - b) * a + (1 - x) *(1 - a) *(1 - b) + x * (1 - a) *(1 - b))}

actual = c()
predicted = c()
vals = c(1,3,5,7,9)
for (px in vals) {
  for (pa in vals) {
      for (pb in vals) {
        actual = c(actual, df.graph$rating.mean[df.graph$focal_high == px & df.graph$alton_high == pa & df.graph$altoff_high == pb])
        predicted = c(predicted, smax(f(px/10, pa/10, pb/10), f(pa/10, px/10, pb/10)))
        #predicted = c(predicted, thomas(px/10, pa/10, pb/10))
      }
  }
}

cor.test(actual, predicted)