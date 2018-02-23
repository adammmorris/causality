
# Preliminaries -----------------------------------------------------------

require(ggplot2)
require(dplyr)
require(lme4)
require(lmerTest)
require(lattice)
require(psych)
se <- function(x) {sd(x) / sqrt(length(x))};
dodge <- position_dodge(width=0.9)

# Import data -----------------------------------------------------------

setwd('/Users/adam/Me/Psychology/Projects/causality/git/data')
path = ''
data = read.csv(paste0(path, 'data.csv')) %>% arrange(subject) %>%
  mutate(focal_fac = factor(ifelse(focal_high == 10, 'high', ifelse(focal_high > 5, 'high', 'low')), c('low', 'high', 'one')),
         alt_fac = factor(ifelse(alt_high == 10, 'high', ifelse(alt_high > 5, 'high', 'low')), c('low', 'high', 'one')),
         rt_fac = factor(rt > median(rt), c(T,F), c('Slow', 'Fast')))
df.graph = data %>% group_by(focal_high, alt_high) %>% summarise(rating.mean = (mean(rating) + 1) / 9, rating.se = se(rating))

#trellis.par.set("axis.line",list(col=NA,lty=1,lwd=1))
wireframe(rating.mean ~ alt_high * focal_high, data = df.graph, colorkey = TRUE, drape = TRUE,  screen=list(z=-60, x=-60, y=0),
          #col.regions = colorRampPalette(c("#ff0000", "#ffff00"))(100),
          xlab = list("Prob. of blue \n(alternative)", cex = 1.3), ylab = list("Prob. of green\n(focal)", cex = 1.3),
          zlab = list("How causal\nwas green?", cex = 1.3))

model = lmer(rating ~ focal_high + alt_high + (1 + focal_high + alt_high | subject), data = data)
summary(model)

df.graph2 = data %>% filter(rt_fac == 'Slow') %>% group_by(focal_fac, alt_fac) %>% summarise(rating.mean = mean(rating), rating.se = se(rating))

ggplot(data = df.graph2, aes(x = focal_fac, y = rating.mean, colour = alt_fac, group = alt_fac)) +
  geom_line() +
  geom_errorbar(aes(ymax = rating.mean + rating.se, ymin = rating.mean - rating.se), width = .2) 

f = function(x,a) {a*(1-x)/(1-x*a)}
f_exp = function(x,a) {ifelse(px == 10 & pa == 10, 1/2, exp(f(px/10,pa/10)) / sum(exp(f(px/10,pa/10)) + exp(f(pa/10,px/10))))}
thomas = function(x,a) {1-x*(1-a)}
hh = function(x,a) {ifelse(x > .5, 0, ifelse(x < a, 1, 1/2))}
sp = function(x,a) {a*(1-x)}
dp = function(x,a) {a}

df.test = data.frame(rating = NULL, alt_high = NULL, focal_high = NULL)

actual = c()
predicted_us = c()
predicted_t = c()
predicted_sp = c()
predicted_dp = c()
for (px in 1:10) {
  for (pa in 1:10) {
        actual = c(actual, df.graph$rating.mean[df.graph$focal_high == px & df.graph$alt_high == pa])
        predicted_us = c(predicted_us, f(px/10,pa/10))
        predicted_t = c(predicted_t, thomas(px/10,pa/10))
        predicted_sp = c(predicted_sp, sp(px/10,pa/10))
        predicted_dp = c(predicted_dp, dp(px/10,pa/10))
        #df.test = rbind(df.test, data.frame(rating = f_exp(px/10,pa/10), alt_high = pa, focal_high = px))
  }
}

cor.test(actual, predicted_us)
cor.test(predicted_us, predicted_t)
r.test(n=99, r12=.8671829, r23 = .7379976, r13 = .7884932)

wireframe(rating ~ alt_high * focal_high, data = df.test, colorkey = TRUE, drape = TRUE,  screen=list(z=130, x=-60, y=0),
          col.regions = colorRampPalette(c("red", "yellow"))(100), light.source = c(10,0,10)) 

