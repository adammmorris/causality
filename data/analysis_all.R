require(ggplot2)
require(dplyr)
require(lme4)
require(lmerTest)
require(lattice)
se <- function(x) {sd(x) / sqrt(length(x))};
dodge <- position_dodge(width=0.9)

setwd('/Users/adam/Me/Psychology/Projects/causality/git/data')
path = 'thomas_all_dis/v1/'
data = read.csv(paste0(path, 'data.csv')) %>% arrange(subject) %>%
  mutate(focal_fac = factor(ifelse(focal_high == 10, 'high', ifelse(focal_high > 5, 'high', 'low')), c('low', 'high', 'one')),
         alt_fac = factor(ifelse(alt_high == 10, 'high', ifelse(alt_high > 5, 'high', 'low')), c('low', 'high', 'one')))
df.graph = data %>% group_by(focal_high, alt_high) %>% summarise(rating.mean = mean(rating), rating.se = se(rating))

trellis.par.set("axis.line",list(col=NA,lty=1,lwd=1))
wireframe(rating.mean ~ alt_high * focal_high, data = df.graph, colorkey = TRUE, drape = TRUE,  screen=list(z=130, x=-60, y=0),
          col.regions = colorRampPalette(c("#ff0000", "#ffff00"))(100),
          xlab = list("Prob. of blue \n(alternative)", cex = 1.3), ylab = list("Prob. of green\n(focal)", cex = 1.3),
          zlab = list("How causal\nwas green?", cex = 1.3))

model = lmer(rating ~ focal_high * alt_high + (1 + focal_high + alt_high | subject), data = data)
summary(model)

df.graph2 = data %>% group_by(focal_fac, alt_fac) %>% summarise(rating.mean = mean(rating), rating.se = se(rating))

ggplot(data = df.graph2, aes(x = focal_fac, y = rating.mean, colour = alt_fac, group = alt_fac)) +
  geom_line() +
  geom_errorbar(aes(ymax = rating.mean + rating.se, ymin = rating.mean - rating.se), width = .2) 

f = function(x,a) {a*(1-x)/(1-x*a)}
f_exp = function(x,a) {ifelse(px == 10 & pa == 10, 1/2, exp(f(px/10,pa/10)) / sum(exp(f(px/10,pa/10)) + exp(f(pa/10,px/10))))}
thomas = function(x,a) {1-x*(1-a)}
deltap = function(x,a) {a*(1-x)}
ppc = function(x,a) {1+a/(x*(1-a))}

df.test = data.frame(rating = NULL, alt_high = NULL, focal_high = NULL)

actual = c()
predicted = c()
for (px in 1:10) {
  for (pa in 1:9) {
        actual = c(actual, df.graph$rating.mean[df.graph$focal_high == px & df.graph$alt_high == pa])
        #predicted = c(predicted, f_exp(px/10,pa/10))
        predicted = c(predicted, ppc(px/10,pa/10))
        #df.test = rbind(df.test, data.frame(rating = f_exp(px/10,pa/10), alt_high = pa, focal_high = px))
  }
}

cor.test(actual, predicted)

wireframe(rating ~ alt_high * focal_high, data = df.test, colorkey = TRUE, drape = TRUE,  screen=list(z=130, x=-60, y=0),
          col.regions = colorRampPalette(c("red", "yellow"))(100), light.source = c(10,0,10)) 

