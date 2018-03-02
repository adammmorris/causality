
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
data = read.csv(paste0(path, 'data.csv')) %>% arrange(subject)

# how many subjects are there?
numsubj = length(unique(data$subject))
numobs = nrow(data)

# Make 3D graph -----------------------------------------------------------


df.graph = data %>% group_by(focal_high, alt_high) %>% summarise(rating.mean = (mean(rating) + 1) / 9, rating.se = se(rating))

#trellis.par.set("axis.line",list(col=NA,lty=1,lwd=1))
wireframe(rating.mean ~ alt_high * focal_high, data = df.graph, colorkey = TRUE, drape = TRUE,  screen=list(z=-60, x=-60, y=0),
          #col.regions = colorRampPalette(c("#ff0000", "#ffff00"))(100),
          xlab = list("Prob. of blue \n(alternative)", cex = 1.3), ylab = list("Prob. of green\n(focal)", cex = 1.3),
          zlab = list("How causal\nwas green?", cex = 1.3))

wireframe(rating.mean ~ alt_high * focal_high, data = df.graph, colorkey = TRUE, drape = TRUE,  screen=list(z=130, x=-60, y=0),
          col.regions = colorRampPalette(c("red", "yellow"))(100), light.source = c(10,0,10)) 

# Test for linear effects -------------------------------------------------


linear.model = lmer(rating ~ focal_high + alt_high + (1 + focal_high + alt_high | subject), data = data)
summary(linear.model)


# Compute correlations ----------------------------------------------------

# all the models
# "x" stands for Prob(green = 1); "a" stands for Prob(blue = 1)

normed = function(x,a,f) {exp(f(x,a)) / (exp(f(x,a)) + exp(f(a,x)))}
our_model = function(x,a) {ifelse(a == 1 & x == 1, 1/2, a*(1-x)/(1-x*a))}
icard = function(x,a) {1-x*(1-a)} # model from Icard et al.
hh = function(x,a) {ifelse(x > .5, 0, ifelse(x < a, 1, 1/2))} # model from Halpern & Hitchcock
sp = function(x,a) {a*(1-x)} # model from Spellman
dp = function(x,a) {a} # delta-P model (and Power-PC model)

df.cors = data.frame(actual = numeric(), ours = numeric(), sp = numeric(), dp = numeric(), hh = numeric(), icard = numeric(),
                     ours_normed = numeric(), sp_normed = numeric(), dp_normed = numeric(), hh_normed = numeric(), icard_normed = numeric())
df.modeling = matrix(0,10,10)
for (px in 1:10) {
  for (pa in 1:10) {
        x = px/10
        a = pa/10
        actual = df.graph$rating.mean[df.graph$focal_high == px & df.graph$alt_high == pa]
        df.cors = rbind(df.cors, data.frame(actual = actual, 
                                            ours = our_model(x,a), sp = sp(x,a), dp = dp(x,a), hh = hh(x,a), icard = icard(x,a),
                                            ours_normed = normed(x,a,our_model), sp_normed = normed(x,a,sp), dp_normed = normed(x,a,dp),
                                            hh_normed = normed(x,a,hh), icard_normed = normed(x,a,icard)))
        df.modeling[px, pa] = actual
  }
}

# check correlations b/w models and ratings
cor.test(df.cors$actual, df.cors$ours) # .86
cor.test(df.cors$actual, df.cors$ours_normed) # .87
cor.test(df.cors$actual, df.cors$sp) # .72
cor.test(df.cors$actual, df.cors$sp_normed) # .8
cor.test(df.cors$actual, df.cors$dp) # .62
cor.test(df.cors$actual, df.cors$dp_normed) # .8
cor.test(df.cors$actual, df.cors$hh) # .45
cor.test(df.cors$actual, df.cors$hh_normed) # .6
cor.test(df.cors$actual, df.cors$icard) # .74
cor.test(df.cors$actual, df.cors$icard_normed) # .8

# is our model significantly more correlated than the next-best?
# yes: t(98) = 3.91, p < .0002
r.test(n=100, r12 = cor(df.cors$actual, df.cors$ours_normed, use = "complete.obs"), 
       r13 = cor(df.cors$actual, df.cors$icard_normed, use = "complete.obs"), 
       r23 = cor(df.cors$ours, df.cors$icard_normed, use = "complete.obs"))

# scatterplots
theme_update(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
             panel.background = element_rect(colour = "black"),
             axis.text=element_text(size=20, colour = "black"), axis.title=element_text(size=24, face = "bold"),
             axis.title.x = element_text(vjust = 0),
             legend.title = element_text(size = 24, face = "bold"), legend.text = element_text(size = 20),
             plot.title = element_text(size = 26, face = "bold", vjust = 1))

ggplot(df.cors, aes(x = ours, y = actual)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_continuous(breaks = c(0, 1)) + 
  scale_y_continuous(breaks = c(.3, .8)) + 
  labs(x = "Our model's predictions", y = "Actual ratings")

ggplot(df.cors, aes(x = sp, y = actual)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_continuous(breaks = c(0, 1)) + 
  scale_y_continuous(breaks = c(.3, .8)) + 
  labs(x = "SP's predictions", y = "Actual ratings")

ggplot(df.cors, aes(x = dp, y = actual)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_continuous(breaks = c(0, 1)) + 
  scale_y_continuous(breaks = c(.3, .8)) + 
  labs(x = "Delta-P and Power-PC's predictions", y = "Actual ratings")

ggplot(df.cors, aes(x = hh, y = actual)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_continuous(breaks = c(0, 1)) + 
  scale_y_continuous(breaks = c(.3, .8)) + 
  labs(x = "Halpern & Hitchcock's predictions", y = "Actual ratings")

ggplot(df.cors, aes(x = icard, y = actual)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_continuous(breaks = c(0, 1)) + 
  scale_y_continuous(breaks = c(.3, .8)) + 
  labs(x = "Icard et al.'s predictions", y = "Actual ratings")

# normed
ggplot(df.cors, aes(x = ours_normed, y = actual)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_continuous(breaks = c(0, 1)) + 
  scale_y_continuous(breaks = c(.3, .8)) + 
  labs(x = "Our model's predictions", y = "Actual ratings")

ggplot(df.cors, aes(x = sp_normed, y = actual)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_continuous(breaks = c(0, 1)) + 
  scale_y_continuous(breaks = c(.3, .8)) + 
  labs(x = "SP's predictions", y = "Actual ratings")

ggplot(df.cors, aes(x = dp, y = actual)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_continuous(breaks = c(0, 1)) + 
  scale_y_continuous(breaks = c(.3, .8)) + 
  labs(x = "Delta-P and Power-PC's predictions", y = "Actual ratings")

ggplot(df.cors, aes(x = hh, y = actual)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_continuous(breaks = c(0, 1)) + 
  scale_y_continuous(breaks = c(.3, .8)) + 
  labs(x = "Halpern & Hitchcock's predictions", y = "Actual ratings")

ggplot(df.cors, aes(x = icard, y = actual)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_x_continuous(breaks = c(0, 1)) + 
  scale_y_continuous(breaks = c(.3, .8)) + 
  labs(x = "Icard et al.'s predictions", y = "Actual ratings")

# Save for model fitting --------------------------------------------------
write.table(data %>% mutate(rating = (rating + 1) / 10) %>%
              select(rating, focal_high, alt_high, subject) %>% 
              mutate(subject = as.numeric(subject)) %>%
              arrange(subject), 'ratings.csv', row.names = F, sep = ",", col.names = F)