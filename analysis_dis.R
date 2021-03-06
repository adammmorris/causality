
# Preliminaries -----------------------------------------------------------

require(ggplot2)
require(dplyr)
require(lme4)
require(lmerTest)
require(lattice)
require(psych)
require(BayesFactor)
require(smoothie)
se <- function(x) {sd(x) / sqrt(length(x))};
dodge <- position_dodge(width=0.9)

# Import data -----------------------------------------------------------

# only works in Rstudio -- otherwise you have to set the path manually!
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
data = read.csv('data_dis.csv') %>% arrange(subject) %>% filter(rating > -1) %>%
  mutate(rating = rating / 8, half = factor(trial_ind > 1, c(F, T), c('First', 'Second')))

# how many subjects are there?
numsubj = length(unique(data$subject))
numobs = nrow(data)

# Make graph of empirical results -----------------------------------------------------------

# graph both dimensions together
df.graph.2d = data %>% 
  group_by(green_prob, blue_prob) %>% 
  summarise(rating.mean = mean(rating), 
            rating.se = se(rating)) %>% 
  ungroup() %>% 
  mutate_at(vars(green_prob,blue_prob),funs(factor(.,labels = paste0(seq(10,100,10),"%"))))


ggplot(df.graph.2d,aes(x = green_prob, y = rating.mean, group = blue_prob, color = blue_prob))+
  geom_errorbar(aes(ymin = rating.mean-rating.se,ymax = rating.mean+rating.se),width=0,size=0.5)+
  geom_line(size=3)+
  geom_point(size=4)+
  labs(x = '',
       color = '',
       y = '')+
  scale_color_grey(start = 0.85, end = 0)+
  theme_bw()+
  theme(text = element_text(size = 26,color='black'),
        panel.grid = element_blank(),
        legend.position = 'none',
        legend.direction = 'horizontal'
  )+
  scale_y_continuous(breaks = c(), limits = c(0,1)) +
  scale_x_discrete(breaks = c())

# graph dimensions separately

df.graph.2d.green = data %>% 
  group_by(green_prob) %>% 
  summarise(rating.mean = mean(rating), 
            rating.se = se(rating)) %>% 
  ungroup() %>% 
  mutate_at(vars(green_prob),funs(factor(.,labels = paste0(seq(10,100,10),"%"))))

ggplot(df.graph.2d.green,aes(x = green_prob, y = rating.mean))+
  geom_errorbar(aes(ymin = rating.mean-rating.se,ymax = rating.mean+rating.se),width=0,size=0.5)+
  geom_point(size=4) +
  geom_smooth(method='lm')+
  ylab('') + xlab('') +
  scale_y_continuous(breaks = NULL, labels = NULL, limits = c(.6, .8)) +
  scale_x_discrete(breaks = c())

df.graph.2d.blue = data %>% 
  group_by(blue_prob) %>% 
  summarise(rating.mean = mean(rating), 
            rating.se = se(rating)) %>% 
  ungroup() %>% 
  mutate_at(vars(blue_prob),funs(factor(.,labels = paste0(seq(10,100,10),"%"))))

ggplot(df.graph.2d.blue,aes(x = blue_prob, y = rating.mean))+
  geom_errorbar(aes(ymin = rating.mean-rating.se,ymax = rating.mean+rating.se),width=0,size=0.5)+
  geom_point(size=4) +
  geom_smooth(method='lm')+
  ylab('') + xlab('') +
  scale_y_continuous(breaks = NULL, labels = NULL, limits = c(.6, .8)) +
  scale_x_discrete(breaks = c())

# check for order effects

df.graph.2d.green = data %>% 
  group_by(green_prob, half) %>% 
  summarise(rating.mean = mean(rating), 
            rating.se = se(rating)) %>% 
  ungroup() %>% 
  mutate_at(vars(green_prob),funs(factor(.,labels = paste0(seq(10,100,10),"%"))))

ggplot(df.graph.2d.green %>% filter(half == 'Second') %>% mutate(green_prob = as.numeric(green_prob)),aes(x = green_prob, y = rating.mean))+
  geom_errorbar(aes(ymin = rating.mean-rating.se,ymax = rating.mean+rating.se),width=0,size=0.5)+
  geom_point(size=4) +
  geom_smooth(method='lm')+
  ylab('') + xlab('') +
  scale_y_continuous(breaks = NULL, labels = NULL, limits = c(.59, .8)) +
  scale_x_discrete(breaks = c())

df.graph.2d.blue = data %>% 
  group_by(blue_prob, half) %>% 
  summarise(rating.mean = mean(rating), 
            rating.se = se(rating)) %>% 
  ungroup() %>% 
  mutate_at(vars(blue_prob),funs(factor(.,labels = paste0(seq(10,100,10),"%"))))

ggplot(df.graph.2d.blue %>% filter(half == 'Second') %>% mutate(blue_prob = as.numeric(blue_prob)),aes(x = blue_prob, y = rating.mean))+
  geom_errorbar(aes(ymin = rating.mean-rating.se,ymax = rating.mean+rating.se),width=0,size=0.5)+
  geom_point(size=4) +
  geom_smooth(method='lm')+
  ylab('') + xlab('') +
  scale_y_continuous(breaks = NULL, labels = NULL, limits = c(.59, .8)) +
  scale_x_discrete(breaks = c())


# split by certainty

data = data %>%
  mutate(green_prob_fac = factor(green_prob, levels = 1:10, labels = c('<1', '<1', '<1', '<1', '<1', '<1', '<1', '<1', '<1', '1')),
         blue_prob_fac = factor(blue_prob, levels = 1:10, labels = c('<1', '<1', '<1', '<1', '<1', '<1', '<1', '<1', '<1', '1')))

df.graph.2d.green2 = data %>% 
  group_by(green_prob, blue_prob_fac) %>% 
  summarise(rating.mean = mean(rating), 
            rating.se = se(rating),
            num = n()) %>% 
  ungroup()

ggplot(df.graph.2d.green2,aes(x = green_prob, y = rating.mean, color = blue_prob_fac, group = blue_prob_fac))+
  geom_smooth(method='lm')+
  geom_point(size=4) +
  ylab('') + xlab('') +
  scale_y_continuous(breaks = c()) +
  scale_x_discrete(breaks = c()) +
  scale_color_grey(start = 0.6, end = 0)+
  theme(legend.position = 'none')

df.graph.2d.blue2 = data %>% 
  group_by(blue_prob, green_prob_fac) %>% 
  summarise(rating.mean = mean(rating), 
            rating.se = se(rating)) %>% 
  ungroup()

ggplot(df.graph.2d.blue2 %>% filter(),aes(x = blue_prob, y = rating.mean, color = green_prob_fac, group = green_prob_fac))+
  geom_smooth(method='lm')+
  geom_point(size=4) +
  ylab('') + xlab('') +
  scale_y_continuous(breaks = NULL) +
  scale_x_discrete(breaks = c()) +
  scale_color_grey(start = 0.6, end = 0)+
  theme(legend.position = 'none')

# Test for statistical effects -------------------------------------------------


linear.model = lmer(rating ~ green_prob + blue_prob + (1 + green_prob + blue_prob | subject), data = data)
summary(linear.model)

# check for order effects in basic patterns
linear.model = lmer(rating ~ green_prob * trial_ind + blue_prob * trial_ind + (1 + green_prob + blue_prob + trial_ind | subject), data = data)
summary(linear.model)

linear.model = lm(rating ~ green_prob + blue_prob, data = data %>% filter(trial_ind == 0))
summary(linear.model)

# testing nonlinear pattern in supersession
data.blue.no1 = data %>% filter(blue_prob != 10)
numsubj = length(unique(data.blue.no1$subject))
linear.model = lmer(rating ~ blue_prob + (1 + blue_prob | subject), data = data.blue.no1)
summary(linear.model)
predictions = predict(linear.model, data.frame(blue_prob = rep(10,numsubj), subject = unique(data.blue.no1$subject)))
c(mean(predictions) - 1.96 * sd(predictions) / sqrt(length(predictions)), mean(predictions) + 1.96 * sd(predictions) / sqrt(length(predictions)))
mean((data %>% filter(blue_prob == 10))$rating)

# abnormal deflation | blue is certain
linear.model = lm(rating ~ green_prob, data = data %>% filter(blue_prob == 10))
summary(linear.model)
regressionBF(rating ~ green_prob, data = data %>% filter(blue_prob == 10))

linear.model = lm(rating ~ green_prob * blue_prob_fac, data = data)
summary(linear.model)

# supersession | green is certain
linear.model = lm(rating ~ blue_prob, data = data %>% filter(green_prob == 10))
summary(linear.model)
regressionBF(rating ~ blue_prob, data = data %>% filter(green_prob == 10))

linear.model = lm(rating ~ blue_prob * green_prob_fac, data = data)
summary(linear.model)


# Compute correlations ----------------------------------------------------

# all the models
# "x" stands for Prob(green = 1); "a" stands for Prob(blue = 1)

icard = function(x,a) {x} # model from Icard et al.
hh = function(x,a) {ifelse(x > .5 | a > .5, 0, 1)} # model from Halpern & Hitchcock
hh2 = function(x,a) {ifelse(x + a > 1, 0, 1)} # model from Halpern & Hitchcock
sp = function(x,a) {(1-a)*(1-x)} # model from Spellman
dp = function(x,a) {1-a} # delta-P model (and Power-PC model)
pc = function(x,a) {1}

df.graph = data %>% group_by(green_prob, blue_prob) %>% summarise(rating.mean = mean(rating), rating.se = se(rating))
df.cors = data.frame(x = numeric(), a = numeric(), actual = numeric(), ours = numeric(), sp = numeric(), dp = numeric(), hh = numeric(), icard = numeric(),
                     ours_normed = numeric(), sp_normed = numeric(), dp_normed = numeric(), hh_normed = numeric(), icard_normed = numeric(), test = numeric(), test_normed = numeric())
df.modeling = matrix(0,10,10)
for (px in 1:10) {
  for (pa in 1:10) {
        x = px/10
        a = pa/10
        actual = df.graph$rating.mean[df.graph$green_prob == px & df.graph$blue_prob == pa]
        df.cors = rbind(df.cors, data.frame(x = px, a = pa, actual = actual, 
                                            sp = sp(x,a), dp = dp(x,a), hh = hh(x,a), hh2 = hh2(x,a), icard = icard(x,a), pc = pc(x,a)))
        df.modeling[px, pa] = actual
  }
}

# check correlations b/w models and ratings
cor.test(df.cors$actual, df.cors$sp)
cor.test(df.cors$actual, df.cors$dp) 
cor.test(df.cors$actual, df.cors$pc)
cor.test(df.cors$actual, df.cors$hh) 
cor.test(df.cors$actual, df.cors$hh2) 
cor.test(df.cors$actual, df.cors$icard)

# is our model significantly more correlated than the next-best?
r.test(n=100, r12 = cor(df.cors$actual, df.cors$icard, use = "complete.obs"), 
       r13 = cor(df.cors$actual, df.cors$dp, use = "complete.obs"), 
       r23 = cor(df.cors$icard, df.cors$dp, use = "complete.obs"))


# Graph results -----------------------------------------------------------

theme_update(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
             panel.background = element_rect(colour = "black"),
             axis.text=element_text(size=20, colour = "black"), axis.title=element_text(size=24, face = "bold"),
             axis.title.x = element_text(vjust = 0),
             legend.title = element_text(size = 24, face = "bold"), legend.text = element_text(size = 20),
             plot.title = element_text(size = 26, face = "bold", vjust = 1))

df.cors.2d = df.cors %>% mutate_at(vars(x,a),funs(factor(.,labels = paste0(seq(10,100,10),"%"))))

splot = function(aesthetic) {
  ggplot(df.cors, aesthetic) +
    geom_point(size = 3) +
    geom_smooth(method = "lm") +
    xlim(0,1) + 
    ylim(0,1) +
    labs(x = "", y = "") +
    theme(axis.text = element_blank(), axis.ticks = element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.border = element_blank(), axis.line = element_line(colour = "black", size = 1),panel.background = element_blank())
}

twodplot = function(aesthetic, normed) {
  ggplot(df.cors.2d,aesthetic)+
    geom_line(size=3)+
    geom_point(size=4)+
    labs(x = '',
         color = '',
         y = '')+
    scale_color_grey(start = 0.85, end = 0)+
    theme_bw()+
    theme(text = element_text(size = 26,color='black'),
          panel.grid = element_blank(),
          legend.position = 'none',
          legend.direction = 'horizontal'
    )+
    #guides(col = guide_legend(nrow = 2, direction = 'vertical', byrow=T)) +
    ylim(ifelse(normed, .2, 0), ifelse(normed, .8, 1))+
    scale_y_continuous(limits = c(0,1), labels = NULL, breaks = NULL) +
    scale_x_discrete(breaks = c())
}

# sp
splot(aes(x=sp,y=actual))
twodplot(aes(x = x, y = sp, group = a, color = a), F)

# dp
splot(aes(x=dp,y=actual))
twodplot(aes(x = x, y = dp, group = a, color = a), F)

# pc
splot(aes(x=pc,y=actual))
twodplot(aes(x = x, y = pc, group = a, color = a), F)

# halpern & hitchcock
splot(aes(x=hh,y=actual))
twodplot(aes(x = x, y = hh, group = a, color = a), F)

splot(aes(x=hh2,y=actual))
twodplot(aes(x = x, y = hh2, group = a, color = a), F)

# icard
splot(aes(x=icard,y=actual))
twodplot(aes(x = x, y = icard, group = a, color = a), F)