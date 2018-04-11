
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

# only works in Rstudio -- otherwise you have to set the path manually!
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
data = read.csv('data.csv') %>% arrange(subject) %>% filter(rating > -1) %>% mutate(rating = rating / 8)

# how many subjects are there?
numsubj = length(unique(data$subject))
numobs = nrow(data)

# Make 3D graph -----------------------------------------------------------


df.graph = data %>% group_by(focal_high, alt_high) %>% summarise(rating.mean = mean(rating), rating.se = se(rating))
mat.graph = matrix(0, nrow = 10, ncol = 10)
for (i in 1:10) {
  for (j in 1:10) {
    mat.graph[i,j] = df.graph$rating.mean[df.graph$focal_high == i & df.graph$alt_high == j]
  }
}

wireframe(rating.mean ~ alt_high * focal_high, data = df.graph, colorkey = F, drape = TRUE,  screen=list(z=120, x=-70, y=0),
          xlab = list(""), ylab = list(""), zlab = list(""), par.settings = list(axis.line = list(col = NA)),
          scales = list(col = 1, relation = 'free', lwd = 3), zlim = c(.2, .8))
 
df.graph.2d = data %>% 
  group_by(focal_high, alt_high) %>% 
  summarise(rating.mean = mean(rating), 
            rating.se = se(rating)) %>% 
  ungroup() %>% 
  mutate_at(vars(focal_high,alt_high),funs(factor(.,labels = paste0(seq(10,100,10),"%"))))

ggplot(df.graph.2d,aes(x = focal_high, y = rating.mean, group = alt_high, color = alt_high))+
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
  #guides(col = guide_legend(nrow = 2, direction = 'vertical', byrow=T)) +
  ylim(0, .8)+
  scale_y_continuous(breaks = c()) +
  scale_x_discrete(breaks = c())


# Test for linear effects -------------------------------------------------


linear.model = lmer(rating ~ focal_high + alt_high + (1 + focal_high + alt_high | subject), data = data)
summary(linear.model)


linear.model2 = lm(rating ~ focal_high, data = data %>% filter(alt_high == 1))
summary(linear.model2)


# Compute correlations ----------------------------------------------------

# all the models
# "x" stands for Prob(green = 1); "a" stands for Prob(blue = 1)

normed = function(x,a,f) {exp(f(x,a)) / (exp(f(x,a)) + exp(f(a,x)))}
our_model = function(x,a) {ifelse(a == 1 & x == 1, 1/2, a*(1-x)/(1-x*a))}
icard = function(x,a) {1-x*(1-a)} # model from Icard et al.
hh = function(x,a) {ifelse(x > .5, 0, ifelse(x < a, 1, 1/2))} # model from Halpern & Hitchcock
sp = function(x,a) {a*(1-x)} # model from Spellman
dp = function(x,a) {a} # delta-P model (and Power-PC model)


df.cors = data.frame(x = numeric(), a = numeric(), actual = numeric(), ours = numeric(), sp = numeric(), dp = numeric(), hh = numeric(), icard = numeric(),
                     ours_normed = numeric(), sp_normed = numeric(), dp_normed = numeric(), hh_normed = numeric(), icard_normed = numeric())
df.modeling = matrix(0,10,10)
for (px in 1:10) {
  for (pa in 1:10) {
        x = px/10
        a = pa/10
        actual = df.graph$rating.mean[df.graph$focal_high == px & df.graph$alt_high == pa]
        df.cors = rbind(df.cors, data.frame(x = px, a = pa, actual = actual, 
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
cor.test(df.cors$actual, df.cors$icard_restricted) # .74
cor.test(df.cors$actual, df.cors$icard_normed) # .8

# is our model significantly more correlated than the next-best?
# yes: t(99) = 3.91, p < .0002
r.test(n=100, r12 = cor(df.cors$actual, df.cors$ours, use = "complete.obs"), 
       r13 = cor(df.cors$actual, df.cors$sp_normed, use = "complete.obs"), 
       r23 = cor(df.cors$ours, df.cors$sp_normed, use = "complete.obs"))

r.test(n=100, r12 = cor(df.cors$actual, df.cors$ours_normed, use = "complete.obs"), 
       r13 = cor(df.cors$actual, df.cors$sp_normed, use = "complete.obs"), 
       r23 = cor(df.cors$ours_normed, df.cors$sp_normed, use = "complete.obs"))

r.test(n=100, r12 = cor(df.cors$actual, df.cors$ours, use = "complete.obs"), 
       r13 = cor(df.cors$actual, df.cors$ours_normed, use = "complete.obs"), 
       r23 = cor(df.cors$ours, df.cors$ours_normed, use = "complete.obs"))


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
          panel.border = element_rect(color = 'black', fill = NA, size = 3))
}

threedplot = function(formula, ckey) {
  wireframe(formula, data = df.cors, colorkey = ckey, drape = TRUE,  screen=list(z=120, x=-70, y=0),
            xlab = list(""), ylab = list(""), zlab = list(""), par.settings = list(axis.line = list(col = NA)),
            scales = list(col = 1, relation = 'free', lwd = 3), zlim = c(0, 1))
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
    scale_y_continuous(breaks = c()) +
    scale_x_discrete(breaks = c())
}

# AFI model, unnormed
splot(aes(x=ours,y=actual))
threedplot(ours ~ a * x, T)
twodplot(aes(x = x, y = ours, group = a, color = a), F)

# AFI model, normed
splot(aes(x=ours_normed,y=actual))
threedplot(ours_normed ~ a * x, T)
twodplot(aes(x = x, y = ours_normed, group = a, color = a), T)

# sp
splot(aes(x=sp,y=actual))
threedplot(sp ~ a * x, F)
twodplot(aes(x = x, y = sp, group = a, color = a), F)

# dp / power-pc
splot(aes(x=dp,y=actual))
threedplot(dp ~ a * x, F)
twodplot(aes(x = x, y = dp, group = a, color = a), F)

# halpern & hitchcock
splot(aes(x=hh,y=actual))
threedplot(hh ~ a * x, F)
twodplot(aes(x = x, y = hh, group = a, color = a), F)

# icard
splot(aes(x=icard,y=actual))
threedplot(icard ~ a * x, F)
twodplot(aes(x = x, y = icard, group = a, color = a), F)

# others, normed
splot(aes(x=sp_normed,y=actual))
threedplot(sp_normed ~ a * x, F)
twodplot(aes(x = x, y = sp_normed, group = a, color = a), T)

# RT ----------------------------------------------------------------------

df.rt = data %>% filter(rt < 120000) %>% mutate(rt.log = log(rt), rt.sec = rt / 1000)
df.rt = df.rt %>% filter(rt.sec < 20)
hist(df.rt$rt.sec)

# Save for model fitting --------------------------------------------------
df.fitting = data %>% mutate(rating = (rating + 1) / 10) %>%
  select(rating, focal_high, alt_high, subject) %>% 
  mutate(subject = as.numeric(subject)) %>%
  arrange(subject)
df.test = df.fitting %>% group_by(subject) %>% summarize(n = length(unique(rating)))
write.table(df.fitting, 'ratings.csv', row.names = F, sep = ",", col.names = F)
