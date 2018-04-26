setwd("E:/Github/1.project/Project-Statistical-Methods")
library(lme4)
library(nlme)

required.df = read.csv("4.HypothesisData/final_formatted_data.csv")
required.df


model1 = lm(score ~ meanPP + age + sex + session + task + scorer, data=required.df)
summary(model1)

model2 = lm(score ~ log(meanPP) + age + sex + session + task + scorer, data=required.df)
summary(model2)

shapiro.test(log(required.df$meanPP))
shapiro.test(required.df$meanPP)
qqnorm(required.df$meanPP)
qqline(required.df$meanPP)

qqnorm(log(required.df$meanPP))
qqline(log(required.df$meanPP))

hist(required.df$meanPP)
hist(log(required.df$meanPP))


model3 = lm(score ~ meanPP + age + sex + session + task + scorer + ms.year + timing, data=required.df)
summary(model3)

model4 = lm(timing ~ meanPP + age + sex + session + task + scorer + ms.year + score, data=required.df)
summary(model4)

model5 = lmer(score ~ meanPP + age + sex + session + task + scorer + 1|subject, data=required.df, REML=FALSE)
summary(model5)

# AIC      BIC   logLik deviance df.resid 
# 1419.2   1627.2   -652.6   1305.2      227 


model6 = lmer(score ~ meanPP + age + sex + session + task + scorer + ms.year + timing + 1|subject, data=required.df, REML=FALSE)
summary(model6)

# AIC      BIC   logLik deviance df.resid 
# 1531.7   1823.6   -685.8   1371.7      204


model7 = lmer(score ~ meanPP + age + sex + session + task + scorer + ms.year + 1|subject, data=required.df, REML=FALSE)
summary(model7)

# AIC      BIC   logLik deviance df.resid 
# 1435.5   1683.6   -649.8   1299.5      216 


model8 = lme(score ~ meanPP + age + sex + session + task + scorer + ms.year + timing,  data=required.df,
             random=~1|subject, na.action = na.omit)
summary(model8)
anova(model8)

# numDF denDF  F-value p-value
# (Intercept)     1   261 923.5788  <.0001
# meanPP          1   261   7.3138  0.0073
# age             1    11   0.7224  0.4135
# sex             1    11   1.8400  0.2021
# session         4   261  65.8279  <.0001
# task            1   261  14.4043  0.0002
# scorer          1   261   0.0236  0.8781
# ms.year         1    11   0.7534  0.4039
# timing          1   261   6.0633  0.0144

model9 = lmer(score ~ meanPP + session + task +  1|subject, data=required.df, REML=FALSE)
summary(model9)

# AIC      BIC   logLik deviance df.resid 
# 1365.0   1474.5   -652.5   1305.0      254 


model10 = lmer(score ~ meanPP + session + task + timing  +  1|subject, data=required.df, REML=FALSE)
summary(model10)

# AIC      BIC   logLik deviance df.resid 
# 1466.5   1605.1   -695.2   1390.5      246

###FOR TIMING
model11 = lme(timing ~ meanPP + age + sex + session + task + scorer + ms.year + score,  data=required.df,
             random=~1|subject, na.action = na.omit)
summary(model11)
anova(model11)

# numDF denDF  F-value p-value
# (Intercept)     1   261 1637.380  <.0001
# meanPP          1   261   22.103  <.0001
# age             1    11    0.045  0.8365
# sex             1    11    1.191  0.2985
# session         4   261   30.346  <.0001
# task            1   261 5123.462  <.0001
# scorer          1   261    0.000  1.0000
# ms.year         1    11    0.000  0.9944
# score           1   261    6.489  0.0114


model12 = lmer(timing ~ meanPP + age + sex + session + task + scorer + ms.year + score +  1|subject, data=required.df, REML=FALSE)
summary(model12)
# 
# AIC      BIC   logLik deviance df.resid 
# 3541.8   3833.7  -1690.9   3381.8      204
model13 = lmer(timing ~ meanPP + session + task +  1|subject, data=required.df, REML=FALSE)
summary(model13)

# AIC      BIC   logLik deviance df.resid 
# 3440.3   3549.8  -1690.2   3380.3      254

model14 = lmer(timing ~ meanPP + session + task + score +  1|subject, data=required.df, REML=FALSE)
summary(model14)
# 
# AIC      BIC   logLik deviance df.resid 
# 3452.1   3590.7  -1688.0   3376.1      246 


