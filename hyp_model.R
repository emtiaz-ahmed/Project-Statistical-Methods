setwd("E:/Github/1.project/Project-Statistical-Methods")
library(lme4)
library(nlme)

required.df = read.csv("4.HypothesisData/hyp_test_for_randomized_data.csv")
required.df

model1 = lm(score ~ meanPP + age + sex + session + task + scorer, data=required.df)
summary(model1)

model2 = lm(score ~ log(meanPP) + age + sex + session + task + scorer, data=required.df)
summary(model2)

model3 = glm(score ~ meanPP + age + sex + session + task + scorer, data=required.df)
summary(model3)


model4 = lmer(score ~ meanPP + age + sex + session + task + scorer + 1|subject, data=required.df, REML=FALSE)
summary(model4)

# AIC      BIC   logLik deviance df.resid 
# 1417.6   1625.6   -651.8   1303.6      227 

model5 = lmer(score ~ meanPP + 1|subject, data=required.df, REML=FALSE)
summary(model5)
# 
# AIC      BIC   logLik deviance df.resid 
# 1623.9   1653.0   -803.9   1607.9      276 


model6 = lmer(score ~ meanPP + age  + 1|subject, data=required.df, REML=FALSE)
summary(model6)
# AIC      BIC   logLik deviance df.resid 
# 1623.9   1653.0   -803.9   1607.9      276 


model7 = lmer(score ~ meanPP + sex + 1|subject, data=required.df, REML=FALSE)
summary(model7)

# AIC      BIC   logLik deviance df.resid 
# 1572.5   1601.7   -778.3   1556.5      276

model8 = lmer(score ~ meanPP + sex + session + 1|subject, data=required.df, REML=FALSE)
summary(model8)

# AIC      BIC   logLik deviance df.resid 
# 1488.4   1597.9   -714.2   1428.4      254 

model9 = lmer(score ~ meanPP + sex + session + task + 1|subject, data=required.df, REML=FALSE)
summary(model9)
# 
# AIC      BIC   logLik deviance df.resid 
# 1375.8   1514.4   -649.9   1299.8      246

model10 = lmer(score ~ meanPP + sex + session + task + scorer + 1|subject, data=required.df, REML=FALSE)
summary(model10)

# AIC      BIC   logLik deviance df.resid 
# 1380.2   1551.7   -643.1   1286.2      237 



model11 = lme(score ~ meanPP + age +sex + session + task + scorer , data=required.df,random=~1|subject,
              na.action = na.omit)
anova(model11)

# numDF denDF  F-value p-value
# (Intercept)     1   262 899.7998  <.0001
# meanPP          1   262   7.1860  0.0078
# age             1    12   0.7036  0.4180
# sex             1    12   1.7928  0.2054
# session         4   262  64.7111  <.0001
# task            1   262  14.1597  0.0002
# scorer          1   262   0.0232  0.8791





# 
# 
# 
# 
# 
# r.df = subset(required.df, meanPP>0)
# r.df
# 
# model5 = lmer(score ~ log(meanPP) + age + sex + session + task + scorer + 1|subject, data=r.df, REML=FALSE)
# summary(model5)
# 
# model6 = lmer(score ~ log(meanPP) + age + sex + session + task + scorer + 1|subject, data=r.df, REML=FALSE)
# summary(model6)
# 
# 
# 
# # rr.df = required.df
# # rr.df$meanPP = as.character(rr.df$meanPP)
# # rr.df$meanPP[rr.df$meanPP < 0] = NA
# # rr.df$meanPP[is.na(rr.df$meanPP)] = 0
# # rr.df$meanPP = as.numeric(rr.df$meanPP)
# # 
# # 
# # model6 = lmer(score ~ log(meanPP) + age + sex + session + task + scorer + 1|subject, data=rr.df, REML=FALSE)
# # summary(model6)
# 
# hist(log(required.df$meanPP))

