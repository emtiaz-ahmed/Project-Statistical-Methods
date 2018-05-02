setwd("E:/Github/1.project/Project-Statistical-Methods")
library(lme4)
library(nlme)
library(ggplot2)

required.df = read.csv("4.HypothesisData/final_formatted_data.csv")
required.df


head(required.df)


model1 = lm(score ~ meanPP + age + sex + session + task + scorer, data=required.df, na.action = na.omit)
summary(model1)

# Call:
#   lm(formula = score ~ meanPP + age + sex + session + task + scorer, 
#      data = required.df, na.action = na.omit)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -9.4223 -2.2615  0.3624  2.5722  8.4426 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        6.9725     4.5886   1.520  0.12978    
# meanPP            -1.7048    82.7358  -0.021  0.98358    
# age                0.4632     0.1746   2.653  0.00844 ** 
#   sexMale           -2.0139     0.4620  -4.359 1.85e-05 ***
#   sessionSession 2   4.1382     0.6759   6.123 3.18e-09 ***
#   sessionSession 3   6.1145     0.6806   8.984  < 2e-16 ***
#   sessionSession 4   7.2968     0.6776  10.769  < 2e-16 ***
#   sessionSession 5   7.7831     0.6847  11.367  < 2e-16 ***
#   taskSuturing      -1.3173     0.4253  -3.097  0.00216 ** 
#   scorerScorer 2     0.0493     0.4237   0.116  0.90745    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 3.57 on 274 degrees of freedom
# (16 observations deleted due to missingness)
# Multiple R-squared:  0.4438,	Adjusted R-squared:  0.4256 
# F-statistic:  24.3 on 9 and 274 DF,  p-value: < 2.2e-16

model1.improved = lm(score ~ age + sex + session + task, data=required.df, na.action = na.omit)
summary(model1.improved) 

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        8.1027     4.0770   1.987 0.047810 *  
#   age                0.4226     0.1710   2.472 0.014001 *  
#   sexMale           -2.1942     0.4562  -4.810 2.42e-06 ***
#   sessionSession 2   4.0833     0.6562   6.223 1.69e-09 ***
#   sessionSession 3   6.1000     0.6562   9.296  < 2e-16 ***
#   sessionSession 4   7.2833     0.6562  11.099  < 2e-16 ***
#   sessionSession 5   7.2833     0.6562  11.099  < 2e-16 ***
#   taskSuturing      -1.4333     0.4150  -3.454 0.000635 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 3.594 on 292 degrees of freedom
# Multiple R-squared:  0.4342,	Adjusted R-squared:  0.4206 
# F-statistic: 32.01 on 7 and 292 DF,  p-value: < 2.2e-16


model2 = lme(score ~ meanPP + age + sex + session + task + scorer,  data=required.df,
             random=~1|subject, na.action = na.omit)

anova(model2)

# numDF denDF  F-value p-value
# (Intercept)     1   262 899.7998  <.0001
# meanPP          1   262   7.1860  0.0078
# age             1    12   0.7036  0.4180
# sex             1    12   1.7928  0.2054
# session         4   262  64.7111  <.0001
# task            1   262  14.1597  0.0002
# scorer          1   262   0.0232  0.8791

summary(model2)

# AIC      BIC    logLik
# 1418.679 1462.037 -697.3395



model2.improved = lme(score ~ meanPP + session + task,  data=required.df,
             random=~1|subject, na.action = na.omit)
summary(model2.improved)

# AIC      BIC    logLik
# 1418.66 1451.276 -700.3299

model2.improved.2 = lmer(score ~ session + task + 1|subject, data=required.df, REML=FALSE)
summary(model2.improved.2)

# AIC      BIC   logLik deviance df.resid 
# 1449.3   1534.5   -701.6   1403.3      277 

anova(model2.improved.2,model1.improved)

# Models:
#   model1.improved: score ~ age + sex + session + task
# model2.improved.2: score ~ session + task + 1 | subject
# Df    AIC    BIC  logLik deviance  Chisq Chi Df Pr(>Chisq)    
# model1.improved    9 1628.8 1662.2 -805.42   1610.8                             
# model2.improved.2 23 1449.3 1534.5 -701.65   1403.3 207.54     14  < 2.2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#######################################
# So, mixed model is better
######################################


###################################
#Normality test of meanPP:
###################################

shapiro.test(required.df$meanPP)


# Shapiro-Wilk normality test
# 
# data:  required.df$meanPP
# W = 0.71883, p-value < 2.2e-16

qqnorm(required.df$meanPP, pch = 1, frame = FALSE)
qqline(required.df$meanPP, col = "steelblue", lwd = 2)

########################## not normal based on p value and Q-Q plot



shapiro.test(log(required.df$meanPP))

# Shapiro-Wilk normality test
# 
# data:  log(required.df$meanPP)
# W = 0.15846, p-value < 2.2e-16

qqnorm(log(required.df$meanPP), pch = 1, frame = FALSE)
qqline(log(required.df$meanPP), col = "steelblue", lwd = 2)

########################## log of meanPP is also not normal based on p value and Q-Q plot


######################################
# Extra 1 ()
#######################################
model.e1 = lme(score ~ meanPP + age + sex + session + task + scorer + ms.year + timing,  data=required.df,
             random=~1|subject, na.action = na.omit)
summary(model.e1)
# Data: required.df 
# AIC      BIC    logLik
# 1423.994 1474.476 -697.9972

anova(model.e1)

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

model.e1.improved.1 = lme(score ~ meanPP + session + task + timing,  data=required.df,
               random=~1|subject, na.action = na.omit)
summary(model.e1.improved.1)

# AIC      BIC    logLik
# 1425.552 1461.756 -702.7758

# Value Std.Error  DF   t-value p-value
# (Intercept)      17.723800   1.53991 262 11.509602  0.0000
# meanPP           16.335557  69.48846 262  0.235083  0.8143
# sessionSession 2  3.457769   0.53451 262  6.469084  0.0000
# sessionSession 3  5.426561   0.54955 262  9.874613  0.0000
# sessionSession 4  6.325919   0.58653 262 10.785380  0.0000
# sessionSession 5  6.574769   0.58354 262 11.267119  0.0000
# taskSuturing      2.254570   1.43539 262  1.570698  0.1175
# timing           -0.003988   0.00160 262 -2.489422  0.0134

model.e1.improved.2 = lme(score ~ session + task + timing,  data=required.df,
                          random=~1|subject, na.action = na.omit)
summary(model.e1.improved.2)

# AIC      BIC    logLik
# 1524.377 1557.499 -753.1886



###########################
# Extra 2  (check any effect of experience and session on number of sutures)
##########################

m.e2 = lm(sutures ~ ms.year+session,data=required.df)
summary(m.e2)

# Call:
#   lm(formula = sutures ~ ms.year + session, data = required.df)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -3.0123 -0.8790  0.1851  0.8210  2.5158 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        1.0763     0.2131   5.051 7.75e-07 ***
#   ms.year            0.8026     0.1005   7.984 3.23e-14 ***
#   sessionSession 2   1.6667     0.2263   7.365 1.80e-12 ***
#   sessionSession 3   2.6333     0.2263  11.637  < 2e-16 ***
#   sessionSession 4   3.0667     0.2263  13.551  < 2e-16 ***
#   sessionSession 5   3.3000     0.2263  14.583  < 2e-16 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 1.239 on 294 degrees of freedom
# Multiple R-squared:  0.5413,	Adjusted R-squared:  0.5335 
# F-statistic: 69.38 on 5 and 294 DF,  p-value: < 2.2e-16

m.e2.mix = lme(sutures ~ ms.year+session,data=required.df, random=~1|subject)
summary(m.e2.mix)

# Linear mixed-effects model fit by REML
# Data: required.df 
# AIC      BIC    logLik
# 821.1376 850.6063 -402.5688
# 
# Random effects:
#   Formula: ~1 | subject
# (Intercept) Residual
# StdDev:   0.9619946 0.847297
# 
# Fixed effects: sutures ~ ms.year + session 
# Value Std.Error  DF   t-value p-value
# (Intercept)      1.076316 0.5670825 281  1.897988  0.0587
# ms.year          0.802632 0.3556553  13  2.256768  0.0419
# sessionSession 2 1.666667 0.1546946 281 10.773919  0.0000
# sessionSession 3 2.633333 0.1546946 281 17.022792  0.0000
# sessionSession 4 3.066667 0.1546946 281 19.824011  0.0000
# sessionSession 5 3.300000 0.1546946 281 21.332360  0.0000

################################ from both lm and lme we get significant result
anova(m.e2.mix, m.e2)

# Model df       AIC       BIC    logLik   Test L.Ratio p-value
# m.e2.mix     1  8  821.1376  850.6063 -402.5688                       
# m.e2         2  7 1000.0727 1025.8577 -493.0363 1 vs 2 180.935  <.0001

####### from anova we can see that the lm model is better than lme model


#################################
# Extra 3 (Any effect on timing)
#################################

m.e3 = lm(timing ~ meanPP + age + sex + task + scorer + ms.year + sutures + session, data=required.df)
summary(m.e3)

# Call:
#   lm(formula = timing ~ meanPP + age + sex + task + scorer + ms.year + 
#        sutures + session, data = required.df)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -349.60  -56.58    0.62   73.39  295.62 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       8.850e+01  2.188e+02   0.405   0.6861    
# meanPP            1.938e+03  2.656e+03   0.730   0.4662    
# age               1.212e+01  9.747e+00   1.243   0.2148    
# sexMale           2.341e+00  1.572e+01   0.149   0.8817    
# taskSuturing      8.727e+02  1.351e+01  64.572  < 2e-16 ***
#   scorerScorer 2    1.842e-13  1.346e+01   0.000   1.0000    
# ms.year           1.217e+01  1.705e+01   0.714   0.4760    
# sutures          -2.730e+01  6.161e+00  -4.430 1.37e-05 ***
#   sessionSession 2 -4.568e+01  2.356e+01  -1.939   0.0535 .  
# sessionSession 3 -3.795e+01  2.635e+01  -1.440   0.1509    
# sessionSession 4 -9.197e+01  2.831e+01  -3.249   0.0013 ** 
#   sessionSession 5 -7.394e+01  2.947e+01  -2.509   0.0127 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 113.4 on 272 degrees of freedom
# (16 observations deleted due to missingness)
# Multiple R-squared:  0.9408,	Adjusted R-squared:  0.9384 
# F-statistic: 392.8 on 11 and 272 DF,  p-value: < 2.2e-16


m.e3.improved = lm(timing ~ task + ms.year + sutures + session, data=required.df)
summary(m.e3.improved)

# lm(formula = timing ~ task + ms.year + sutures + session, data = required.df)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -342.26  -53.24    6.90   73.04  285.71 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       378.137     21.222  17.818  < 2e-16 ***
#   taskSuturing      874.333     13.013  67.187  < 2e-16 ***
#   ms.year            23.758     10.083   2.356 0.019127 *  
#   sutures           -25.030      5.303  -4.720 3.66e-06 ***
#   sessionSession 2  -49.150     22.394  -2.195 0.028964 *  
#   sessionSession 3  -44.388     24.867  -1.785 0.075296 .  
# sessionSession 4 -101.309     26.226  -3.863 0.000138 ***
#   sessionSession 5  -87.235     27.011  -3.230 0.001381 ** 
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 112.7 on 292 degrees of freedom
# Multiple R-squared:  0.9407,	Adjusted R-squared:  0.9393 
# F-statistic:   662 on 7 and 292 DF,  p-value: < 2.2e-16

m.e3.improved1 = lm(timing ~ task + sutures + session, data=required.df)
summary(m.e3.improved1)

# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       399.796     19.276  20.741  < 2e-16 ***
#   taskSuturing      874.333     13.114  66.671  < 2e-16 ***
#   sutures           -19.756      4.844  -4.078 5.86e-05 ***
#   sessionSession 2  -57.940     22.252  -2.604  0.00969 ** 
#   sessionSession 3  -58.276     24.345  -2.394  0.01731 *  
#   sessionSession 4 -117.482     25.508  -4.606 6.14e-06 ***
#   sessionSession 5 -104.639     26.183  -3.996 8.14e-05 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 113.6 on 293 degrees of freedom
# Multiple R-squared:  0.9396,	Adjusted R-squared:  0.9384 
# F-statistic: 759.6 on 6 and 293 DF,  p-value: < 2.2e-16

m.e3.mix = lme(timing ~ task + sutures + session, data=required.df, random = ~1|subject)
summary(m.e3.mix)

# Fixed effects: timing ~ task + sutures + session 
# Value Std.Error  DF  t-value p-value
# (Intercept)       394.2299 24.203446 279 16.28817  0.0000
# taskSuturing      874.3333 11.715302 279 74.63174  0.0000
# sutures           -17.2257  6.340272 279 -2.71687  0.0070
# sessionSession 2  -62.1571 21.325684 279 -2.91466  0.0038
# sessionSession 3  -64.9389 24.937499 279 -2.60407  0.0097
# sessionSession 4 -125.2411 26.854618 279 -4.66367  0.0000
# sessionSession 5 -112.9885 27.944381 279 -4.04333  0.0001

anova(m.e3.mix, m.e3.improved1)

# Model df      AIC      BIC    logLik   Test  L.Ratio p-value
# m.e3.mix           1  9 3612.935 3646.057 -1797.468                        
# m.e3.improved1     2  8 3651.803 3681.244 -1817.901 1 vs 2 40.86777  <.0001

#######from here we can see that, linear model is better than mix model























