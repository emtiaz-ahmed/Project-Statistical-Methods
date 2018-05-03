setwd("E:/Github/1.project/Project-Statistical-Methods")
library(lme4)
library(nlme)
library(ggplot2)

required.df = read.csv("4.HypothesisData/final_formatted_data.csv")
required.df


head(required.df)


# model1 = lm(score ~ meanPP + age + session + task + scorer, data=required.df, na.action = na.omit)
# summary(model1)
# 
# # Coefficients:
# #   Estimate Std. Error t value Pr(>|t|)    
# # (Intercept)        1.9837     4.5867   0.432 0.665720    
# # meanPP           -37.0441    84.9900  -0.436 0.663277    
# # age                0.6484     0.1748   3.710 0.000251 ***
# #   sessionSession 2   4.1435     0.6977   5.939 8.63e-09 ***
# #   sessionSession 3   6.0182     0.7021   8.571 7.50e-16 ***
# #   sessionSession 4   7.2343     0.6992  10.346  < 2e-16 ***
# #   sessionSession 5   7.8002     0.7067  11.037  < 2e-16 ***
# #   taskSuturing      -1.3347     0.4390  -3.040 0.002591 ** 
# #   scorerScorer 2     0.0493     0.4373   0.113 0.910330    
# # ---
# 
# model1.improved = lm(score ~ age + session + task, data=required.df, na.action = na.omit)
# summary(model1.improved) 
# 
# # Coefficients:
# #   Estimate Std. Error t value Pr(>|t|)    
# # (Intercept)        1.6497     3.9927   0.413 0.679786    
# # age                0.6383     0.1711   3.731 0.000229 ***
# #   sessionSession 2   4.0833     0.6805   6.000  5.8e-09 ***
# #   sessionSession 3   6.1000     0.6805   8.964  < 2e-16 ***
# #   sessionSession 4   7.2833     0.6805  10.702  < 2e-16 ***
# #   sessionSession 5   7.2833     0.6805  10.702  < 2e-16 ***
# #   taskSuturing      -1.4333     0.4304  -3.330 0.000979 ***


model2 = lme(score ~ meanPP + age + session + task + scorer,  data=required.df,
             random=~1|subject, na.action = na.omit)

summary(model2)

# Fixed effects: score ~ meanPP + age + session + task + scorer 
# Value Std.Error  DF   t-value p-value
# (Intercept)       2.557300  13.31801 262  0.192018  0.8479
# meanPP           18.258880  70.30933 262  0.259694  0.7953
# age               0.591338   0.56953  13  1.038286  0.3181
# sessionSession 2  3.821194   0.52025 262  7.344883  0.0000
# sessionSession 3  5.875547   0.52599 262 11.170442  0.0000
# sessionSession 4  7.024948   0.52170 262 13.465495  0.0000
# sessionSession 5  7.224059   0.52880 262 13.661181  0.0000
# taskSuturing     -1.226256   0.32563 262 -3.765798  0.0002
# scorerScorer 2    0.049296   0.32377 262  0.152256  0.8791


model2.improved = lme(score ~ session + task,  data=required.df,
             random=~1|subject, na.action = na.omit)
summary(model2.improved)




###################################
#Normality test of meanPP:
###################################

shapiro.test(required.df$meanPP)


# Shapiro-Wilk normality test
# 
# data:  required.df$meanPP
# W = 0.71883, p-value < 2.2e-16

pdf("hypothesis_plots/model_plot/pp_normality.pdf")
qqnorm(required.df$meanPP, pch = 1, frame = FALSE)
qqline(required.df$meanPP, col = "steelblue", lwd = 2)
dev.off()


########################## not normal based on p value and Q-Q plot



shapiro.test(log(required.df$meanPP))

# Shapiro-Wilk normality test
# 
# data:  log(required.df$meanPP)
# W = 0.15846, p-value < 2.2e-16

pdf("hypothesis_plots/model_plot/log_pp__normality.pdf")
qqnorm(log(required.df$meanPP), pch = 1, frame = FALSE)
qqline(log(required.df$meanPP), col = "steelblue", lwd = 2)
dev.off()

########################## log of meanPP is also not normal based on p value and Q-Q plot


######################################
# Extra 1 (experience is significant or not?)
#######################################
model.e1 = lme(score ~ meanPP + age + session + task + scorer + factor(ms.year),  data=required.df,
             random=~1|subject, na.action = na.omit)
summary(model.e1)

# AIC      BIC    logLik
# 1423.886 1470.809 -698.9429

# model.e1.improved.1 = lme(score ~ session + task + timing,  data=required.df,
#                random=~1|subject, na.action = na.omit)
# summary(model.e1.improved.1)
# 
# # AIC      BIC    logLik
# # 1425.552 1461.756 -702.7758
# 
# # Value Std.Error  DF   t-value p-value
# # (Intercept)      17.723800   1.53991 262 11.509602  0.0000
# # meanPP           16.335557  69.48846 262  0.235083  0.8143
# # sessionSession 2  3.457769   0.53451 262  6.469084  0.0000
# # sessionSession 3  5.426561   0.54955 262  9.874613  0.0000
# # sessionSession 4  6.325919   0.58653 262 10.785380  0.0000
# # sessionSession 5  6.574769   0.58354 262 11.267119  0.0000
# # taskSuturing      2.254570   1.43539 262  1.570698  0.1175
# # timing           -0.003988   0.00160 262 -2.489422  0.0134
# 
# model.e1.improved.2 = lme(score ~ session + task + timing,  data=required.df,
#                           random=~1|subject, na.action = na.omit)
# summary(model.e1.improved.2)
# 
# # AIC      BIC    logLik
# # 1524.377 1557.499 -753.1886

###### ms year is not significant

###########################
# Extra 2  (check any effect of experience and session on number of sutures)
##########################

m.e2 = lm(sutures ~ factor(ms.year)+session,data=required.df)
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

# m.e2.mix = lme(sutures ~ ms.year+session,data=required.df, random=~1|subject)
# summary(m.e2.mix)
# 
# # Linear mixed-effects model fit by REML
# # Data: required.df 
# # AIC      BIC    logLik
# # 821.1376 850.6063 -402.5688
# # 
# # Random effects:
# #   Formula: ~1 | subject
# # (Intercept) Residual
# # StdDev:   0.9619946 0.847297
# # 
# # Fixed effects: sutures ~ ms.year + session 
# # Value Std.Error  DF   t-value p-value
# # (Intercept)      1.076316 0.5670825 281  1.897988  0.0587
# # ms.year          0.802632 0.3556553  13  2.256768  0.0419
# # sessionSession 2 1.666667 0.1546946 281 10.773919  0.0000
# # sessionSession 3 2.633333 0.1546946 281 17.022792  0.0000
# # sessionSession 4 3.066667 0.1546946 281 19.824011  0.0000
# # sessionSession 5 3.300000 0.1546946 281 21.332360  0.0000

################################ from both lm and lme we get significant result


#################################
# Extra 3 (Any effect on timing)
#################################

# m.e3 = lm(timing ~ meanPP + age + sex + task + scorer + ms.year + sutures + session, data=required.df)
# summary(m.e3)

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


# m.e3.improved = lm(timing ~ task + ms.year + sutures + session, data=required.df)
# summary(m.e3.improved)

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

# m.e3.improved1 = lm(timing ~ task + sutures + session, data=required.df)
# summary(m.e3.improved1)

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

m.e3.mix = lme(timing ~ age + task + sutures + session, data=required.df, random = ~1|subject)
summary(m.e3.mix)


# Value Std.Error  DF  t-value p-value
# (Intercept)       122.5892 277.65771 279  0.44151  0.6592
# age                11.8986  12.11493  13  0.98214  0.3440
# taskSuturing      874.3333  11.71923 279 74.60674  0.0000
# sutures           -18.8683   6.54858 279 -2.88128  0.0043
# sessionSession 2  -59.4195  21.50518 279 -2.76303  0.0061
# sessionSession 3  -60.6135  25.31258 279 -2.39460  0.0173
# sessionSession 4 -120.2040  27.32489 279 -4.39906  0.0000
# sessionSession 5 -107.5680  28.46676 279 -3.77872  0.0002

########################
# Extra 5()
########################

model.i1 = lme(score ~ session + task + session*task,  data=required.df,
               random=~1|subject, na.action = na.omit)
summary(model.i1) 


model.i2 = lme(score ~ session + meanPP + session*meanPP,  data=required.df,
               random=~1|subject, na.action = na.omit)
summary(model.i2) 

model.i3 = lme(score ~ meanPP + task + task*meanPP,  data=required.df,
               random=~1|subject, na.action = na.omit)
summary(model.i3) 

########################
# Extra 6()
#####################



