###################################################################
#  This file read data from 4.HypothesisData folder and Performance file for all subjects and 
#  format the data for hypothesis testing.
###################################################################

setwd("E:/Github/1.project/Project-Statistical-Methods")
#getwd()
#this library lubridate required for converting time data using ms() function
library(lubridate)


pp.data = read.csv("4.HypothesisData/hyp_test_4.csv")
performance.data = read.csv("Data/MicrosurgeryPerformance.csv")


id = unlist(lapply(performance.data$ID, function(x){rep(c(x),20)}))
id

age = unlist(lapply(performance.data$Age, function(x){rep(c(x),20)}))
age

ms.year = unlist(lapply(performance.data$MS.Year, function(x){rep(c(x),20)}))
ms.year

sex = unlist(lapply(performance.data$Sex, function(x){if(x==1){rep(c("Male"),20)} else{rep(c("Female"),20)}}))
sex

subject = unlist(lapply(performance.data$ID, function(x){rep(paste(c("Subject",x), collapse = " "),20)}))
subject


session = rep(c(rep(c("Session 1"),4), rep(c("Session 2"),4), rep(c("Session 3"),4), rep(c("Session 4"),4),
                rep(c("Session 5"),4)),length(subject)/20)
session

task = rep(c(rep(c("Cutting"),2), rep(c("Suturing"),2)), length(subject)/4)
task


scorer = rep(c("Scorer 1","Scorer 2"),length(subject)/2)
scorer

score = as.numeric(as.vector(apply(performance.data, 1, function(x){c(x[8],x[10],x[9],x[11],x[15],x[17],x[16],x[18],x[22],
                                                                      x[24],x[23],x[25],x[29],x[31],x[30],x[32],
                                                                      x[36],x[38],x[37],x[39])})))
score

#using ms() function it converts into second
timing = as.numeric(ms(as.vector(apply(performance.data, 1, function(x){c(x[5],x[5],x[6],x[6],x[12],x[12],x[13],x[13],x[19],
                                                                       x[19],x[20],x[20],x[26],x[26],x[27],x[27],
                                                                       x[33],x[33],x[34],x[34])}))))
timing

meanPP = unlist(lapply(pp.data$Mean.PP, function(x){rep(c(x),2)}))
meanPP

sutures = as.numeric(as.vector(apply(performance.data, 1, function(x){c(rep(c(x[7]),4),rep(c(x[14]),4),
  rep(c(x[21]),4),rep(c(x[28]),4),rep(c(x[35]),4))})))
sutures

required.df = data.frame(id, age, ms.year, sex, subject, session, task, meanPP, timing, sutures, scorer, score)
required.df

write.csv(file="4.HypothesisData/final_formatted_data.csv", required.df, row.names = FALSE)








