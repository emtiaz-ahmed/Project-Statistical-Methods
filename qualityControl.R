setwd("E:/Github/1.project/Project-Statistical-Methods")

library(ggplot2)
library(lubridate)
library(gridExtra)
library(grid)
library(ggpubr)
library(cowplot)

#...read input file
data = read.csv("Data/MicrosurgeryPerformance.csv", header = TRUE, sep=",")
data

draw_Biographic_Data_plots<-function(data){
  label = c("Male", "Female")
  #...count male if sex = 1 and female if sex = 2 
  male = sum(data$Sex == 1)
  female = sum(data$Sex == 2)
  genders = c(male, female)
  
  #...create dataframe using label and genders
  gender.frame = data.frame(label, genders)
  
  #...plot the barplot for gender distribution
  ggplot(gender.frame, aes(x=label, y=genders, fill=label)) + geom_bar(stat = "identity") +
    scale_fill_manual(values = c("deeppink1","dodgerblue1")) +
    labs(title="Barplot of Gender distribution", x="Gender", y="Count") +
    guides(fill=FALSE) +
    scale_y_continuous(breaks = seq(0,12,by=2), limits = c(0,12)) +
    geom_text(aes(label=genders), vjust=0, nudge_y = 0.1) +
    theme_bw() +
    theme(plot.title = element_text(hjust=0.5))
  
  # ggsave(file="1.Quality_control/Biographic_data/gender_distribution.png", dpi = 600, width = 10, height = 8, units = "in")
  ggsave(file="1.Quality_control/Biographic_data/gender_distribution.pdf")
  
  #...plot the histogram for age distribution
  ggplot(data, aes(x=data$Age)) + geom_histogram(col="black", fill="dodgerblue1", binwidth = 1) +
    labs(title="Histogram of Age distribution", x="Age", y="Count") +
    scale_x_continuous(breaks=c(22,23,24,25,26))+
    theme_bw() +
    geom_text(stat='count', aes(label=..count..), vjust=0, nudge_y = 0.1) +
    theme(plot.title = element_text(hjust=0.5))
    
  
  # ggsave(file="1.Quality_control/Biographic_data/age_distribution.png", dpi = 600, width = 10, height = 8, units = "in")
  ggsave(file="1.Quality_control/Biographic_data/age_distribution.pdf")
  
}

draw_Trait_Psychometric_Data_plots<-function(){
  data.tai = read.table("Data/tai_scores.txt")
  
  #remove specific rows , because all subjects are not available in final file
  data.tai = data.tai[-c(5,6,9,15,18,20),]

  ggplot(data.tai, aes(data.tai$V2)) +
    geom_histogram(col="black", fill="dodgerblue1", binwidth=1) +
    labs(title="Histogram of tai scores", x="Tai Score", y="Count") +
    scale_x_continuous(breaks = seq(20,80,by=2), limits = c(20,80)) +
    theme_bw() +
    theme(plot.title = element_text(hjust=0.5))
  
  outputFile = paste("1.Quality_control/Trait_Psychometric_data/",".png")
  # ggsave(file="1.Quality_control/Trait_Psychometric_data/Tai_score_histogram.png", dpi = 600, width = 10, height = 8, units = "in")
  ggsave(file="1.Quality_control/Trait_Psychometric_data/Tai_score_histogram.pdf")
  
}



#...draw the timing plots for each subject based on their cutting and suturing time
draw_timing_plot <- function(data){
  #...create dataframe using subject name and all cutting time
  cut.timing = data.frame(data$ID, data$Cutting.Time.1, data$Cutting.Time.2, data$Cutting.Time.3, data$Cutting.Time.4, data$Cutting.Time.5)
  #...create dataframe using subject name and all suturing time
  sut.timing = data.frame(data$ID, data$Suturing.Time.1 , data$Suturing.Time.2, data$Suturing.Time.3,
                                 data$Suturing.Time.4, data$Suturing.Time.5)
  #...find the number of rows
  number.of.row = nrow(cut.timing)
  
  for(i in 1:number.of.row){
    #...select specific row from the dataframe
    sub.timing.cut = cut.timing[i,]
    sub.timing.sut = sut.timing[i,] 
    #...find the subject number
    subject.name = paste("Subject",sub.timing.cut$data.ID,sep=" ")
    
    #...create x axis ticks
    label = c(rep("Session 1",2), rep("Session 2",2), rep("Session 3",2), rep("Session 4",2), rep("Session 5",2))
    task.type = c("Cutting", "Suturing")
    tasks = rep(task.type, 5)
    
    #...convert timing into time using ms()[member of lubridate library] and then convert into numeric value
    cut.session1 = as.numeric(ms(sub.timing.cut$data.Cutting.Time.1)) / 60
    cut.session2 = as.numeric(ms(sub.timing.cut$data.Cutting.Time.2)) / 60
    cut.session3 = as.numeric(ms(sub.timing.cut$data.Cutting.Time.3)) / 60
    cut.session4 = as.numeric(ms(sub.timing.cut$data.Cutting.Time.4)) / 60
    cut.session5 = as.numeric(ms(sub.timing.cut$data.Cutting.Time.5)) / 60
    
    #...convert timing into time using ms()[member of lubridate library] and then convert into numeric value
    sut.session1 = as.numeric(ms(sub.timing.sut$data.Suturing.Time.1)) / 60
    sut.session2 = as.numeric(ms(sub.timing.sut$data.Suturing.Time.2)) / 60
    sut.session3 = as.numeric(ms(sub.timing.sut$data.Suturing.Time.3)) / 60
    sut.session4 = as.numeric(ms(sub.timing.sut$data.Suturing.Time.4)) / 60
    sut.session5 = as.numeric(ms(sub.timing.sut$data.Suturing.Time.5)) / 60
    
    #...copy the timing's
    timing = c(cut.session1,sut.session1, cut.session2, sut.session2, cut.session3, sut.session3
                 , cut.session4, sut.session4, cut.session5, sut.session5)
    
    
    #...create dataframe using label and cutting timing
    subject.timing = data.frame(label, tasks, timing)
    
    #...concate string to create title
    barTitle = paste("Timing barplot of", subject.name)
    xLabel = paste("Subject", subject.name)
    
   
    ggplot(subject.timing, aes(x=label, y=timing)) + geom_bar(aes(fill=tasks), position = "dodge", stat="identity") +
      labs(title = barTitle, x = "", y = "Time") +
      labs(fill = "Tasks") +
      theme_bw() +
      theme(plot.title = element_text(hjust=0.5)) +
      scale_y_continuous(breaks = seq(0,20,by=4), limits = c(0,20))
    
    outputFile = paste("1.Quality_control/Performance_Data/Timing/timing_plot_of_",subject.name,".png")
    #...save the output files
    ggsave(file = outputFile, dpi = 600, width = 10, height = 8, units = "in")
  }
  
  
  
}



#...draw the accuracy plot for each subject based on their score
draw_accuracy_plot <- function(){
  data = read.csv("Data/MicrosurgeryPerformance.csv")
  data
  # #...create dataframe using subject name and all scores
  score = data.frame(data$ID, data$Score.Cut1, data$Score.Cut2 , data$Score.Cut1.1, data$Score.Cut2.1,
                     data$Score.Cut1.2, data$Score.Cut2.2, data$Score.Cut1.3, data$Score.Cut2.3, 
                     data$Score.Cut1.4, data$Score.Cut2.4, data$Score.Sut1, data$Score.Sut2, 
                     data$Score.Sut1.1, data$Score.Sut2.1, data$Score.Sut1.2, data$Score.Sut2.2,
                     data$Score.Sut1.3, data$Score.Sut2.3, data$Score.Sut1.4, data$Score.Sut2.4)
  # score = data.frame(data$UH,data$Score.1, data$Score.2, data$Score.3, data$Score.4, data$Score.5,
  #                    data$Sutures.1, data$Sutures.2, data$Sutures.3, data$Sutures.4, data$Sutures.5)
  score
  # names(score)
  #...find the number of rows
  number.of.row = nrow(score)
  

  for(i in 1:number.of.row){
    #...select specific row from the dataframe
    sub.score = score[i,]
    sub.score
    #...find the subject number
    subject.name = paste("Subject",sub.score$data.ID,sep=" ")

    #...create x axis ticks
    label = c(rep("Session 1",4), rep("Session 2",4), rep("Session 3",4), rep("Session 4",4),
              rep("Session 5",4))
    #... task label
    task = c(rep(c("Cutting 1", "Cutting 2", "Suturing 1", "Suturing 2" ),5))
    
    
    performance = c(sub.score$data.Score.Cut1 , sub.score$data.Score.Cut2, sub.score$data.Score.Sut1 ,
                    sub.score$data.Score.Sut2, sub.score$data.Score.Cut1.1 , sub.score$data.Score.Cut2.1,
                    sub.score$data.Score.Sut1.1 , sub.score$data.Score.Sut2.1, sub.score$data.Score.Cut1.2 ,
                    sub.score$data.Score.Cut2.2, sub.score$data.Score.Sut1.2 , sub.score$data.Score.Sut2.2,
                    sub.score$data.Score.Cut1.3 , sub.score$data.Score.Cut2.3, sub.score$data.Score.Sut1.3 ,
                    sub.score$data.Score.Sut2.3, sub.score$data.Score.Cut1.4 , sub.score$data.Score.Cut2.4,
                    sub.score$data.Score.Sut1.4 , sub.score$data.Score.Sut2.4)
    
    performance

    #...create dataframe using label, task and performance
    subject.performance = data.frame(label, task, performance)
    subject.performance

    #...concate string to create title
    barTitle = paste("Accuracy barplot of", subject.name)
    xLabel = paste("Subject", subject.name)

   
    ggplot(subject.performance, aes(x = label, y = performance)) + geom_bar(aes(fill=task), position = "dodge", stat="identity", width = 0.8, col="black") +
      labs(title = barTitle, x = "", y = "Score") +
      theme_bw() + 
      theme(plot.title = element_text(hjust=0.5)) +
      labs(fill = "Tasks") +
      scale_y_continuous(breaks = seq(0,30,by=2), limits = c(0,30))
  

    outputFile = paste("1.Quality_control/Performance_Data/Accuracy/accuracy_plot_of_",subject.name,".png")
    #...save the output files
    ggsave(file = outputFile, dpi = 600, width = 10, height = 8, units = "in")
    
  }

  
  
}


draw_state_psychometric_data<-function(){
  totalSubject = list.dirs(path = "Data", full.names = TRUE, recursive = FALSE)
  #removed specific subjects, because they are not in final file
  totalSubject = totalSubject[-c(5,6,9,15,18,20)]
  totalSubject
  flagg = 1
  
  for(j in totalSubject){
    # dir.name = paste("input/", "subject26" )
    # dir.name
    #...read all tai.csv files from all folders
     # subject.files <- dir("input/subject26", recursive=TRUE, full.names=TRUE, pattern="*_NASA.csv$")
    subject.files <- dir(j, recursive=TRUE, full.names=TRUE, pattern="*_NASA.csv$")
    subject.file.names <- list.dirs(path = "Data", full.names = FALSE, recursive = FALSE)
    #removed specific subjects, because they are not in final file
    subject.file.names = subject.file.names[-c(5,6,9,15,18,20)]
    subject.file.names
    #...find number of files
    total.Nasa.file = length(subject.files)
    total.Nasa.file
    
    # session.name = list.dirs(path = "input/subject21", full.names = FALSE, recursive = FALSE)
    session.name = list.dirs(path = j, full.names = FALSE, recursive = FALSE)
    total.session = total.Nasa.file / 2
    
    if(total.session < length(session.name)){
      flagg = flagg + 1
      next
    }
    
    nasa.label.list = list()
    for(sname in 1:length(session.name)){
      
      nasa.label.list[sname] = list(c(rep(session.name[sname],6)))
      
    }
    nasa.label = unlist(nasa.label.list)
    nasa.label
    
    
    subject.nasa = list()
    
    for(i in 1:total.Nasa.file){
      temp1 = read.csv(subject.files[i], header = TRUE)
      subject.nasa[i] = list(temp1)
      
    }
    subject.nasa
    subject.nasa.df = as.data.frame(subject.nasa)
    subject.nasa.df
    
   
   
    nasa.response = c(rep(c("Mental Demand", "Physical Demand", "Temporal Demand", "Performance", "Effort", "Frustration"),length(session.name)))
    nasa.response
    nasa.cutting = c(subject.nasa.df$Cutting1, subject.nasa.df$Cutting2, 
                     subject.nasa.df$Cutting3, subject.nasa.df$Cutting4, subject.nasa.df$Cutting5)
    nasa.cutting
    nasa.suturing = c(subject.nasa.df$Suturing1, subject.nasa.df$Suturing2, subject.nasa.df$Suturing3,
                      subject.nasa.df$Suturing4, subject.nasa.df$Suturing5)
    nasa.suturing
    
    nasa.data = data.frame(nasa.label, nasa.response, nasa.cutting, nasa.suturing)
    nasa.data
    
    subject.name = paste(" ", subject.file.names[flagg])
    subject.name
    # #...concate string to create title
    barTitle = paste("Barplots for all the NASA-TLX subscales of", subject.name)
    xLabel = subject.name
    
    
    ggplot(nasa.data, aes(x=nasa.label, y=nasa.cutting)) + geom_bar(aes(fill=nasa.response), position = "dodge", stat="identity", width = 0.8, col="black") +
      labs(title = barTitle, x = "", y = "Score") +
      theme_bw() +
      theme(plot.title = element_text(hjust=0.5)) +
      labs(fill = "Response") +
      scale_y_continuous(breaks = seq(0,20,by=4), limits = c(0,20)) +
      scale_fill_manual(values = c("dodgerblue","plum","gold","deeppink1","springgreen","gray33"))
    
    outputFile = paste("1.Quality_control/State_Psychometric_Data/Cutting/nasa_plot_of_subject_",subject.name,".png")
    #...save the output files
    ggsave(file = outputFile, dpi = 600, width = 10, height = 8, units = "in")
    ggplot(nasa.data, aes(x=nasa.label, y=nasa.suturing)) + geom_bar(aes(fill=nasa.response), position = "dodge", stat="identity", width = 0.8, col="black") +
      labs(title = barTitle, x = "", y = "Score") + theme_bw() +
      theme(plot.title = element_text(hjust=0.5)) +
      labs(fill = "Response") +
      scale_y_continuous(breaks = seq(0,20,by=4), limits = c(0,20)) +
      scale_fill_manual(values = c("dodgerblue","plum","gold","deeppink1","springgreen","gray33"))
    
    outputFile = paste("1.Quality_control/State_Psychometric_Data/Suturing/nasa_plot_of_subject_",subject.name,".png")
    #...save the output files
    ggsave(file = outputFile, dpi = 600, width = 10, height = 8, units = "in")
    
    # if(flagg == 5){
    #   break
    # }
    flagg = flagg + 1
  }
  
  
}


#...draw the accuracy plot for combined average subject based on their score
draw_accuracy_plot_combined <- function(){
  data = read.csv("Data/MicrosurgeryPerformance.csv")
  data
  
  
  
  #...create x axis ticks
  label = c(rep("Session 1",4), rep("Session 2",4), rep("Session 3",4), rep("Session 4",4),
            rep("Session 5",4))
  #... task label
  task = c(rep(c("Cutting Score 1", "Cutting Score 2", "Suturing Score 1", "Suturing Score 2" ),5))
  
  # #...create dataframe using subject name and all scores
  score = c(mean(data$Score.Cut1), mean(data$Score.Cut2) , mean(data$Score.Sut1), mean(data$Score.Sut2), 
            mean(data$Score.Cut1.1),mean(data$Score.Cut2.1), mean(data$Score.Sut1.1), mean(data$Score.Sut2.1),
            mean(data$Score.Cut1.2), mean(data$Score.Cut2.2),mean(data$Score.Sut1.2),mean(data$Score.Sut2.2),
            mean(data$Score.Cut1.3), mean(data$Score.Cut2.3), mean(data$Score.Sut1.3),mean(data$Score.Sut2.3),
            mean(data$Score.Cut1.4),mean(data$Score.Cut2.4), mean(data$Score.Sut1.4), mean(data$Score.Sut2.4))
  
  score
  
  performance = data.frame(label,task,score)
  
  
  ggplot(performance, aes(x = label, y = score)) + geom_bar(aes(fill=task), position = "dodge", stat="identity", width = 0.8, col="black") +
    labs(title = "Accuracy barplot of all subjects", x = "", y = "Score") +
    theme_bw() + 
    theme(plot.title = element_text(hjust=0.5)) +
    labs(fill = "Scores") +
    scale_y_continuous(breaks = seq(0,30,by=2), limits = c(0,30))
  
  
  outputFile = paste("1.Quality_control/Performance_Data/Accuracy/average_accuracy_plot.png")
  #...save the output files
  # ggsave(file = outputFile, dpi = 600, width = 10, height = 8, units = "in")
  
  ggsave(file = "1.Quality_control/Performance_Data/Accuracy/average_accuracy_plot.pdf")
  
  
  
}

#...draw the timing plots for combined (average) subject based on their cutting and suturing time
draw_timing_plot_combined <- function(){
  data = read.csv("Data/MicrosurgeryPerformance.csv")
  data
  #...create dataframe using subject name and all cutting time
  cut.timing = data.frame(data$Cutting.Time.1, data$Cutting.Time.2, data$Cutting.Time.3, data$Cutting.Time.4, data$Cutting.Time.5)
  #...create dataframe using subject name and all suturing time
  sut.timing = data.frame(data$Suturing.Time.1 , data$Suturing.Time.2, data$Suturing.Time.3,
                          data$Suturing.Time.4, data$Suturing.Time.5)
  #...find the number of rows
  number.of.row = nrow(cut.timing)
  
  cut.session1 = 0
  cut.session2 = 0
  cut.session3 = 0
  cut.session4 = 0
  cut.session5 = 0
  
  sut.session1 = 0
  sut.session2 = 0
  sut.session3 = 0
  sut.session4 = 0
  sut.session5 = 0
  
  for(i in 1:number.of.row){
    #...select specific row from the dataframe
    sub.timing.cut = cut.timing[i,]
    sub.timing.sut = sut.timing[i,] 
    #...find the subject number
    subject.name = paste("Subject",sub.timing.cut$data.ID,sep=" ")
    
    #...create x axis ticks
    label = c(rep("Session 1",2), rep("Session 2",2), rep("Session 3",2), rep("Session 4",2), rep("Session 5",2))
    task.type = c("Cutting", "Suturing")
    tasks = rep(task.type, 5)
    
    #...convert timing into time using ms()[member of lubridate library] and then convert into numeric value
    cut.session1 = cut.session1 +  as.numeric(ms(sub.timing.cut$data.Cutting.Time.1)) / 60
    cut.session2 = cut.session2 + as.numeric(ms(sub.timing.cut$data.Cutting.Time.2)) / 60
    cut.session3 = cut.session3 + as.numeric(ms(sub.timing.cut$data.Cutting.Time.3)) / 60
    cut.session4 = cut.session4 + as.numeric(ms(sub.timing.cut$data.Cutting.Time.4)) / 60
    cut.session5 = cut.session5 + as.numeric(ms(sub.timing.cut$data.Cutting.Time.5)) / 60
    
    #...convert timing into time using ms()[member of lubridate library] and then convert into numeric value
    sut.session1 = sut.session1 + as.numeric(ms(sub.timing.sut$data.Suturing.Time.1)) / 60
    sut.session2 = sut.session2 + as.numeric(ms(sub.timing.sut$data.Suturing.Time.2)) / 60
    sut.session3 = sut.session3 + as.numeric(ms(sub.timing.sut$data.Suturing.Time.3)) / 60
    sut.session4 = sut.session4 + as.numeric(ms(sub.timing.sut$data.Suturing.Time.4)) / 60
    sut.session5 = sut.session5 + as.numeric(ms(sub.timing.sut$data.Suturing.Time.5)) / 60
    
    
    
  }
  
  #...copy the timing's
  timing = c(cut.session1/15,sut.session1/15, cut.session2/15, sut.session2/15, cut.session3/15, sut.session3/15
             , cut.session4/15, sut.session4/15, cut.session5/15, sut.session5/15)
  timing
  
  #...create dataframe using label and cutting timing
  subject.timing = data.frame(label, tasks, timing)
  subject.timing

  
  
  ggplot(subject.timing, aes(x=label, y=timing)) + geom_bar(aes(fill=tasks), position = "dodge", stat="identity") +
    labs(title = "Timing barplot of all subjects", x = "", y = "Time [ m ]") +
    labs(fill = "Tasks") +
    theme_bw() +
    theme(plot.title = element_text(hjust=0.5)) +
    scale_y_continuous(breaks = seq(0,20,by=4), limits = c(0,20))
  
  outputFile = paste("1.Quality_control/Performance_Data/Timing/average_timing_plot.png")
  #...save the output files
  # ggsave(file = outputFile, dpi = 600, width = 10, height = 8, units = "in")
  ggsave(file = "1.Quality_control/Performance_Data/Timing/average_timing_plot.pdf")
  
  
}

draw_state_psychometric_data_combined<-function(){
  totalSubject = list.dirs(path = "Data", full.names = TRUE, recursive = FALSE)
  #removed specific subjects, because they are not in final file
  totalSubject = totalSubject[-c(5,6,9,15,18,20)]
  totalSubject
  flagg = 1
  
  nasa.cutting1.1 = 0
  nasa.cutting1.2 = 0
  nasa.cutting1.3 = 0
  nasa.cutting1.4 = 0
  nasa.cutting1.5 = 0
  nasa.cutting1.6 = 0
  nasa.cutting2.1 = 0
  nasa.cutting2.2 = 0
  nasa.cutting2.3 = 0
  nasa.cutting2.4 = 0
  nasa.cutting2.5 = 0
  nasa.cutting2.6 = 0
  nasa.cutting3.1 = 0
  nasa.cutting3.2 = 0
  nasa.cutting3.3 = 0
  nasa.cutting3.4 = 0
  nasa.cutting3.5 = 0
  nasa.cutting3.6 = 0
  nasa.cutting4.1 = 0
  nasa.cutting4.2 = 0
  nasa.cutting4.3 = 0
  nasa.cutting4.4 = 0
  nasa.cutting4.5 = 0
  nasa.cutting4.6 = 0
  nasa.cutting5.1 = 0
  nasa.cutting5.2 = 0
  nasa.cutting5.3 = 0
  nasa.cutting5.4 = 0
  nasa.cutting5.5 = 0
  nasa.cutting5.6 = 0
  
  nasa.suturing1.1 = 0
  nasa.suturing1.2 = 0
  nasa.suturing1.3 = 0
  nasa.suturing1.4 = 0
  nasa.suturing1.5 = 0
  nasa.suturing1.6 = 0
  nasa.suturing2.1 = 0
  nasa.suturing2.2 = 0
  nasa.suturing2.3 = 0
  nasa.suturing2.4 = 0
  nasa.suturing2.5 = 0
  nasa.suturing2.6 = 0
  
  nasa.suturing3.1 = 0
  nasa.suturing3.2 = 0
  nasa.suturing3.3 = 0
  nasa.suturing3.4 = 0
  nasa.suturing3.5 = 0
  nasa.suturing3.6 = 0
  
  nasa.suturing4.1 = 0
  nasa.suturing4.2 = 0
  nasa.suturing4.3 = 0
  nasa.suturing4.4 = 0
  nasa.suturing4.5 = 0
  nasa.suturing4.6 = 0
  
  nasa.suturing5.1 = 0
  nasa.suturing5.2 = 0
  nasa.suturing5.3 = 0
  nasa.suturing5.4 = 0
  nasa.suturing5.5 = 0
  nasa.suturing5.6 = 0
  
  
  for(j in totalSubject){
    subject.files <- dir(j, recursive=TRUE, full.names=TRUE, pattern="*_NASA.csv$")
    subject.file.names <- list.dirs(path = "Data", full.names = FALSE, recursive = FALSE)
    #removed specific subjects, because they are not in final file
    subject.file.names = subject.file.names[-c(5,6,9,15,18,20)]
    subject.file.names
    #...find number of files
    total.Nasa.file = length(subject.files)
    total.Nasa.file
    
    # session.name = list.dirs(path = "input/subject21", full.names = FALSE, recursive = FALSE)
    session.name = list.dirs(path = j, full.names = FALSE, recursive = FALSE)
    total.session = total.Nasa.file / 2
    
    if(total.session < length(session.name)){
      flagg = flagg + 1
      next
    }
    
    nasa.label.list = list()
    for(sname in 1:length(session.name)){
      
      nasa.label.list[sname] = list(c(rep(session.name[sname],6)))
      
    }
    
    
    subject.nasa = list()
    
    for(i in 1:total.Nasa.file){
      temp1 = read.csv(subject.files[i], header = TRUE)
      subject.nasa[i] = list(temp1)
      
    }
    subject.nasa
    subject.nasa.df = as.data.frame(subject.nasa)
    subject.nasa.df
   
    nasa.cutting1.1 = nasa.cutting1.1 + subject.nasa.df$Cutting1[1]
    nasa.cutting1.2 = nasa.cutting1.2 + subject.nasa.df$Cutting1[2]
    nasa.cutting1.3 = nasa.cutting1.3 + subject.nasa.df$Cutting1[3]
    nasa.cutting1.4 = nasa.cutting1.4 + subject.nasa.df$Cutting1[4]
    nasa.cutting1.5 = nasa.cutting1.5 + subject.nasa.df$Cutting1[5]
    nasa.cutting1.6 = nasa.cutting1.6 + subject.nasa.df$Cutting1[6]
    
    nasa.cutting2.1 = nasa.cutting2.1 + subject.nasa.df$Cutting2[1]
    nasa.cutting2.2 = nasa.cutting2.2 + subject.nasa.df$Cutting2[2]
    nasa.cutting2.3 = nasa.cutting2.3 + subject.nasa.df$Cutting2[3]
    nasa.cutting2.4 = nasa.cutting2.4 + subject.nasa.df$Cutting2[4]
    nasa.cutting2.5 = nasa.cutting2.5 + subject.nasa.df$Cutting2[5]
    nasa.cutting2.6 = nasa.cutting2.6 + subject.nasa.df$Cutting2[6]
    
    nasa.cutting3.1 = nasa.cutting3.1 + subject.nasa.df$Cutting3[1]
    nasa.cutting3.2 = nasa.cutting3.2 + subject.nasa.df$Cutting3[2]
    nasa.cutting3.3 = nasa.cutting3.3 + subject.nasa.df$Cutting3[3]
    nasa.cutting3.4 = nasa.cutting3.4 + subject.nasa.df$Cutting3[4]
    nasa.cutting3.5 = nasa.cutting3.5 + subject.nasa.df$Cutting3[5]
    nasa.cutting3.6 = nasa.cutting3.6 + subject.nasa.df$Cutting3[6]
    
    nasa.cutting4.1 = nasa.cutting4.1 + subject.nasa.df$Cutting4[1]
    nasa.cutting4.2 = nasa.cutting4.2 + subject.nasa.df$Cutting4[2]
    nasa.cutting4.3 = nasa.cutting4.3 + subject.nasa.df$Cutting4[3]
    nasa.cutting4.4 = nasa.cutting4.4 + subject.nasa.df$Cutting4[4]
    nasa.cutting4.5 = nasa.cutting4.5 + subject.nasa.df$Cutting4[5]
    nasa.cutting4.6 = nasa.cutting4.6 + subject.nasa.df$Cutting4[6]
    
    nasa.cutting5.1 = nasa.cutting5.1 + subject.nasa.df$Cutting5[1]
    nasa.cutting5.2 = nasa.cutting5.2 + subject.nasa.df$Cutting5[2]
    nasa.cutting5.3 = nasa.cutting5.3 + subject.nasa.df$Cutting5[3]
    nasa.cutting5.4 = nasa.cutting5.4 + subject.nasa.df$Cutting5[4]
    nasa.cutting5.5 = nasa.cutting5.5 + subject.nasa.df$Cutting5[5]
    nasa.cutting5.6 = nasa.cutting5.6 + subject.nasa.df$Cutting5[6]
    
    nasa.suturing1.1 = nasa.suturing1.1 + subject.nasa.df$Suturing1[1]
    nasa.suturing1.2 = nasa.suturing1.2 + subject.nasa.df$Suturing1[2]
    nasa.suturing1.3 = nasa.suturing1.3 + subject.nasa.df$Suturing1[3]
    nasa.suturing1.4 = nasa.suturing1.4 + subject.nasa.df$Suturing1[4]
    nasa.suturing1.5 = nasa.suturing1.5 + subject.nasa.df$Suturing1[5]
    nasa.suturing1.6 = nasa.suturing1.6 + subject.nasa.df$Suturing1[6]
    
    nasa.suturing2.1 = nasa.suturing2.1 + subject.nasa.df$Suturing2[1]
    nasa.suturing2.2 = nasa.suturing2.2 + subject.nasa.df$Suturing2[2]
    nasa.suturing2.3 = nasa.suturing2.3 + subject.nasa.df$Suturing2[3]
    nasa.suturing2.4 = nasa.suturing2.4 + subject.nasa.df$Suturing2[4]
    nasa.suturing2.5 = nasa.suturing2.5 + subject.nasa.df$Suturing2[5]
    nasa.suturing2.6 = nasa.suturing2.6 + subject.nasa.df$Suturing2[6]
    
    nasa.suturing3.1 = nasa.suturing3.1 + subject.nasa.df$Suturing3[1]
    nasa.suturing3.2 = nasa.suturing3.2 + subject.nasa.df$Suturing3[2]
    nasa.suturing3.3 = nasa.suturing3.3 + subject.nasa.df$Suturing3[3]
    nasa.suturing3.4 = nasa.suturing3.4 + subject.nasa.df$Suturing3[4]
    nasa.suturing3.5 = nasa.suturing3.5 + subject.nasa.df$Suturing3[5]
    nasa.suturing3.6 = nasa.suturing3.6 + subject.nasa.df$Suturing3[6]
    
    nasa.suturing4.1 = nasa.suturing4.1 + subject.nasa.df$Suturing4[1]
    nasa.suturing4.2 = nasa.suturing4.2 + subject.nasa.df$Suturing4[2]
    nasa.suturing4.3 = nasa.suturing4.3 + subject.nasa.df$Suturing4[3]
    nasa.suturing4.4 = nasa.suturing4.4 + subject.nasa.df$Suturing4[4]
    nasa.suturing4.5 = nasa.suturing4.5 + subject.nasa.df$Suturing4[5]
    nasa.suturing4.6 = nasa.suturing4.6 + subject.nasa.df$Suturing4[6]
    
    nasa.suturing5.1 = nasa.suturing5.1 + subject.nasa.df$Suturing5[1]
    nasa.suturing5.2 = nasa.suturing5.2 + subject.nasa.df$Suturing5[2]
    nasa.suturing5.3 = nasa.suturing5.3 + subject.nasa.df$Suturing5[3]
    nasa.suturing5.4 = nasa.suturing5.4 + subject.nasa.df$Suturing5[4]
    nasa.suturing5.5 = nasa.suturing5.5 + subject.nasa.df$Suturing5[5]
    nasa.suturing5.6 = nasa.suturing5.6 + subject.nasa.df$Suturing5[6]
    
    
    flagg = flagg + 1
    
  }
  
  nasa.label = c(rep("Session 1",6), rep("Session 2",6), rep("Session 3",6), rep("Session 4",6), rep("Session 5",6))
  nasa.response = c(rep(c("Mental Demand", "Physical Demand", "Temporal Demand", "Performance", "Effort", "Frustration"),5))
  nasa.response
  
  nasa.cut = c(nasa.cutting1.1/13,nasa.cutting1.2/13, nasa.cutting1.3/13, nasa.cutting1.4/13,nasa.cutting1.5/13,nasa.cutting1.6/13,
               nasa.cutting2.1/13,nasa.cutting2.2/13, nasa.cutting2.3/13, nasa.cutting2.4/13,nasa.cutting2.5/13,nasa.cutting2.6/13,
               nasa.cutting3.1/13,nasa.cutting3.2/13, nasa.cutting3.3/13, nasa.cutting3.4/13,nasa.cutting3.5/13,nasa.cutting3.6/13,
               nasa.cutting4.1/13,nasa.cutting4.2/13, nasa.cutting4.3/13, nasa.cutting4.4/13,nasa.cutting4.5/13,nasa.cutting4.6/13,
               nasa.cutting5.1/13,nasa.cutting5.2/13, nasa.cutting5.3/13, nasa.cutting5.4/13,nasa.cutting5.5/13,nasa.cutting5.6/13)
               
  nasa.sut = c(nasa.suturing1.1/13,nasa.suturing1.2/13,nasa.suturing1.3/13,nasa.suturing1.4/13,nasa.suturing1.5/13,nasa.suturing1.6/13,
               nasa.suturing2.1/13,nasa.suturing2.2/13,nasa.suturing2.3/13,nasa.suturing2.4/13,nasa.suturing2.5/13,nasa.suturing2.6/13,
               nasa.suturing3.1/13,nasa.suturing3.2/13,nasa.suturing3.3/13,nasa.suturing3.4/13,nasa.suturing3.5/13,nasa.suturing3.6/13,
               nasa.suturing4.1/13,nasa.suturing4.2/13,nasa.suturing4.3/13,nasa.suturing4.4/13,nasa.suturing4.5/13,nasa.suturing4.6/13,
               nasa.suturing5.1/13,nasa.suturing5.2/13,nasa.suturing5.3/13,nasa.suturing5.4/13,nasa.suturing5.5/13,nasa.suturing5.6/13)
  
  nasa.data = data.frame(nasa.label, nasa.response, nasa.cut, nasa.sut)
  nasa.data
  
  ggplot(nasa.data, aes(x=nasa.label, y=nasa.cut)) + geom_bar(aes(fill=nasa.response), position = "dodge", stat="identity", width = 0.8, col="black") +
    labs(title = "Barplots for all the NASA-TLX subscales of Cutting", x = "", y = "Avg. Score") +
    theme_bw() +
    theme(plot.title = element_text(hjust=0.5)) +
    labs(fill = "Response") +
    scale_y_continuous(breaks = seq(0,20,by=4), limits = c(0,20)) +
    scale_fill_manual(values = c("dodgerblue","plum","gold","deeppink1","springgreen","gray33"))
  
  outputFile = paste("1.Quality_control/State_Psychometric_Data/Cutting/average_cutting_nasa_plot.png")
  #...save the output files
  # ggsave(file = outputFile, dpi = 600, width = 10, height = 8, units = "in")
  ggsave(file = "1.Quality_control/State_Psychometric_Data/Cutting/average_cutting_nasa_plot.pdf",width = 10, height = 8, units = "in")
  ggplot(nasa.data, aes(x=nasa.label, y=nasa.sut)) + geom_bar(aes(fill=nasa.response), position = "dodge", stat="identity", width = 0.8, col="black") +
    labs(title = "Barplots for all the NASA-TLX subscales of Suturing", x = "", y = "Avg. Score") + theme_bw() +
    theme(plot.title = element_text(hjust=0.5)) +
    labs(fill = "Response") +
    scale_y_continuous(breaks = seq(0,20,by=4), limits = c(0,20)) +
    scale_fill_manual(values = c("dodgerblue","plum","gold","deeppink1","springgreen","gray33"))
  
  outputFile = paste("1.Quality_control/State_Psychometric_Data/Suturing/average_suturing_nasa_plot.png")
  #...save the output files
  # ggsave(file = outputFile, dpi = 600, width = 10, height = 8, units = "in")
  ggsave(file = "1.Quality_control/State_Psychometric_Data/Suturing/average_suturing_nasa_plot.pdf",width = 10, height = 8, units = "in")
  
  
}


#...draw the accuracy plot grid 
draw_accuracy_plot_grid <- function(){
  data = read.csv("Data/MicrosurgeryPerformance.csv")
  data
  acc.plots1 = list()
  acc.plots2 = list()
  # #...create dataframe using subject name and all scores
  score = data.frame(data$ID, data$Score.Cut1, data$Score.Cut2 , data$Score.Cut1.1, data$Score.Cut2.1,
                     data$Score.Cut1.2, data$Score.Cut2.2, data$Score.Cut1.3, data$Score.Cut2.3, 
                     data$Score.Cut1.4, data$Score.Cut2.4, data$Score.Sut1, data$Score.Sut2, 
                     data$Score.Sut1.1, data$Score.Sut2.1, data$Score.Sut1.2, data$Score.Sut2.2,
                     data$Score.Sut1.3, data$Score.Sut2.3, data$Score.Sut1.4, data$Score.Sut2.4)
  # score = data.frame(data$UH,data$Score.1, data$Score.2, data$Score.3, data$Score.4, data$Score.5,
  #                    data$Sutures.1, data$Sutures.2, data$Sutures.3, data$Sutures.4, data$Sutures.5)
  score
  # names(score)
  #...find the number of rows
  number.of.row = nrow(score)
  flag = 1
  
  for(i in 1:number.of.row){
    #...select specific row from the dataframe
    sub.score = score[i,]
    sub.score
    #...find the subject number
    subject.name = paste("Subject",sub.score$data.ID,sep=" ")
    
    #...create x axis ticks
    label = c(rep("Session 1",4), rep("Session 2",4), rep("Session 3",4), rep("Session 4",4),
              rep("Session 5",4))
    #... task label
    task = c(rep(c("Cutting 1", "Cutting 2", "Suturing 1", "Suturing 2" ),5))
    
    
    performance = c(sub.score$data.Score.Cut1 , sub.score$data.Score.Cut2, sub.score$data.Score.Sut1 ,
                    sub.score$data.Score.Sut2, sub.score$data.Score.Cut1.1 , sub.score$data.Score.Cut2.1,
                    sub.score$data.Score.Sut1.1 , sub.score$data.Score.Sut2.1, sub.score$data.Score.Cut1.2 ,
                    sub.score$data.Score.Cut2.2, sub.score$data.Score.Sut1.2 , sub.score$data.Score.Sut2.2,
                    sub.score$data.Score.Cut1.3 , sub.score$data.Score.Cut2.3, sub.score$data.Score.Sut1.3 ,
                    sub.score$data.Score.Sut2.3, sub.score$data.Score.Cut1.4 , sub.score$data.Score.Cut2.4,
                    sub.score$data.Score.Sut1.4 , sub.score$data.Score.Sut2.4)
    
    performance
    
    #...create dataframe using label, task and performance
    subject.performance = data.frame(label, task, performance)
    subject.performance
    
    #...concate string to create title
    barTitle = subject.name
    xLabel = paste("Subject", subject.name)
    
    if(i < 9){
      # used p for collect the legend
      p = ggplot(subject.performance, aes(x = label, y = performance)) + geom_bar(aes(fill=task), position = "dodge", stat="identity", width = 0.8, col="black") +
        labs(title = barTitle, x = "", y = "Score") +
        theme_bw() + 
        theme(plot.title = element_text(hjust=0.5)) +
        labs(fill = "Tasks") +
        scale_y_continuous(breaks = seq(0,30,by=2), limits = c(0,30))
      
      acc.plots1[[i]] = ggplot(subject.performance, aes(x = label, y = performance)) + geom_bar(aes(fill=task), position = "dodge", stat="identity", width = 0.8, col="black") +
        labs(title = barTitle, x = "", y = "Score") +
        theme_bw() + 
        theme(plot.title = element_text(hjust=0.5), legend.position = "none") +
        labs(fill = "Tasks") +
        scale_y_continuous(breaks = seq(0,30,by=2), limits = c(0,30))
    }else{
      acc.plots2[[i-8]] = ggplot(subject.performance, aes(x = label, y = performance)) + geom_bar(aes(fill=task), position = "dodge", stat="identity", width = 0.8, col="black") +
        labs(title = barTitle, x = "", y = "Score") +
        theme_bw() + 
        theme(plot.title = element_text(hjust=0.5), legend.position = "none") +
        labs(fill = "Tasks") +
        scale_y_continuous(breaks = seq(0,30,by=2), limits = c(0,30))
    }
    
    
    
    
    
  }
  #add the legend as a new plot, and get_legend() from cowplot
  acc.plots1[[9]] <- get_legend(p + theme(legend.position="right"))
  acc.plots2[[number.of.row-7]] <- get_legend(p + theme(legend.position="right"))
 
  
  m1 = marrangeGrob(acc.plots1, nrow=3,ncol = 3, top = "")
  m2 = marrangeGrob(acc.plots2, nrow=3,ncol = 3, top = "")
  # m1 = marrangeGrob(acc.plots1, nrow=3, ncol=3, list(top=NULL) )
  # m1 = grid.arrange(acc.plots1, nrow=3, ncol=3)
  # m1 = plot_grid(acc.plots1, nrow = 3, ncol = 3)
  ggsave("1.Quality_control/Performance_Data/Accuracy/grid_accuracy_plot_1.pdf", m1,width = 15, height = 10, units = "in")
  ggsave("1.Quality_control/Performance_Data/Accuracy/grid_accuracy_plot_2.pdf", m2,width = 15, height = 10, units = "in")
  
  # ggsave("m.pdf", m1, width = 15, height = 10, units = "in")
  
  
}




draw_Biographic_Data_plots(data)
draw_Trait_Psychometric_Data_plots()

draw_timing_plot(data)
draw_accuracy_plot()

draw_accuracy_plot_combined()
draw_timing_plot_combined()
draw_state_psychometric_data_combined()

draw_state_psychometric_data()

draw_accuracy_plot_grid()




