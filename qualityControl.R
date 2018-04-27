setwd("E:/Github/1.project/Project-Statistical-Methods")

library(ggplot2)
library(lubridate)
library(gridExtra)
library(grid)

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
  
  ggsave(file="1.Quality_control/Biographic_data/gender_distribution.png", dpi = 600, width = 10, height = 8, units = "in")
  
  #...plot the histogram for age distribution
  ggplot(data, aes(x=data$Age)) + geom_histogram(col="black", fill="dodgerblue1", binwidth = 1) +
    labs(title="Histogram of Age distribution", x="Age", y="Count") +
    scale_x_continuous(breaks=c(22,23,24,25,26))+
    theme_bw() +
    geom_text(stat='count', aes(label=..count..), vjust=0, nudge_y = 0.1) +
    theme(plot.title = element_text(hjust=0.5))
    
  
  ggsave(file="1.Quality_control/Biographic_data/age_distribution.png", dpi = 600, width = 10, height = 8, units = "in")

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
  ggsave(file="1.Quality_control/Trait_Psychometric_data/Tai_score_histogram.png", dpi = 600, width = 10, height = 8, units = "in")

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


#...draw the accuracy plot for each subject based on their score
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
  ggsave(file = outputFile, dpi = 600, width = 10, height = 8, units = "in")
  
  
  
  
}




draw_Biographic_Data_plots(data)
draw_Trait_Psychometric_Data_plots()

draw_timing_plot(data)
draw_accuracy_plot()

draw_accuracy_plot_combined()

draw_state_psychometric_data()




