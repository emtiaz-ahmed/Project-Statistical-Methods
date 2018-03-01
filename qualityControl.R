setwd("E:/Github/1.project/Project-Statistical-Methods")
#getwd()
library(ggplot2)
library(lubridate)
library(gridExtra)
library(grid)

#...read input file
data = read.csv("MicrosurgeryPerformance.csv", header = TRUE, sep=",")


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
    theme(plot.title = element_text(hjust=0.5)) +
    scale_y_continuous(breaks = seq(0,12,by=2), limits = c(0,12))
  
  ggsave(file="Quality_control/Biographic_data/gender_distribution.png", dpi = 600, width = 10, height = 8, units = "in")
  
  #...plot the histogram for age distribution
  ggplot(data, aes(x=data$Age)) + geom_histogram(col="black", fill="dodgerblue1", binwidth = 1) +
    labs(title="Histogram of Age distribution", x="Age", y="Count") +
    theme(plot.title = element_text(hjust=0.5)) +
    scale_x_continuous(breaks=c(22,23,24,25,26))
  
  ggsave(file="Quality_control/Biographic_data/age_distribution.png", dpi = 600, width = 10, height = 8, units = "in")
  
  
}

draw_Trait_Psychometric_Data_plots<-function(){
  # #...read all tai.csv files from all folders
  # files <- dir("input", recursive=TRUE, full.names=TRUE, pattern="*_tai.csv$")
  # #...find number of files
  # total.file = length(files)
  # 
  # temp = read.csv(files[1], header = FALSE)
  # #...list the questions
  # questions = temp$V1
  # #...read all the scores and create a list
  # flag1 = 1
  # tai.score = list()
  # for(i in files){
  #   trait.data = read.csv(i,header=FALSE)
  #   tai.score[flag1] = list(trait.data$V2)
  #   flag1 = flag1 + 1
  #   
  # }
  # #...create dataframe of all scores
  # trait.tai <-as.data.frame(matrix(unlist(tai.score), nrow=total.file, ncol=20, byrow=TRUE))
  # 
  # #...draw plots for each question
  # for(i in 1:ncol(trait.tai)){
  #   #...plot the histogram for tai scores
  #   ggplot(trait.tai, aes(x=trait.tai[i])) + geom_histogram(col="black", fill="dodgerblue1", binwidth=1) +
  #     labs(title="Histogram of tai scores", x="Score", y="Count") +
  #     theme(plot.title = element_text(hjust=0.5)) +
  #     scale_y_continuous(breaks = seq(0,21,by=2), limits = c(0,21)) +
  #     scale_x_continuous(breaks = seq(0,6,by=1), limits = c(0,6))
  #   
  #   outputFile = paste("Quality_control/Trait_Psychometric_data/",questions[i],".png")
  #   ggsave(file=outputFile, dpi = 600, width = 10, height = 8, units = "in")
    
  
  #}
  # #...read all tai.csv files from all folders
  # files <- dir("input", recursive=TRUE, full.names=TRUE, pattern="*_tp.csv$")
  # files
  # #...find number of files
  # total.file = length(files)
  # total.file
  # 
  # tai.score = list()
  # flag = 1
  # for(i in files){
  #   tai.data = read.csv(i, header = FALSE)
  #   tai.data
  #   tai.score[flag] = list(tai.data$V2[1])
  #   flag = flag + 1
  #   
  # }
  # tai.df = as.data.frame(unlist(tai.score))
  # tai.df
  # 
  # ggplot(tai.df, aes(tai.df$`unlist(tai.score)`)) +
  #   geom_histogram(col="black", fill="dodgerblue1", binwidth=1) +
  #   labs(title="Histogram of tai scores", x="Tai Score", y="Count") +
  #   theme(plot.title = element_text(hjust=0.5)) +
  #   scale_x_continuous(breaks = seq(24,56,by=2), limits = c(24,56))
  # 
  # outputFile = paste("Quality_control/Trait_Psychometric_data/",".png")
  # ggsave(file="Quality_control/Trait_Psychometric_data/Tai_score_histogram.png", dpi = 600, width = 10, height = 8, units = "in")
   
  data.tai = read.table("tai_scores.txt") 
  data.tai
  ggplot(data.tai, aes(data.tai$V2)) +
    geom_histogram(col="black", fill="dodgerblue1", binwidth=1) +
    labs(title="Histogram of tai scores", x="Tai Score", y="Count") +
    theme(plot.title = element_text(hjust=0.5)) +
    scale_x_continuous(breaks = seq(16,56,by=2), limits = c(16,56))
  
  outputFile = paste("Quality_control/Trait_Psychometric_data/",".png")
  ggsave(file="Quality_control/Trait_Psychometric_data/Tai_score_histogram.png", dpi = 600, width = 10, height = 8, units = "in")
  
  
  
}



#...draw the timing plots for each subject based on their cutting and suturing time
draw_timing_plot <- function(data){
  #...create dataframe using subject name and all cutting time
  cut.timing = data.frame(data$Subject, data$Cutting.Time.1, data$Cutting.Time.2, data$Cutting.Time.3, data$Cutting.Time.4, data$Cutting.Time.5)
  #...create dataframe using subject name and all suturing time
  sut.timing = data.frame(data$Subject, data$Suturing.Time.1 , data$Suturing.Time.2, data$Suturing.Time.3,
                                 data$Suturing.Time.4, data$Suturing.Time.5)
  #...find the number of rows
  number.of.row = nrow(cut.timing)
  
  for(i in 1:number.of.row){
    #...select specific row from the dataframe
    sub.timing.cut = cut.timing[i,]
    sub.timing.sut = sut.timing[i,] 
    #...find the subject number
    subject.name = sub.timing.cut$data.Subject
    
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
    barTitle = paste("Timing barplot of subject", subject.name)
    xLabel = paste("Subject", subject.name)
    
   
    ggplot(subject.timing, aes(x=label, y=timing)) + geom_bar(aes(fill=tasks), position = "dodge", stat="identity") +
      labs(title = barTitle, x = xLabel, y = "Time") +
      theme(plot.title = element_text(hjust=0.5)) +
      scale_y_continuous(breaks = seq(0,20,by=4), limits = c(0,20))
    
    outputFile = paste("Quality_control/Performance_Data/Timing/timing_plot_of_subject_",subject.name,".png")
    #...save the output files
    ggsave(file = outputFile, dpi = 600, width = 10, height = 8, units = "in")
  }
  
  
  
}

#...draw the accuracy plot for each subject based on their score
# draw_accuracy_plot <- function(data){
#   #...create dataframe using subject name and all scores
#   score = data.frame(data$Subject,data$Score.1, data$Score.2, data$Score.3, data$Score.4, data$Score.5)
#   score
#   #...find the number of rows
#   number.of.row = nrow(score)
#   
#   for(i in 1:number.of.row){
#     #...select specific row from the dataframe
#     sub.score = score[i,] 
#     #...find the subject number
#     subject.name = sub.score$data.Subject
#     
#     #...create x axis ticks
#     label = c("Session 1", "Session 2", "Session 3", "Session 4", "Session 5")
#     #...copy only the scores
#     performance = c(sub.score$data.Score.1, sub.score$data.Score.2, sub.score$data.Score.3, sub.score$data.Score.4, sub.score$data.Score.5)
#     
#     #...create dataframe using label and performance
#     subject.performance = data.frame(label, performance)
#     
#     #...concate string to create title
#     barTitle = paste("Accuracy barplot of subject", subject.name)
#     xLabel = paste("Subject", subject.name)
#     
#     #...plot the graph
#     ggplot(subject.performance, aes(x = label, y = performance)) + geom_bar(stat="identity", col = "black", fill="dodgerblue1") +
#       labs(title = barTitle, x = xLabel, y = "Score") +
#       theme(plot.title = element_text(hjust=0.5)) + 
#       scale_y_continuous(breaks = seq(0,30,by=5), limits = c(0,30))
#     
#     outputFile = paste("Quality_control/Performance_Data/Accuracy/accuracy_plot_of_subject_",subject.name,".png")
#     #...save the output files
#     ggsave(file = outputFile, dpi = 600, width = 10, height = 8, units = "in")
#   }
#   
#   
#   
# }

#...draw the accuracy plot for each subject based on their score
draw_accuracy_plot <- function(data){
  # #...create dataframe using subject name and all scores
  score = data.frame(data$Subject,data$Score.1, data$Score.2, data$Score.3, data$Score.4, data$Score.5,
                     data$Sutures.1, data$Sutures.2, data$Sutures.3, data$Sutures.4, data$Sutures.5)
  score
  #...find the number of rows
  number.of.row = nrow(score)

  for(i in 1:number.of.row){
    #...select specific row from the dataframe
    sub.score = score[i,]
    #...find the subject number
    subject.name = sub.score$data.Subject

    #...create x axis ticks
    label = c(rep("Session 1",2), rep("Session 2",2), rep("Session 3",2), rep("Session 4",2),
              rep("Session 5",2))
    #... task label
    task = c(rep(c("Cutting Score", "Suturing Score"),5))
    
    #...copy only the scores
    performance = c(sub.score$data.Score.1, sub.score$data.Sutures.1, sub.score$data.Score.2,
                    sub.score$data.Sutures.2, sub.score$data.Score.3, sub.score$data.Sutures.3,
                    sub.score$data.Score.4, sub.score$data.Sutures.4, sub.score$data.Score.5,
                    sub.score$data.Sutures.5)
    
    performance

    #...create dataframe using label, task and performance
    subject.performance = data.frame(label, task, performance)
    subject.performance

    #...concate string to create title
    barTitle = paste("Accuracy barplot of subject", subject.name)
    xLabel = paste("Subject", subject.name)

    #...plot the graph
    # ggplot(subject.performance, aes(x = label, y = performance)) + geom_bar(stat="identity", col = "black", fill="dodgerblue1") +
    #   labs(title = barTitle, x = xLabel, y = "Score") +
    #   theme(plot.title = element_text(hjust=0.5)) +
    #   scale_y_continuous(breaks = seq(0,30,by=5), limits = c(0,30))
    
    ggplot(subject.performance, aes(x = label, y = performance)) + geom_bar(aes(fill=task), position = "dodge", stat="identity", width = 0.8, col="black") +
      labs(title = barTitle, x = xLabel, y = "Score") +
      theme(plot.title = element_text(hjust=0.5)) +
      labs(fill = "Tasks") +
      scale_y_continuous(breaks = seq(0,30,by=2), limits = c(0,30))
  

    outputFile = paste("Quality_control/Performance_Data/Accuracy/accuracy_plot_of_subject_",subject.name,".png")
    #...save the output files
    ggsave(file = outputFile, dpi = 600, width = 10, height = 8, units = "in")
  }
  
  
  
}


draw_state_psychometric_data<-function(){
  totalSubject = list.dirs(path = "input", full.names = TRUE, recursive = FALSE)
  totalSubject
  flagg = 1
  
  for(j in totalSubject){
    # dir.name = paste("input/", "subject01" )
    # dir.name
    #...read all tai.csv files from all folders
    # subject.files <- dir("input/subject21", recursive=TRUE, full.names=TRUE, pattern="*_NASA.csv$")
    subject.files <- dir(j, recursive=TRUE, full.names=TRUE, pattern="*_NASA.csv$")
    subject.file.names <- list.dirs(path = "input", full.names = FALSE, recursive = FALSE)
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
      labs(title = barTitle, x = xLabel, y = "Time") +
      theme(plot.title = element_text(hjust=0.5)) +
      labs(fill = "Response") +
      scale_y_continuous(breaks = seq(0,20,by=4), limits = c(0,20)) +
      scale_fill_manual(values = c("dodgerblue","plum","gold","deeppink1","springgreen","gray33"))
    
    outputFile = paste("Quality_control/State_Psychometric_Data/Cutting/nasa_plot_of_subject_",subject.name,".png")
    #...save the output files
    ggsave(file = outputFile, dpi = 600, width = 10, height = 8, units = "in")
    ggplot(nasa.data, aes(x=nasa.label, y=nasa.suturing)) + geom_bar(aes(fill=nasa.response), position = "dodge", stat="identity", width = 0.8, col="black") +
      labs(title = barTitle, x = xLabel, y = "Time") +
      theme(plot.title = element_text(hjust=0.5)) +
      labs(fill = "Response") +
      scale_y_continuous(breaks = seq(0,20,by=4), limits = c(0,20)) +
      scale_fill_manual(values = c("dodgerblue","plum","gold","deeppink1","springgreen","gray33"))
    
    outputFile = paste("Quality_control/State_Psychometric_Data/Suturing/nasa_plot_of_subject_",subject.name,".png")
    #...save the output files
    ggsave(file = outputFile, dpi = 600, width = 10, height = 8, units = "in")
    
    # if(flagg == 5){
    #   break
    # }
    flagg = flagg + 1
  }
  
  
}



# draw_perinasal_Perspiration_Signal_Data<-function(){
#   folder.list = list.dirs(path = "input", full.names = TRUE, recursive = FALSE)
#   folder.list
#   
#   for(folderName in folder.list){
#     # subFolder.list = list.dirs(path = folderName, full.names = TRUE, recursive = FALSE)
#     subFolder.list = list.dirs(path = "input/subject01", full.names = TRUE, recursive = FALSE)
#     subFolder.list
#     
#     allPlot.list = list()
#     pflag = 1
#     for(subFolderName in subFolder.list){
#       # subFolderNam = "input/subject01/session2" 
#       baseline.file = dir(subFolderName, recursive=FALSE, full.names=TRUE, pattern="*Baseline.*.csv$")
#       cutting.file = dir(subFolderName, recursive=FALSE, full.names=TRUE, pattern="*Cutting[0-9]*.csv$")
#       suturing.file = dir(subFolderName, recursive=FALSE, full.names=TRUE, pattern="*Suturing[0-9]*.csv$")
#       
#       # baseline.file = dir("input/subject01/session2", recursive=FALSE, full.names=TRUE, pattern="*Baseline.*.csv$")
#       # cutting.file = dir("input/subject01/session2", recursive=FALSE, full.names=TRUE, pattern="*Cutting[0-9]*.csv$")
#       # suturing.file = dir("input/subject01/session2", recursive=FALSE, full.names=TRUE, pattern="*Suturing[0-9]*.csv$")
#       baseline.file
#       cutting.file
#       suturing.file
#       baseline.data = read.csv(baseline.file)
#       baseline.data
#       cuting.data = read.csv(cutting.file)
#       cuting.data
#       suturing.data = read.csv(suturing.file)
#       suturing.data
#       
#       p = ggplot() + geom_line(data=baseline.data, aes(baseline.data$Time, baseline.data$Perspiration), col="black") +
#         geom_line(data=cuting.data, aes(cuting.data$Time, cuting.data$Perspiration), col="green") +
#         geom_line(data=suturing.data, aes(suturing.data$Time, suturing.data$Perspiration), col="red") +
#         labs(x="Time", y="Perspiration")
#       
#       p
#       allPlot.list[[pflag]] = p
#       
#       pflag = pflag + 1
#       
#     }
#     
#     # allPlot.list
#     
#     n <- length(allPlot.list)
#     nCol <- floor(sqrt(n))
#     
#     png("Quality_control/Perinasal_Perspiration_(Stress)_Signal_Data/s1.png")
#     do.call(grid.arrange, c(allPlot.list, list(ncol=nCol)))
#     # grid.arrange(grobs=allPlot.list, ncol=nCol)
#     # outputFile = paste("Quality_control/State_Psychometric_Data/Suturing/nasa_plot_of_subject_",subject.name,".png")
#     #...save the output files
#     dev.off()
#   }
#   
# }




draw_Biographic_Data_plots(data)
draw_Trait_Psychometric_Data_plots()

draw_timing_plot(data)
draw_accuracy_plot(data)

draw_state_psychometric_data()

# draw_perinasal_Perspiration_Signal_Data()


