setwd("E:/Github/1.project/Project-Statistical-Methods")
#getwd()
library(ggplot2)
library(lubridate)

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
  #...read all tai.csv files from all folders
  files <- dir("input", recursive=TRUE, full.names=TRUE, pattern="*_tai.csv$")
  #...find number of files
  total.file = length(files)
  
  temp = read.csv(files[1], header = FALSE)
  #...list the questions
  questions = temp$V1
  #...read all the scores and create a list
  flag1 = 1
  tai.score = list()
  for(i in files){
    trait.data = read.csv(i,header=FALSE)
    tai.score[flag1] = list(trait.data$V2)
    flag1 = flag1 + 1
    
  }
  #...create dataframe of all scores
  trait.tai <-as.data.frame(matrix(unlist(tai.score), nrow=total.file, ncol=20, byrow=TRUE))
  
  #...draw plots for each question
  for(i in 1:ncol(trait.tai)){
    #...plot the histogram for tai scores
    ggplot(trait.tai, aes(x=trait.tai[i])) + geom_histogram(col="black", fill="dodgerblue1", binwidth=1) +
      labs(title="Histogram of tai scores", x="Score", y="Count") +
      theme(plot.title = element_text(hjust=0.5)) +
      scale_y_continuous(breaks = seq(0,21,by=2), limits = c(0,21)) +
      scale_x_continuous(breaks = seq(0,6,by=1), limits = c(0,6))
    
    outputFile = paste("Quality_control/Trait_Psychometric_data/",questions[i],".png")
    ggsave(file=outputFile, dpi = 600, width = 10, height = 8, units = "in")
    
    
  }
}



draw_Biographic_Data_plots(data)
draw_Trait_Psychometric_Data_plots()
