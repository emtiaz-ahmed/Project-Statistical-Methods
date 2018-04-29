#-------------------------#
#--------LIBRARIES--------#
#-------------------------#
library(ggplot2)
library(dplyr)

#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
findFileAndSetPath <- function(file_dir, file_name_pattern) {
  file_name <- list.files(path=file_dir, pattern=file_name_pattern, recursive=F)
  file.path(file_dir, file_name)
}

convert_pp_to_csv <- function(df, file_path) {
  write.table(df, file = file_path, row.names=F, sep = ",")
}





boxplotForTasks <- function() {
  data_file_path <- file.path(current_dir, '3.Mean Data', 'Full Data Set', 'full_data_set.csv')
  
  df <- read.csv(data_file_path)
  print(head(df, 2))
  
  df_cutting <- df %>% filter(Task == 'Cutting')
  df_suturing <- df %>% filter(Task == 'Suturing')
  
  print(nrow(df_suturing))
  
  ggplot(aes(y = Perspiration,
             # x = interaction(Session, Task)),
             x = Session),
         data = df_cutting) +
    geom_boxplot()
  
  # ggplot(aes(y = Perspiration, 
  #            # x = interaction(Session, Task)), 
  #            x = Session), 
  #        data = df_suturing) + 
  #   geom_boxplot()
}






saveImage <- function(image_path, plot) {
  ggsave(image_path, plot, width=12, height=9)
}

drawBoxPlot <- function(df, task_name, image_extension='') {
  df <- df %>% filter(Task == task_name)
  
  plot <- ggplot(aes(y = Base.Mean.PP,
             x = Session),
         data = df) +
    geom_boxplot() +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) +
    labs(title=paste("Perinasal Perspiration Comparison For", task_name)) +
    ylab(expression(paste("Perinasal Perspiration [",""^"o","C",""^2,"]"))) +
    scale_x_discrete(labels = c("1", "2", "3", "4", "5")) +
    stat_summary(fun.y=mean, 
                 colour="red", 
                 geom="point",
                 shape=8, 
                 size=3, 
                 show.legend=F)
  
  ## SAVING PLOT
  image_path <- file.path(output_dir, paste0(task_name, "_Boxplot", image_extension, ".pdf"))
  saveImage(image_path, plot)
}


testAnova <- function(df, task_name) {
  df <- df %>% filter(Task == task_name)
  fit <- aov(Base.Mean.PP ~ Session, data=df)
  print(paste0("Showing anova significance for ", task_name, "........"))
  print(summary(fit))
  print(TukeyHSD(fit))
}


boxplotForTasksMeanPP <- function() {
  data_file_path <- file.path(current_dir, '4.HypothesisData', 'hyp_test_4.csv')
  df <- read.csv(data_file_path)

  
  # drawBoxPlot(df, task_cutting)
  # drawBoxPlot(df, task_suturing)

  
  # df_min <- df %>% filter(Task == task_name) %>% group_by(Session) %>% slice(which.min(Base.Mean.PP))
  # df_max <- df %>% filter(Task == task_name) %>% group_by(Session) %>% slice(which.max(Base.Mean.PP))
  

  df <- df[!(df$Id=="S01-S5-Cut"),]
  df <- df[!(df$Id=="S07-S3-Sut"),]
  #### df <- df[!(df$Id=="S01-S4-Sut"),]
  

  # drawBoxPlot(df, task_cutting, img_extension)
  # drawBoxPlot(df, task_suturing, img_extension)

  
  testAnova(df, task_cutting)
  testAnova(df, task_suturing)
}




#-------------------------#
#-------Main Program------#
#-------------------------#
current_dir <- dirname(rstudioapi::getSourceEditorContext()$path)
output_dir <- file.path(current_dir, '5.FinalGraphs')


img_extension <- '_Without_Outliers'
task_cutting <- 'Cutting'
task_suturing <- 'Suturing'

# boxplotForTasks()
boxplotForTasksMeanPP()

