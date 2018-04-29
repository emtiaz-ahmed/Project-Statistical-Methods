###################################
###         Add Accuracy       ###
##################################



#-------------------------#
#--------LIBRARIES--------#
#-------------------------#
# install.packages("here")

library(dplyr)
library(here)


#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
concatePath <- function(path1, path2) {
  file.path(path1, path2)
}

findFileAndSetPath <- function(file_dir, file_name_pattern) {
  file_name <- list.files(path=file_dir, pattern=file_name_pattern, recursive=FALSE)
  concatePath(file_dir, file_name)
}

getMean <- function(file_dir, file_name_pattern) {
  file_name <- list.files(path=file_dir, pattern=file_name_pattern, recursive=FALSE)
  file_path <- concatePath(file_dir, file_name)
  
  if(identical(file_path, character(0))) {
    return('')
  } else {
    df <- read.csv(file_path)
    # return(mean(df[!is.na(df$Perspiration)]))
    print(mean(df$Perspiration))
    return(mean(df$Perspiration))
  }
  
}

addRow <- function(df, task_name, mean_val, subj_idx, sess_idx) {
  df <- rbind(df, data.frame(
                    "Id" = paste0("S", subj_idx, "-S", sess_idx, "-", substr(task_name, 1, 3)),
                    "Subject" = paste0("Suject", subj_idx),
                    "Session" = paste0("Session", sess_idx),
                    "Task" = task_name,
                    "Mean PP" = mean_val,
                    stringsAsFactors = F
                  )
              )
  return(df)
}

convertToCamelCase <- function(x){
  capit <- function(x) paste0(toupper(substring(x, 1, 1)), substring(x, 2, nchar(x)))
  sapply(strsplit(x, "\\."), function(x) paste(capit(x), collapse=""))
}

# create_subj_dir_mean <- function(subj_name) {
#   subj_dir_mean <- file.path(hyp_dir, subj_name)
#   
#   if (!file.exists(subj_dir_mean)){
#     dir.create(subj_dir_mean)
#   }
# }

convert_to_csv <- function(hyp_df, subj_name) {
  subj_path_mean <- paste0(file.path(hyp_dir, subj_name), '.csv')
  write.csv(hyp_df, file=subj_path_mean, row.names = F)
}

#-------------------------#
#-------Main Program------#
#-------------------------#
current_dir <- here()
data_dir <- concatePath(current_dir,  "Data")
hyp_dir <- concatePath(current_dir,  "4.HypothesisData")

# hyp_dir <- dir.create(file.path(current_dir, "4.HypothesisData"), showWarnings = FALSE)

score_data <- read.csv(concatePath(data_dir, 'MicrosurgeryPerformance.csv'))
subj_id_list <- score_data$ID


setwd(data_dir)
getwd()

baseline_file_pattern <- ".*Baseline..csv"
cutting_file_pattern <- ".*Cutting..csv"
suturing_file_pattern <- ".*Suturing..csv"

#USE APPLY FAMILY
?list.dirs
subj_list <- list.dirs(path=data_dir, full.names=FALSE, recursive=FALSE)
subj_list <- subj_list[ grepl("subject.*", subj_list) ]

## INITIALIZING EMPTY DATAFRAME ##
hyp_df <- data.frame(
  "Id" = character(0),
  "Subject" = character(0),
  "Session" = character(0),
  "Task" = character(0),
  "Mean PP" = integer(0),
  check.names = F
)


for(subj_idx in 1 : length(subj_list)) {
# for(subj_idx in 1 : 2) {
  
  subj_name <- subj_list[subj_idx]
  subj_id <- substr(subj_name, 8, 9)
  print(as.numeric(subj_id))
  
  if(is.element(as.numeric(subj_id), subj_id_list)) {
    
    subj_dir <- file.path(data_dir, subj_name)
    session_list <- list.dirs(path=subj_dir, full.names=FALSE, recursive=FALSE)
    
    
    for(sess_idx in 1 : length(session_list)) {
      # for(sess_idx in 1 : 1) {
      
      session_name <- session_list[sess_idx]
      session_id <- substr(session_name, 8, 8)
      session_path <- file.path(subj_name, session_name)
      
      baseline_mean <- getMean(session_path, baseline_file_pattern)
      cutting_mean <- getMean(session_path, cutting_file_pattern)
      suturing_mean <- getMean(session_path, suturing_file_pattern)
      
      if (is.numeric(baseline_mean)) {
        if (is.numeric(cutting_mean)) {
          cutting_mean <- baseline_mean - cutting_mean
        }
        if (is.numeric(suturing_mean)) {
          suturing_mean <- baseline_mean - suturing_mean
        }
      }
      
      
      # cutting_mean <- getMean(session_path, cutting_file_pattern)
      # suturing_mean <- getMean(session_path, suturing_file_pattern)
      
      ## APPENDING EACH ROW FOR A SESSION ##
      hyp_df <- addRow(hyp_df, 'Cutting', cutting_mean, subj_id, session_id)
      hyp_df <- addRow(hyp_df, 'Suturing', suturing_mean, subj_id, session_id)
      
      rm(cutting_mean, suturing_mean)
    }
    
  }
}

convert_to_csv(hyp_df, "hyp_test_3_final")


