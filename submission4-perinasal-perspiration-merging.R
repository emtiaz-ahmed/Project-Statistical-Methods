#-------------------------#
#--------LIBRARIES--------#
#-------------------------#


#-------------------------#
#---FUNCTION DEFINITION---#
#-------------------------#
# file.path <- function(path1, path2) {
#   file.path(path1, path2)
# }

findFileAndSetPath <- function(file_dir, file_name_pattern) {
  file_name <- list.files(path=file_dir, pattern=file_name_pattern, recursive=F)
  file.path(file_dir, file_name)
}

convert_pp_to_csv <- function(df, file_path) {
  write.table(df, file = file_path, row.names=F, sep = ",")
}

downsample_using_mean <- function(df, col_names) {
  per_sec_df <- NULL
  if(nrow(df) <= 0) {
    return(NULL)
  }
  
  max_time <- floor(tail(df$Time, 1))
  if(!is.numeric(max_time) || length(max_time) == 0) {
    return(NULL)
  }
  
  for(cur_time in c(0 : max_time)) {   #Use minTime instead of 0
    one_sec_data = df[df$Time >= cur_time & df$Time < cur_time+1,]
    if(nrow(one_sec_data) > 0) {
      mean_values = colMeans(one_sec_data, na.rm = TRUE)
      data = data.frame("Time" = cur_time)
      for (column in col_names) {
        if(is.nan(mean_values[column])) {
          ################
          # val = NA/''
          ################
          val = 'NA'
        } else {
          val = mean_values[column]
        }
        d = data.frame(column = val, row.names = NULL)
        colnames(d) = c(column)
        data = cbind(data, d)
      }
      per_sec_df = rbind(per_sec_df, data)
    }
    else {
      data = data.frame("Time" = cur_time)
      for (column in col_names) {
        ################
        # val = NA/''
        ################
        val = 'NA'
        d = data.frame(column = val, row.names = NULL)
        colnames(d) = c(column)
        data = cbind(data, d)
      }
      per_sec_df = rbind(per_sec_df, data) 
    }
    # data = NULL
  }
  return(per_sec_df)
}

mergeDataForSessions <- function(subj_name, subj_dir, session_name, subj_df) {
  session_path <- file.path(subj_dir, session_name)
  file_list <- list.files(path=session_path, pattern=file_pattern, recursive=F)
  
  for (pattern_idx in 1:length(file_patterns)) {
    file_pattern <- file_patterns[pattern_idx]
    file_name <- file_list[grepl(file_pattern, file_list)]
    
    if(!identical(file_name, character(0))) {
      file_path <- findFileAndSetPath(session_path, file_pattern)
      df <- read.csv(file_path)[, 2:3]
      df <- downsample_using_mean(df, c("Perspiration"))
      df$Task <- rep(task_list[pattern_idx], nrow(df))
      df$Session <- rep(session_name, nrow(df))
      df$Subject <- rep(subj_name, nrow(df))
      
      subj_df <- rbind(subj_df, df)
    }
  }
  
  return(subj_df)
}

mergeDataForOneSub <- function() {
  subj_list <- list.dirs(path=data_dir, full.names=F, recursive=F)
  merged_df <- data.frame()
  
  ###########################################
  # TRYING TO USE APPLY INSTEAD OF FOR LOOP #
  ###########################################
  # sapply(subj_list, function(subj_name) {
  # merged_df <- sapply(subj_list[1], function(subj_name) {
  
  
  for(subj_idx in 1 : length(subj_list)) {
  # for(subj_idx in 1 : 3) {
    subj_name <- subj_list[subj_idx]
    subj_dir <- file.path(data_dir, subj_name)
    session_list <- list.dirs(path=subj_dir, full.names=F, recursive=F)

    
    ###########################################
    # TRYING TO USE APPLY INSTEAD OF FOR LOOP #
    ###########################################
    # subj_df <- data.frame()
    # subj_df <- sapply(session_list, function(session_name) {
    #   # sapply(session_list[1], function(session_name) {
    #   df <- mergeDataForSessions(subj_dir, session_name)
    #   subj_df <- rbind(subj_df, df)
    # })
    
    
    subj_df <- data.frame()
    for(sess_idx in 1 : length(session_list)) {
    # for(sess_idx in 1 : 2) {
      session_name <- session_list[sess_idx]
      session_path <- file.path(subj_dir, session_name)
      subj_df <- mergeDataForSessions(subj_name, subj_dir, session_name, subj_df)
    }
    
    merged_df <- rbind(merged_df, subj_df)
    
    # return(merged_df)
  # })
  }
    
  print(head(merged_df))
  merged_df <- merged_df[,c(1, 5, 4, 3, 2)]
  convert_pp_to_csv(merged_df, file.path(output_dir, "full_data_set.csv"))
}


#-------------------------#
#-------Main Program------#
#-------------------------#
current_dir <- dirname(rstudioapi::getSourceEditorContext()$path)

data_dir <- file.path(current_dir, 'Data')
output_dir <- file.path(current_dir, '3.Mean Data', 'Full Data Set')
setwd(data_dir)

file_pattern <- ".*.csv"
task_list <- c("Baseline", "Cutting", "Suturing")
file_patterns <- paste0(".*", task_list, "..csv")
# file_patterns <- c(".*Baseline..csv", ".*Cutting..csv", ".*Suturing..csv")

mergeDataForOneSub()

