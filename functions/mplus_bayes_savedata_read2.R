


mplus_bayes_savedata_read2 <- function(model_int, 
                                      model_link,
                                      savedata = TRUE){
  
  test <- readModels(str_c(model_link,
                           model_int, 
                           ".out"))
  
  name_savedata <- test$savedata_info$fileName
  name_posterior <- test$savedata_info$bayesFile
  
  test <- test$savedata_info$fileVarNames
  
  
  #
  rep_name <- str_extract(test, "\\+[a-zA-Z_0-9]*")
  
  rep_name <- rep_name[!is.na(rep_name)]
  #
  #
  test <- str_replace(test, "F10.3", "")
  test <- str_replace_all(test, " ", "")
  #
  for(i in seq_along(rep_name)){
    test <- str_replace(test, paste0("\\+", rep_name[i]),
                        paste0(rep_name[i], "_p", 1:20, collapse = "\n"))
  }
  
  test <- read_delim(paste0(test, collapse = "\n"),
                     "\n", col_names = F)
  
  
  
  # # import savedata
  mplus_savedata <- read.fwf(str_c(model_link,
                                   name_savedata),
                             widths = rep(10, nrow(test)),
                             col.names = test$X1,
                             na.strings = "         *", 
                             stringsAsFactors = F)
  
  if (savedata == TRUE) {
    save(mplus_savedata, file = 
           str_c(model_link, model_int, "_savedata.RData"))
  }
  
  mplus_savedata
}