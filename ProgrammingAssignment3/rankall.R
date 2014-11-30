Init <- function(fileStr, workDirStr="C:/Users/barberon-br/datasciencecoursera/datasciencecoursera/")
{
  setwd(workDirStr)
  retDfr <- read.csv(fileStr, stringsAsFactors = FALSE)
  return(retDfr)
}


rankall <- function(outcome,num = "best") {
  
  outcomeHosp <- Init("outcome-of-care-measures.csv")
  #--- Coerce character into numeric
  suppressWarnings( outcomeHosp[, 11] <- as.numeric(outcomeHosp[, 11]) )
  suppressWarnings( outcomeHosp[, 17] <- as.numeric(outcomeHosp[, 17]) )
  suppressWarnings( outcomeHosp[, 23] <- as.numeric(outcomeHosp[, 23]) )  
  ## Create a data frame with freq per state for testing the correct input values
  ## Remove row names
  
  outcomeIndex <- switch(outcome, `heart attack` = 11, `heart failure` = 17, `pneumonia` = 23)
  if( is.null(outcomeIndex ) ){
    stop("invalid outcome")
  }
  
  i_num <- num
  
  # For each state, find the hospital of the given rank
  ## build an empty data frame as we don't know
  ## how many records will be required so pre-allocating
  ## space is problamatical
  
  ## For each state, find the hospital of the given rank
  result <- matrix(NA,0,2)
  for(state in unique(outcomeHosp[,7]))
    result<-rbind(result,c(rankhospital(state,outcome,num),state))
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  result[order(result[,2]),]
  colnames(result)=c("hospital", "state")
  rownames(result)=result[,2]
  as.data.frame(result)
  
}