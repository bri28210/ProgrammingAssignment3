Init <- function(fileStr, workDirStr="C:/Users/barberon-br/datasciencecoursera/datasciencecoursera/")
{
  setwd(workDirStr)
  retDfr <- read.csv(fileStr, colClasses = "character")
  return(retDfr)
}


rankhospital <- function(state, outcome,num = "best") {
  
  outcomeHosp <- Init("outcome-of-care-measures.csv")
  #--- Coerce character into numeric
  suppressWarnings( outcomeHosp[, 11] <- as.numeric(outcomeHosp[, 11]) )
  suppressWarnings( outcomeHosp[, 17] <- as.numeric(outcomeHosp[, 17]) )
  suppressWarnings( outcomeHosp[, 23] <- as.numeric(outcomeHosp[, 23]) )  
  ## Create a data frame with freq per state for testing the correct input values
  ## Remove row names
  
  tableHosp<-data.frame(State=names(tapply(outcomeHosp$State,outcomeHosp$State,length)), 
                        Freq=tapply(outcomeHosp$State,outcomeHosp$State,length))
  rownames(tableHosp) <- NULL
  ## Init a data frame with col number and type of outcome
  agreedData<-data.frame(Outcome=c("heart attack","heart failure","pneumonia"),Col=c(11,17,23))
  outcomeIndex <- switch(outcome, `heart attack` = 11, `heart failure` = 17, `pneumonia` = 23)
  
 
  
  ## Check that state and outcome are valid
  if( nrow(tableHosp[tableHosp$State==state,]) == 0 ) stop( "invalid state" )
  if( nrow(agreedData[agreedData$Outcome==outcome,]) == 0 ) stop( "invalid outcome" )
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  # Return hospital name in the state with the given rank 30-day death rate
  hospitalNameAndRate <- outcomeHosp[outcomeHosp$State == state, c(2, outcomeIndex)]
  hospitalNameAndRate[,2] <- as.numeric(as.character(hospitalNameAndRate[,2]))
  hospitalNameAndRate <- hospitalNameAndRate[!is.na(hospitalNameAndRate[,2]),]
  
  sortedNameAndRate <- hospitalNameAndRate[ order(hospitalNameAndRate[,2], hospitalNameAndRate[,1]), ]
  
  result <-
    if(num == "best")
      sortedNameAndRate[1,1]
  else if(num == "worst")
    sortedNameAndRate[nrow(sortedNameAndRate),1]
  else if(as.numeric(num) <= nrow(sortedNameAndRate))
    sortedNameAndRate[as.numeric(num),1]
  else
    NA
  
  as.vector(result)
  
}