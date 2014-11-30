Init <- function(fileStr, workDirStr="C:/Users/barberon-br/datasciencecoursera/datasciencecoursera/")
{
  setwd(workDirStr)
  retDfr <- read.csv(fileStr, colClasses = "character")
  return(retDfr)
}

best <- function(state, outcome) {
  
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
  
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  stateHosp <- outcomeHosp[outcomeHosp$State==state,]
  colNum <- agreedData[agreedData$Outcome==outcome, 2]
  rowNum <- which.min(stateHosp[, colNum])
  return( stateHosp[rowNum, ]$Hospital.Name )
}
