best <- function(state, outcome) {
  
  df <- read.csv("outcome-of-care-measures.csv",colClasses="character") 
  
  states <- df[,7]
  diseases <- c("heart attack","heart failure","pneumonia")
  
  if (! state %in% states ){
    stop("invalid state")
  }
  if (! outcome %in% diseases){
    stop("invalid outcome")
  }
  
if (outcome =="heart attack"){    
  name <- as.factor(df[df[,11]!="Not Available",2])
  sta <- as.factor(df[df[,11]!="Not Available",7])
  mor <- as.numeric(df[df[,11]!="Not Available",11])
  }
  
else if (outcome =="heart failure"){    
  name <- as.factor(df[df[,17]!="Not Available",2])
  sta <- as.factor(df[df[,17]!="Not Available",7])
  mor <- as.numeric(df[df[,17]!="Not Available",17])
 }
else{    
  
  name <- as.factor(df[df[,23]!="Not Available",2])
  sta <- as.factor(df[df[,23]!="Not Available",7])
  mor <- as.numeric(df[df[,23]!="Not Available",23])
 }
  df2 <- data.frame(name,sta,mor)
  df3 <- df2[df2[,2]==state,]
  o <- order(df3[,3])
  df4 <- df3[o,]
  best_hospital <- as.character(df4$name[1])
  best_hospital
  }

    