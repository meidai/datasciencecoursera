rankall <- function(outcome, num = "best"){
  df <- read.csv("outcome-of-care-measures.csv",colClasses="character") 
  diseases <- c("heart attack","heart failure","pneumonia")
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
  states <- levels(factor(df2[,2]))
  states <- c(states)
  o <- order(df2[,1])
  df3 <- df2[o,]
  o2  <- order(df3[,3])
  df4 <- df3[o2,]
  num_hospital <- as.character()
  for ( i in 1:length(states)){
    df5 <- df4[df4[,2]==states[i],]
  if (num == "best") {
    num_hospital[i] <- as.character(df5$name[1]) } 
    
  else if (num == "worst") {
    num_hospital[i] <- as.character(df5$name[length(df5$name)]) } 
    
  else if (num > length(df5$name)){
    num_hospital[i] <- NA}
    
  else {
    num_hospital[i] <- as.character(df5$name[num]) } }
    
  df6 <- data.frame(num_hospital,states,row.names = states,stringsAsFactors=FALSE)
  names(df6) <- c("hospital","state")
  df6 }

