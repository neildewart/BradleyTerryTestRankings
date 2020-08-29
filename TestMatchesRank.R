library("BradleyTerry2")
library("reshape2")
library("ggplot2")

test<-read.csv("~/Test04to19.csv")
teamnames<-sort(unique(c(levels(test[,1]), levels(test[,2]))))

#Define Inverse Logit
inv_logit <- function(p) {exp(p) / (1 + exp(p))}


#Update data variable and define home and away variables
strDates <- c()
test$times <- as.Date(test$StartDate, "%d/%m/%Y")
test$Home<-data.frame(team=test$ï..Home,at.home=1)
test$Away<-data.frame(team=test$Away,at.home=0)

#Function that pulls all results from a given numbers of years leading up to a given date
yearend <- function(years,date){
  subset(test,times<=(as.Date(date)) & times>(as.Date(date))-years*365)
}


#Computes Bradley-Terry model for results from previous two years
yearBT <- function(date){
histBT<-BTm(outcome=Result,player1=Home,player2=Away,
formula=~team,id="team",data=yearend(2,date),refcat="England")
return(histBT)
}

#Function to use exponential decay to apply a weight to games played between 2 and 4 years prior to specified data. Weight is based on how recently the game was played so that more recent games are given greater importance in the ranking system
expBTdata <- function(date){
data<-(yearend(4,date))
temp <- c()
temp<-ifelse ((((data$times) < as.Date(date)-730) & data$Result==1),(temp <- exp(-0.0008*as.numeric(((as.Date(date)-730)-data$times)))),
ifelse  ((((data$times) < as.Date(date)-730) & data$Result==0),(temp <- 1-exp(-0.0008*as.numeric(((as.Date(date)-730)-data$times)))),
temp <- data$Result))

data$Result<-temp

histBT<-BTm(outcome=Result,player1=Home,player2=Away,
            formula=~team,id="team",data=data,refcat="England")
return(histBT)

}


#Normalising function with mean 100 and unit variance
std<-function(x){
  nteams <- (length(x[!is.na(x)]))
  if(length(which(is.na(x)))==0) (x-mean(x))*25+(105-nteams*sd(x)) else
  (x-mean(x,na.rm=T))*25+(105-nteams*sd(x,na.rm=T))
}

altstd<-function(x){if(length(which(is.na(x)))==0) (x-mean(x))*25+(100) else
 (x-mean(x,na.rm=T))*25+(100)
}

oldstd<-function(x){if(length(which(is.na(x)))==0) ((x-mean(x))/sd(x))*20+(100) else
  (x-mean(x,na.rm=T))/sd(x,na.rm=T)*20+(100)
}

#Function to exclude teams with fewer than 10 appearances
minqual <- function(apptable,BTvector){
for (i in 1:12) {
  if(apptable$Freq[i]<10) (BTvector[i]<-NA)
}
return (BTvector)

}



#Computes Rankings based on Bradley-Terry models for each of previous 2 year periods,
#with the most recent 2 year period having weighting of 1, and the former period 0.5
ranking <- function (date) {
date <- as.Date(date, "%d/%m/%Y")
#yearago2<-(as.Date(date)-730)

results_l4y<-data.frame((yearend(4,date)))
matches_l4y<-data.frame(table(results_l4y$Home)+table(results_l4y$Away))

prev3<-(data.frame(BTabilities(expBTdata(date)))$ability)
prev3<-minqual(matches_l4y,prev3)
rating <- std(prev3)
tratings<- data.frame(teamnames,rating)
rank<-c(1:length(teamnames))
rankings<- data.frame(prev3,rank,tratings[order(-rating),])

return(rankings)
}

ranking("31/03/2020")
