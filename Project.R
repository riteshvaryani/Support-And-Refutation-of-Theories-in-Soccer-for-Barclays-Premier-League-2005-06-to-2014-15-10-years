##############################################################################################################
MASTERS PROJECT: ANALYSIS OF BARCLAYS PREMIER LEAGUE SOCCER DATA - 2005-06 TO 2014-2015(10 YEARS)- RITESH VARYANI
##############################################################################################################
#Data-preprocessing and Data Loading- COMPLETE

OrigTable = read.csv( file = "D:/UCLA courses/Masters Project/Project 2/MainData.csv", header=TRUE )
head(OrigTable)

years=c("2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015")
months=c("8","9","10","11","12","1","2","3","4","5")
seasons=c("1","2","3","4","5","6","7","8","9","10")

totalTeams=levels(OrigTable$HomeTeam)
totalReferrees=unique(OrigTable$Referee)

totalMatchesPlayed=c()
for(i in 1:length(totalTeams)) {
  totalMatchesPlayed=c(totalMatchesPlayed,nrow(OrigTable[(OrigTable[,4]==totalTeams[i] | OrigTable[,5]==totalTeams[i]),]))
}
totalMatchesPlayed
##############################################################################################################

#SUB_MODULE 1- COMPLETE
#Find the consistent teams- temas which have played all seasons of the league
consistentTeams=c()
consistentTeams=totalTeams[totalMatchesPlayed==380]
##############################################################################################################

#SUB_MODULE 1.a- COMPLETE
#Highest goals across all seasons for 8 teams

homeTeamGoalsByYears=c()
awayTeamGoalsByYears=c()
firstHalf=c(8,9,10,11,12)
secondHalf=c(1,2,3,4,5)

for(i in 1:length(seasons)) {
  for(j in 1:length(totalTeams)) {
    homeTeamGoalsByYears=c(homeTeamGoalsByYears,
                           sum(OrigTable[(OrigTable$HomeTeam==totalTeams[j] & OrigTable$Season==seasons[i]),6]))
  }
}

for(i in 1:length(seasons)) {
  for(j in 1:length(totalTeams)) {
    awayTeamGoalsByYears=c(awayTeamGoalsByYears,
                           sum(OrigTable[(OrigTable$AwayTeam==totalTeams[j] & OrigTable$Season==seasons[i]),7]))
  }
}

totalGoals=homeTeamGoalsByYears+awayTeamGoalsByYears
totGoals=matrix(totalGoals,nrow=36,ncol=10,byrow=F,dimnames=list(totalTeams,seasons))

totGoalsConsistent=totGoals[consistentTeams,]
totalScoredGoals=rowSums(totGoalsConsistent)

plot.new()
color=rainbow(length(consistentTeams))
plot(1:10,totGoals[1,1:10],type="b",xlim=c(1,12),ylim=c(25,110),col=color[1],xlab='Seasons',ylab='Goals Scored',main='Number of Goals Scored by Consistent Teams in Ten Seasons')
for(i in 2:length(consistentTeams))
  lines(1:10,totGoals[which(totalTeams==consistentTeams[i]),1:10],type="b",col=color[i])
legend(10, 110,c(consistentTeams),col=color, cex=1,lty=c(1,1),lwd=c(2.5,2.5),bty="n")
##############################################################################################################

#SUB_MODULE 2- COMPLETE
#resilient teams..lose at half tm=ime and win at full time

homeResilient=c()
for(i in 1:length(totalTeams))
  homeResilient=c(homeResilient,nrow(OrigTable[OrigTable[,4]==totalTeams[i] & OrigTable[,8]=="H" & OrigTable[,11]=="A",]))
homeResilient=array(homeResilient)
dimnames(homeResilient)=list(totalTeams)

awayResilient=c()
for(i in 1:length(totalTeams))
  awayResilient=c(awayResilient,nrow(OrigTable[OrigTable[,5]==totalTeams[i] & OrigTable[,8]=="A" & OrigTable[,11]=="H",]))
awayResilient=array(awayResilient)
dimnames(awayResilient)=list(totalTeams)

totalResilient=homeResilient+awayResilient
totalResilient

dotchart(as.numeric(totalResilient),labels=row.names(totalResilient),cex=.9,
         main="Resilient Score of the Teams throughout the 10 Seasons(2005-06 to 2014-15)", 
         xlab="Matches Won from Losing Position",col="red",lcolor="blue")
##############################################################################################################

#SUB_MODULE 3- COMPLETE
#Discipline Record

yellowCardByTeam=c()
redCardByTeam=c()
for(i in 1:length(totalTeams)) {
  homeSum=sum(OrigTable[OrigTable$HomeTeam==totalTeams[i],21])
  awaySum=sum(OrigTable[OrigTable$AwayTeam==totalTeams[i],22])
  yellowCardByTeam=c(yellowCardByTeam, homeSum+awaySum)
}
for(i in 1:length(totalTeams)) {
  homeSum=sum(OrigTable[OrigTable$HomeTeam==totalTeams[i],23])
  awaySum=sum(OrigTable[OrigTable$AwayTeam==totalTeams[i],24])
  redCardByTeam=c(redCardByTeam, homeSum+awaySum)
}
redCardTeam=array(redCardByTeam)
dimnames(redCardTeam)=list(totalTeams)
yellowCardTeam=array(yellowCardByTeam)
dimnames(yellowCardTeam)=list(totalTeams)

consistentTeamYellowCard=yellowCardTeam[c(consistentTeams)]
#Arsenal Aston Villa     Chelsea     Everton   Liverpool    Man City  Man United   Tottenham 
#586         643         617         577         538         625         582         557 

consistentTeamRedCard=redCardTeam[c(consistentTeams)]
#Arsenal Aston Villa     Chelsea     Everton   Liverpool    Man City  Man United   Tottenham 
#33          25          38          27          24          34          27          31 
##############################################################################################################

#SUB_MODULE 4- COMPLETE
#Referees-Cards

referee=levels(OrigTable$Referee)
cards=c()
matches=c()
for(i in 1:length(referee)) {
  cards=c(cards, sum(OrigTable[OrigTable$Referee==referee[i],21],OrigTable[OrigTable$Referee==referee[i],22]))
  cards=c(cards, sum(OrigTable[OrigTable$Referee==referee[i],23],OrigTable[OrigTable$Referee==referee[i],24]))
  matches=c(matches, length(which(OrigTable$Referee==referee[i])))
}

l1=c("yellow","red")
l=list(c(),l1)
cardsMat=matrix(cards,nrow=40,ncol=2,byrow=T,l)  
tab=cbind(referee,cardsMat)
refereeTable=cbind(tab,matches)

yellowCardAvg=strtoi(refereeTable[,2])/strtoi(refereeTable[,4])
redCardAvg=strtoi(refereeTable[,3])/strtoi(refereeTable[,4])
refereeTable=cbind(refereeTable,yellowCardAvg,redCardAvg)

refereeOfficiatedGreaterThanHundred=subset(refereeTable, matches > 100)

refereeOfficiatedGreaterThanHundred

refereeYellowDiscipline=array(refereeOfficiatedGreaterThanHundred[,5])
dimnames(refereeYellowDiscipline)=list(refereeOfficiatedGreaterThanHundred[,1])
refereeRedDiscipline=array(refereeOfficiatedGreaterThanHundred[,6])
dimnames(refereeRedDiscipline)=list(refereeOfficiatedGreaterThanHundred[,1])

refereeYellowDiscipline
refereeRedDiscipline

dotchart(as.numeric(refereeYellowDiscipline),cex=.9,labels=row.names(refereeYellowDiscipline),
         main="Number of Yellow Cards shown per Game(for Referees who officiated more than 100 games)", 
         xlab="Number of Yellow Cards shown per Game",col="red",lcolor="blue")

dotchart(as.numeric(refereeRedDiscipline),cex=.9,labels=row.names(refereeRedDiscipline),
         main="Number of Red Cards shown per Game(for Referees who officiated more than 100 games)", 
         xlab="Number of Red Cards shown per Game",col="red",lcolor="blue")
##############################################################################################################

#SUB_MODULE 5- COMPLETE
#least disciplined team
###FOULs
homeTeamFouls=c()
awayTeamFouls=c()

for(i in 1:length(seasons)) {
  for(j in 1:length(totalTeams)) {
    homeS=sum(OrigTable[which(OrigTable$HomeTeam==totalTeams[j] & OrigTable$Season==seasons[i]),17])
    homeTeamFouls=c(homeTeamFouls,homeS)
  }
}

for(i in 1:length(seasons)) {
  for(j in 1:length(totalTeams)) {
    awayS=sum(OrigTable[which(OrigTable$AwayTeam==totalTeams[j] & OrigTable$Season==seasons[i]),18])
    awayTeamFouls=c(awayTeamFouls,awayS)
  }
}

homeFoulMatrix=matrix(homeTeamFouls,nrow=36,ncol=10,byrow=F)
awayFoulMatrix=matrix(awayTeamFouls,nrow=36,ncol=10,byrow=F)
avgHomeFouls=homeFoulMatrix/38
avgAwayFouls=awayFoulMatrix/38
dimnames(avgHomeFouls)=c(list(totalTeams),list(seasons))
dimnames(avgAwayFouls)=c(list(totalTeams),list(seasons))

consistentTeamsHomeFouls=avgHomeFouls[c(consistentTeams),]
consistentTeamsAwayFouls=avgAwayFouls[c(consistentTeams),]

#HOME TEAM FOUL GRAPH
plot.new()
color=rainbow(length(consistentTeams))
plot(1:10,consistentTeamsHomeFouls[1,],type="b",xlim=c(1,12),ylim=c(4,7.5),col=color[1],xlab='Seasons',ylab='Mean Fouls Commited as Home Team Per Game',main='Mean Fouls Committed by the Home Team Per Game- For Consistent Teams')
for(i in 2:length(consistentTeams))
  lines(1:10,consistentTeamsHomeFouls[i,],type="b",col=color[i])
legend(9.7,7.5,c(consistentTeams),col=color, cex=1,lty=c(1,1),lwd=c(2.5,2.5),bty="n")

#AWAY TEAM FOUL GRAPH
plot.new()
color=rainbow(length(consistentTeams))
plot(1:10,consistentTeamsAwayFouls[1,],type="b",xlim=c(1,12),ylim=c(4,7.5),col=color[1],xlab='Seasons',ylab='Mean Fouls Commited as Away Team Per Game',main='Mean Fouls Committed by the Away Team Per Game- For Consistent Teams')
for(i in 2:length(consistentTeams))
  lines(1:10,consistentTeamsAwayFouls[i,],type="b",col=color[i])
legend(9.7, 7.5,c(consistentTeams),col=color, cex=1,lty=c(1,1),lwd=c(2.5,2.5),bty="n")

#check for drastic differences between home and away foul teams
##############################################################################################################

#SUB_MODULE 6
#Month analysis
totalTeams[36]
results=c('HW','Aw','HD','AD','HL','AL','win-percent')

monthAnalysis=array(0,dim=c(36,7,10),dimnames=list(totalTeams,results,months))
#monthAnalysis
for(i in 1:length(totalTeams)) {
  for(j in 1:length(months)) {
    monthAnalysis[i,1,j]=nrow(OrigTable[OrigTable[,2]==months[j] & OrigTable[,4]==totalTeams[i] & OrigTable[,8]=='H',])
    monthAnalysis[i,2,j]=nrow(OrigTable[OrigTable[,2]==months[j] & OrigTable[,5]==totalTeams[i] & OrigTable[,8]=='A',])
    monthAnalysis[i,3,j]=nrow(OrigTable[OrigTable[,2]==months[j] & OrigTable[,4]==totalTeams[i] & OrigTable[,8]=='D',])
    monthAnalysis[i,4,j]=nrow(OrigTable[OrigTable[,2]==months[j] & OrigTable[,5]==totalTeams[i] & OrigTable[,8]=='D',])      
    monthAnalysis[i,5,j]=nrow(OrigTable[OrigTable[,2]==months[j] & OrigTable[,4]==totalTeams[i] & OrigTable[,8]=='A',])
    monthAnalysis[i,6,j]=nrow(OrigTable[OrigTable[,2]==months[j] & OrigTable[,5]==totalTeams[i] & OrigTable[,8]=='H',])      
  }
}
dimnames(monthAnalysis)[3]
#gets win-percent
for(i in 1:length(totalTeams)) {
  for(j in 1:length(months)) {
    monthAnalysis[i,7,j]=(monthAnalysis[i,1,j]+monthAnalysis[i,2,j])/sum(monthAnalysis[i,,j])
  } 
}

consistentTeamMonthAnalysis=monthAnalysis[c(consistentTeams),,]

for(i in 1:length(consistentTeams))
dotchart(as.numeric(consistentTeamMonthAnalysis[i,7,]),labels=months,cex=.9,
         main=sprintf("Team performance by months for last 10 seasons- %s",consistentTeams[i]), 
         xlab="Win percent for the team",col="red",lcolor="blue")
##############################################################################################################
#SUB_MODULE 8
#Percentage of accurate shots
#Consistent teams
homeShots=c()
homeShotsOnTarget=c()
awayShots=c()
awayShotsOnTarget=c()
shots=matrix(nrow=8,ncol=9,byrow=T)
colN=c("Home Shots","Home Shots on Target","Away Shots","Away Shots on Target",
       "Total Shots","Total Shots on Target",
       "Home Shot Accuracy","Away Shot Accuracy","Total Shot Accuracy")

dimnames(shots)=list(consistentTeams,colN)
shots

for(i in 1:length(consistentTeams)) {
  homeShots=c(homeShots,sum(OrigTable[OrigTable$HomeTeam==consistentTeams[i],13]))
  homeShotsOnTarget=c(homeShotsOnTarget,sum(OrigTable[OrigTable$HomeTeam==consistentTeams[i],15]))    
  awayShots=c(awayShots,sum(OrigTable[OrigTable$AwayTeam==consistentTeams[i],14]))    
  awayShotsOnTarget=c(awayShotsOnTarget,sum(OrigTable[OrigTable$AwayTeam==consistentTeams[i],16]))
}
sum(OrigTable[OrigTable$AwayTeam=='Chelsea',14])

shots[,1]=homeShots
shots[,2]=homeShotsOnTarget
shots[,3]=awayShots
shots[,4]=awayShotsOnTarget
shots[,5]=homeShots+awayShots
shots[,6]=homeShotsOnTarget+awayShotsOnTarget
shots[,7]=homeShotsOnTarget/homeShots
shots[,8]=awayShotsOnTarget/awayShots
shots[,9]=shots[,6]/shots[,5]

shots

plot.new()
color=rainbow(length(consistentTeams))
hist(shots[,1])
plot(1:length(consistentTeams),shots[5,],type="b",xlim=c(1,12),ylim=c(4,7.5),col=color[1],xlab='Seasons',ylab='Mean Fouls Commited as Away Team Per Game',main='Mean Fouls Committed by the Away Team Per Game- For Consistent Teams')
for(i in 2:length(consistentTeams))
  lines(1:10,consistentTeamsAwayFouls[i,],type="b",col=color[i])
legend(9.7, 7.5,c(consistentTeams),col=color, cex=1,lty=c(1,1),lwd=c(2.5,2.5),bty="n")





##############################################################################################################

#SUB_MODULES TO BE STUDIED
percentage of goals scored in first half
total number of goals scored in each season for consistent tems--doubtful
most accurate shotss(fst/fs)
corners

#RIVAL PLOT- example
##############################################################################################################