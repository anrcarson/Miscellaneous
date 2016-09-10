###########################
# A Little R - Demo
############################
# Goal - Create a model of a football team's score in a game as a function of yards, downs, TOs, etc.
# Use data from http://www.pro-football-reference.com/

##############
#get data
#############

# include XML package to read data from HTML tables
library(XML)


#team name table
teamnames<-c("Cardinals","Falcons", "Ravens", "Bills", "Panthers","Bears","Bengals", "Browns","Cowboys","Broncos", "Lions","Packers", "Jaguars", "Chiefs","Patriots", "Saints", "Jets", "Raiders","Eagles", "Chargers","Seahawks", "Rams", "Buccaneers", "Texans", "Colts", "Dolphins", "Vikings", "Giants", "Steelers", "49ers", "Titans", "Redskins")
teamurlname<-c("crd", "atl","rav", "buf", "car", "chi","cin", "cle","dal", "den","det","gnb", "jax", "kan","nwe","nor", "nyj","rai", "phi","sdg", "sea", "ram", "tam", "htx", "clt", "mia", "min", "nyg", "pit", "sfo", "oti", "was")
teamnametable<-data.frame(cbind(teamnames,teamurlname))


n<-c("fullname", "urlname", "year", "Week", "Day", "Date", "Boxscore", "Win/Loss", "OT", "Record","Home/Away", "Opponent", "TeamScore","OppScore", "Team1stDowns","TeamTotYd","TeamPassYd", "TeamRushYd","TeamTOs","Opp1stDowns", "OppTotYd", "OppPassYd","OppRushYd", "OppTOs")
n


#get all teams data since 1976
gamescores<-data.frame()
current_year<-data.frame()

for (i in 1:length(teamnametable$teamurlname))
{
  name<-as.character(teamnametable$teamurlname[i])
  fullname<-as.character(teamnametable$teamnames[i])
  year<-1976
  
  for(year in 1976:2014)
  {
  
  url<-paste("http://www.pro-football-reference.com/teams/",name,"/",year, ".htm", sep="")
  tables <- readHTMLTable(url, stringsAsFactors=FALSE)
  
  #convert to dataframe (format like a database table)
  current_year<-data.frame(tables$team_gamelogs[1:21])
  
  #append fullname, urlname, year to data.frame
  urlname_col<-rep(name, each=nrow(current_year))
  fullname_col<-rep(fullname, each=nrow(current_year))
  year_col<-rep(year, each=nrow(current_year))
  
  current_year<-cbind(fullname_col,urlname_col,year_col,current_year)
  
  if (length(current_year)==24)
  {
  names(current_year)<-n
  }
  
  #append to fulldataset
  gamescores<-rbind(gamescores,current_year)
  
  print(paste(i,name,year))
  
  }
  

}




##data cleanup
#head(gamescores)

#clean up names
n<-c("fullname", "urlname", "year", "Week", "Day", "Date", "Boxscore", "Win/Loss", "OT", "Record","Home/Away", "Opponent", "TeamScore","OppScore", "Team1stDowns","TeamTotYd","TeamPassYd", "TeamRushYd","TeamTOs","Opp1stDowns", "OppTotYd", "OppPassYd","OppRushYd", "OppTOs")
n
names(gamescores)<-n
names(gamescores)

#remove empty game rows
gamescores<-gamescores[gamescores$"Win/Loss"=="W"|gamescores$"Win/Loss"=="L",]

#save full data set
saved_fulldata<-gamescores
#gamescores<-saved_fulldata


#############################################
###create function for future cleanup of data
#############################################

clean_gamescores<-function(gamescores)
{
  ###prepare data for analysis
  #remove unneeded columns. Move Home/Away to end
  gamescores<-gamescores[11:24]
  gamescores<-cbind(gamescores[2:14],gamescores[1])
  keep = c(1:4,6:9,11:14)
  gamescores<-gamescores[keep]
  
  #convert "numeric" columns to numeric
  numeric<-c()
  column<-c()
  for (i in 2:11)
    {
    column<-as.numeric(as.character(gamescores[,i]))
    numeric<-cbind(numeric,column)
    }
  
  numeric<-data.frame(numeric)
  names(numeric)<-names(gamescores[2:11])
  
  #add Home/Away to numeric and convert values
  home<-as.matrix(gamescores$"Home/Away")
  home[home==""]<-1
  home[home!="1"]<-0
  home<-as.numeric(home)
  
  numeric<-cbind(numeric,home)
  
  away<-as.matrix(gamescores$"Home/Away")
  away[away=="@"]<-1
  away[away!="1"]<-0
  away<-as.numeric(away)
  
  numeric<-cbind(numeric,away)
  
  #replace missing numeric values with 0
  numeric[is.na(numeric)]<-0
  
  return(numeric)
}

##get cleaned data
numeric<-clean_gamescores(gamescores)

################
#explore data
################




#summary statistics
summary(numeric)
league_averages<-data.frame(t(apply(numeric,2, mean))) #all averages since 1976
league_averages

mean(numeric$TeamScore[numeric$home == 1]) #22.3
mean(numeric$TeamScore[numeric$home == 0]) #19.4. Home advantage worth about 3 points.

#plots
hist(numeric$TeamScore, breaks=50, col="green", main="Team Score Frequencies")
freq_score<-data.frame(table(numeric$TeamScore))
freq_score
freq_score[freq_score$Freq==max(freq_score$Freq),]


TotalYds = numeric$TeamPassYd + numeric$TeamRushYd
plot(TeamScore ~ TotalYds, numeric, main="TotalYds vs. TeamScore", asp=1, col="purple")



############
#models
############

#linear model to predict team score
score_model = lm(numeric$TeamScore ~ numeric$OppScore + numeric$Team1stDowns + numeric$TeamPassYd + numeric$TeamRushYd + numeric$TeamTOs + numeric$Opp1stDowns + numeric$OppPassYd + numeric$OppRushYd + numeric$OppTOs + numeric$home + numeric$away)
summary(score_model)


#linear model to predict point spread
scoredif=numeric$TeamScore - numeric$OppScore
firstdownsdif = numeric$Team1stDowns - numeric$Opp1stDowns
passydsdiff = numeric$TeamPassYd - numeric$OppPassYd
rushydsdiff = numeric$TeamRushYd - numeric$OppRushYd
TOsdiff = numeric$TeamTOs - numeric$OppTOs
home = numeric$home
away = numeric$away

sd_model = lm(scoredif ~ firstdownsdif + passydsdiff + rushydsdiff +  TOsdiff + home + away)
summary(sd_model)

plot(residuals(sd_model) +scoredif ~ scoredif, asp=1, col="blue")

x<-c(100:100)
lines(x,x, col="red")
#model is biased since the residuals are not random.
#Probably because a linear model is not the best type of model.

##########################################################################################
#predict next Seahawks game using averages for both teams this season- final score and score difference
##########################################################################################

#Seahawks playing Oakland Raiders at home on Sunday November 2

#get Seahawks data for 2014 so far
team<-"Seahawks"
year2<-2014

teamdata<-saved_fulldata[saved_fulldata$fullname==team & saved_fulldata$year==year2,]
teamdata<-clean_gamescores(teamdata)

#get averages
teamdata_mean<-data.frame(t(apply(teamdata,2, mean)))

#get Raiders data
opponent<-"Raiders"
year3<-2014
opponentdata<-saved_fulldata[saved_fulldata$fullname==opponent & saved_fulldata$year==year3,]
opponentdata<-clean_gamescores(opponentdata)

#get averages
opponentdata_mean<-data.frame(t(apply(opponentdata,2, mean)))



#calculate score diff - just use averages for each team this season
scoredif=teamdata_mean$TeamScore - opponentdata_mean$TeamScore
firstdownsdif = teamdata_mean$Team1stDowns- opponentdata_mean$Team1stDowns
passydsdiff = teamdata_mean$TeamPassYd - opponentdata_mean$TeamPassYd
rushydsdiff = teamdata_mean$TeamRushYd - opponentdata_mean$TeamRushYd
TOsdiff = teamdata_mean$TeamTOs - opponentdata_mean$TeamTOs
home = 1 #home game for seahawks
away = 0

newdata<-data.frame(firstdownsdif,passydsdiff,rushydsdiff,TOsdiff, home, away, row.names="Score Diff")

predict_scorediff=predict.lm(sd_model, newdata)  
predict_scorediff #10.06 difference in seahawks favor




#calculate final score
#score_model = lm(numeric$TeamScore ~ numeric$OppScore + numeric$Team1stDowns + numeric$TeamPassYd 
#                 + numeric$TeamRushYd + numeric$TeamTOs + numeric$Opp1stDowns + numeric$OppPassYd 
#                 + numeric$OppRushYd + numeric$OppTOs + numeric$home + numeric$away)
#Need to estimate game stats to plug into model. 
#Use weighted averages based on league averages since 1976 and opponent averages this season

coeffs = coefficients(score_model)
teamScore = coeffs[1] +         #intercept
            coeffs[2]*opponentdata_mean$TeamScore*(teamdata_mean$OppScore/league_averages$TeamScore) + 
            coeffs[3]*teamdata_mean$Team1stDowns*(opponentdata_mean$Opp1stDowns/league_averages$Team1stDowns) + 
            coeffs[4]*teamdata_mean$TeamPassYd*(opponentdata_mean$OppPassYd/league_averages$TeamPassYd)+ 
            coeffs[5]*teamdata_mean$TeamRushYd*(opponentdata_mean$OppRushYd/league_averages$TeamRushYd)+ 
            coeffs[6]*teamdata_mean$TeamTOs*(opponentdata_mean$OppTOs/league_averages$TeamTOs)+ 
            
            coeffs[7]*opponentdata_mean$Team1stDowns*(teamdata_mean$Opp1stDowns/league_averages$Team1stDowns)+ 
            coeffs[8]*opponentdata_mean$TeamPassYd*(teamdata_mean$OppPassYd/league_averages$TeamPassYd)+ 
            coeffs[9]*opponentdata_mean$TeamRushYd*(teamdata_mean$OppRushYd/league_averages$TeamRushYd)+ 
            coeffs[10]*opponentdata_mean$TeamTOs*(teamdata_mean$OppTOs/league_averages$TeamTOs)+ 
            coeffs[11]* 1 +  #homegame for seahawks
            coeffs[12]* 0    #homegame for seahawks

teamScore  #27.72

opponentscore = coeffs[1] + 
                coeffs[2]*teamdata_mean$TeamScore*(opponentdata_mean$OppScore/league_averages$TeamScore) + 
                coeffs[3]*opponentdata_mean$Team1stDowns*(teamdata_mean$Opp1stDowns/league_averages$Team1stDowns) + 
                coeffs[4]*opponentdata_mean$TeamPassYd*(teamdata_mean$OppPassYd/league_averages$TeamPassYd)+ 
                coeffs[5]*opponentdata_mean$TeamRushYd*(teamdata_mean$OppRushYd/league_averages$TeamRushYd)+ 
                coeffs[6]*opponentdata_mean$TeamTOs*(teamdata_mean$OppTOs/league_averages$TeamTOs)+ 
                
                coeffs[7]*teamdata_mean$Team1stDowns*(opponentdata_mean$Opp1stDowns/league_averages$Team1stDowns)+ 
                coeffs[8]*teamdata_mean$TeamPassYd*(opponentdata_mean$OppPassYd/league_averages$TeamPassYd)+ 
                coeffs[9]*teamdata_mean$TeamRushYd*(opponentdata_mean$OppRushYd/league_averages$TeamRushYd)+ 
                coeffs[10]*teamdata_mean$TeamTOs*(opponentdata_mean$OppTOs/league_averages$TeamTOs)+ 
                coeffs[11]* 0 +  #away game for Raiders
                coeffs[12]* 1   #away game for Raiders


opponentscore #16.87


#Seahawks win!!! 28 to 17 (rounding)

#########
#Is this model good?  Looking at previous weeks...
#########

#WK 1
#Prediction
#SEA 30 - GB 21

#Actual
#SEA 36 - GB 16

#WK 2
#Prediction
#SEA 23 - SD 24

#Actual
#SEA 21 - SD 30

#WK 3
#Prediction
#SEA 26 - DNV 27

#Actual
#SEA 26 - DNV 20

#WK 4
#Bye

#WK 5
#Prediction
#SEA 24 - WAS 27

#Actual
#SEA 27 - WAS 17

#WK 6
#Prediction
#SEA 27 - DAL 25

#Actual
#SEA 23 - DAL 30

#WK 7
#Prediction
#SEA 28 - STL 25

#Actual
#SEA 26 - STL 28

#WK 8
#Prediction
#SEA 27 - CAR 24

#Actual
#SEA 13- CAR 9 

#Conclusion:
#Correct outcome: 3/7
#Scores: most predictions within 7 points, but still a lot of variance in predictions.

#Model needs more work...

mean(abs(residuals(score_model))) #5.199

#the model will miss a score, on average, by 5 points above or below.
#So if a game's score difference is 10 or lower, the model isn't a trustworthy predicter
#of the game's outcome.

###################
#Going Forward
###################
#Improvements to data set -> improvements in model
#   individual stats like passing completion by QB
#   3rd down conversions
#   number of field goals
#   number of tds
#   ..........

#Try different models
#   exponential
#   power


#Other analysis/challenges using this same data set include...
#   Predict whether a team will make it to the playoffs
#   Predict whether a team will win the SuperBowl
#   Rank teams from best to worst
#   Rank players by position
#   ..........


##########
#Conclusion
##########

#Any questions?
#Thanks!!!



