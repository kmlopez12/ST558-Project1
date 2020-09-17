Project 1 - Karen Lopez
================
September 18, 2020

-   [Reading and Summarizing Data from the NHL’s
    API](#reading-and-summarizing-data-from-the-nhls-api)
    -   [Required Packages](#required-packages)
    -   [Contact and Get Data](#contact-and-get-data)

Reading and Summarizing Data from the NHL’s API
===============================================

This vignette was compiled to provide detailed instructions on reading
and summarizing data from the National Hockey League’s, or NHL’s, API
using R code.

Required Packages
-----------------

First, install and read in the necessary packages, as shown below.
Packages only need to be installed once, but code is included in
comment.

    #install.packages("tidyverse")
    #library(tidyverse)
    library(httr)
    library(jsonlite)
    library(dplyr)

Contact and Get Data
--------------------

This code contains functions that will contact the [NHL records
API](https://gitlab.com/dword4/nhlapi/-/blob/master/records-api.md) and
[NHL stat
API](https://gitlab.com/dword4/nhlapi/-/blob/master/stats-api.md), and
return parsed, well-formatted data.

    #function that creates the url for the records API
    getUrl <- function(input){
      baseUrl <- "https://records.nhl.com/site/api/franchise"
      table <- input
      fullURL <- paste0(baseUrl, table) #create url
    }

    franchise <- function(){
      franchiseAPI <- GET(getUrl(NULL)) #get url to grab data
      franchiseAPI #check connection
      
      franchiseText <- content(franchiseAPI, "text") #convert to JSON text form
      #franchiseText #check dataset
      franchiseList <- fromJSON(franchiseText, flatten=TRUE) #convert form
      franchiseDF <- as.data.frame(franchiseList) #convert to df, adds .data to col names
      #franchiseDF #check dataset
      colnames(franchiseDF) <- c("id", "firstSeasonId", "lastSeasonId", "mostRecentTeamId", "teamCommonName", "teamPlaceName", "total") #rename columns
      
      return(franchiseDF)
    }
    #franchiseData <- franchise() #test that function works
    #print(knitr::kable(franchiseData))

    teamTotals <- function(){
      teamAPI <- GET(getUrl("-team-totals"))
      teamAPI #check connection
      
      teamText <- content(teamAPI, "text") #convert to JSON text form
      #teamText #check dataset
      teamList <- fromJSON(teamText, flatten=TRUE) #convert form
      teamDF <- as.data.frame(teamList)
      #teamList #check dataset
      colnames(teamDF) <- c("id", "activeFranchise", "firstSeasonId", "franchiseId", "gameTypeId", "gamesPlayed", "goalsAgainst", "goalsFor", "homeLosses", "homeOvertimeLosses", "homeTies", "homeWins", "lastSeasonId", "losses","overtimeLosses", "penaltyMinutes", "pointPctg", "points", "roadLosses", "roadOvertimeLosses", "roadTies", "roadWins", "shootoutLosses", "shootoutWins",  "shutouts", "teamId", "teamName", "ties", "triCode", "wins", "total")
      
      return(teamDF)
    }
    #teamTotals <- teamTotals() #test that function works
    #print(knitr::kable(teamTotals))

    #function that return numeric team ID when supplied with team name
    getID <- function(teamName){
      teamData <- data.frame()
      teamID <- NULL
      if(teamName %in% franchiseData$teamCommonName){
        #print(paste0(teamName, " is common name")) #test code
        teamData <- franchiseData %>% filter(franchiseData$teamCommonName==teamName)
        teamID <- teamData$id
      } else if(teamName %in% franchiseData$teamPlaceName){
        #print(paste0(teamName, " is place name")) #test code
        teamData <- franchiseData %>% filter(franchiseData$teamPlaceName==teamName)
        teamID <- teamData$id
      } else {print("Please enter valid team name.")}
      return(teamID)
    }

    seasonRecords <- function(ID){
      url <- ""
      if(is.numeric(ID)){
        url <- paste0("-season-records?cayenneExp=franchiseId=", ID)
      } else if(is.character(ID)){
        ID = getID(ID)
        url <- paste0("-season-records?cayenneExp=franchiseId=", ID)
      }
      #url = "-season-records" #view the 38 teams that have stats
      #print(url) #test code
      seasonAPI <- GET(getUrl(url))
      seasonAPI #check connection
      
      seasonText <- content(seasonAPI, "text") #convert to JSON text form
      seasonText #check dataset
      seasonList <- fromJSON(seasonText, flatten=TRUE) #convert form
      #seasonList #check dataset
      seasonDF <- as.data.frame(seasonList)
      #seasonList #check dataset
      colnames(seasonDF) <- c("id", "fewestGoals", "fewestGoalsAgainst", "fewestGoalsAgainstSeasons", "fewestGoalsSeasons", "fewestLosses", "fewestLossesSeasons", "fewestPoints", "fewestPointsSeasons", "fewestTies", "fewestTiesSeasons", "fewestWins", "fewestWinsSeasons", "franchiseId", "franchiseName", "homeLossStreak", "homeLossStreakDates", "homePointStreak", "homePointStreakDates", "homeWinStreak", "homeWinStreakDates", "homeWinlessStreak", "homeWinlessStreakDates", "lossStreak", "lossStreakDates", "mostGameGoals", "mostGameGoalsDates", "mostGoals", "mostGoalsAgainst", "mostGoalsAgainstSeasons", "mostGoalsSeasons", "mostLosses", "mostLossesSeasons", "mostPenaltyMinutes", "mostPenaltyMinutesSeasons", "mostPoints", "mostPointsSeasons", "mostShutouts", "mostShutoutsSeasons", "mostTies", "mostTiesSeasons", "mostWins", "mostWinsSeasons", "pointStreak", "pointStreakDates", "roadLossStreak", "roadLossStreakDates", "roadPointStreak", "roadPointStreakDates", "roadWinStreak", "roadWinStreakDates", "roadWinlessStreak", "roadWinlessStreakDates", "winStreak", "winStreakDates", "winlessStreak", "winlessStreakDates") #change df col names

      return(seasonDF)
    }
    #seasonRecord <- seasonRecords("Carolina") #test that function works
    #print(knitr::kable(seasonRecord))

    goalieRecords <- function(ID){
      url <- ""
      if(is.numeric(ID)){
        url <- paste0("-goalie-records?cayenneExp=franchiseId=", ID)
      } else if(is.character(ID)){
        ID = getID(ID)
        url <- paste0("-goalie-records?cayenneExp=franchiseId=", ID)
      }
      #url = "-goalie-records" #view the goalies that have stats
      #print(url) #test code
      goalieAPI <- GET(getUrl(url))
      goalieAPI #check connection
      
      goalieText <- content(goalieAPI, "text") #convert to JSON text form
      goalieText #check dataset
      goalieList <- fromJSON(goalieText, flatten=TRUE) #convert form
      goalieList #check dataset

      goalieDF <- as.data.frame(goalieList)
      #goalieList #check dataset
      colnames(goalieDF) <- c("id", "activePlayer", "firstName", "franchiseId", "franchiseName", "gameTypeId", "gamesPlayed", "lastName", "losses", "mostGoalsAgainstDates", "mostGoalsAgainstOneGame", "mostSavesDates", "mostSavesOneGame", "mostShotsAgainstDates", "mostShotsAgainstOneGame", "mostShutoutsOneSeason", "mostShutoutsSeasonIds", "mostWinsOneSeason", "mostWinsSeasonIds", "overtimeLosses", "playerId", "positionCode", "rookieGamesPlayed", "rookieShutouts", "rookieWins", "seasons", "shutouts", "ties", "dwins", "total") #change df col names

      return(goalieDF)
    }
    #goalieRecord <- goalieRecords("Carolina") #test that function works
    #print(knitr::kable(goalieRecord))

    skaterRecords <- function(ID){
      url <- ""
      if(is.numeric(ID)){
        url <- paste0("-skater-records?cayenneExp=franchiseId=", ID)
      } else if(is.character(ID)){
        ID = getID(ID)
        url <- paste0("-skater-records?cayenneExp=franchiseId=", ID)
      }
      
      #url = "-skater-records" #view the skaters that have stats
      #print(url) #test code
      skaterAPI <- GET(getUrl(url))
      #skaterAPI #check connection

      skaterText <- content(skaterAPI, "text") #convert to JSON text form
      #skaterText #check dataset
      skaterList <- fromJSON(skaterText, flatten=TRUE) #convert form
      #skaterList #check dataset
      
      skaterDF <- as.data.frame(skaterList)
      #skaterList #check dataset
      colnames(skaterDF) <- c("id", "activePlayer", "assists", "firstName", "franchiseId", "franchiseName", "gameTypeId", "gamesPlayed", "goals", "lastName", "mostAssistsGameDates", "mostAssistsOneGame", "mostAssistsOneSeason", "mostAssistsSeasonIds", "mostGoalsGameDates", "mostGoalsOneGame", "mostGoalsOneSeason", "mostGoalsSeasonIds", "mostPenaltyMinutesOneSeason", "mostPenaltyMinutesSeasonIds", "mostPointsGameDates", "mostPointsOneGame", "mostPointsOneSeason", "mostPointsSeasonIds", "penaltyMinutes", "playerId", "points", "positionCode", "rookiePoints", "seasons", "total") #change df col names

      return(skaterDF)
    }
    #skaterRecord <- skaterRecords(9) #test that function works
    #print(knitr::kable(skaterRecord))

It is intended that you can either specify ID or the name. The intention
was to map the names the user gives to the IDs and then just use the ID
in the URL building. This means you should use a switch() or if else or
something to look at the ID input given, determine if it is a number
(use ID directly) or convert it from a franchise name to an ID. If you
can get it to work another way with the user specifying either input,
that is fine too!  
I wrote a function that incorporated filter() & returns data based on
user input of the team name. For ex, using the franchise table, if a
user inputs name=“Tigers” into my function’s arguments.  
\#gameTypeId = 2 means regular season games, gameTypeId = 3 means
post-season games. Type =1 if you ever see that is preseason

    #from stats API
    teams <- function(){}
    #http://statsapi.web.nhl/v1/teams
    #8 modifiers list:https://gitlab.com/dword4/nhlapi/-/blob/master/stats-api.md#teams
    #someone can't get Stats API, ?stats=statsSingleSeasonPlayoffs modifier to work
    #someone had trouble with person.names and team.schedule.next (get only teams with next game scheduled)
