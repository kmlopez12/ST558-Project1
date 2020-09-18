Project 1 - Karen Lopez
================
September 18, 2020

-   [Reading and Summarizing Data from the NHL’s
    API](#reading-and-summarizing-data-from-the-nhls-api)
    -   [Required Packages](#required-packages)
    -   [Make Contact and Get Data](#make-contact-and-get-data)
        -   [Franchise API](#franchise-api)
        -   [Stats API](#stats-api)
        -   [Wrapper Function](#wrapper-function)

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

Make Contact and Get Data
-------------------------

This code contains functions that will contact the [NHL records
API](https://gitlab.com/dword4/nhlapi/-/blob/master/records-api.md) and
[NHL stat
API](https://gitlab.com/dword4/nhlapi/-/blob/master/stats-api.md), and
return parsed, well-formatted data.

### Franchise API

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
    franchiseData <- franchise() #test that function works
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

### Stats API

The user is required to input a team ID or name and modifier to access
the desired data. This string input format isn’t ideal, but is corrected
in the wrapper function.  
Input guide for modifiers, copied from API website:  
-?expand=team.roster Shows roster of active players for the specified
team  
-?expand=person.names Same as above, but gives less info  
-?expand=team.schedule.next Returns details of the upcoming game for a
team  
-?expand=team.schedule.previous Same as above but for the last game
played  
-?expand=team.stats Returns the teams stats for the season  
-?expand=team.roster&season=20142015 Adding the season identifier shows
the roster for that season  
-?teamId=4,5,29 Can string team id together to get multiple teams

    #from stats API
    teams <- function(ID, input){
      url <- ""
      if(is.numeric(ID)){
        url <- paste0("https://statsapi.web.nhl.com/api/v1/teams/", ID, "/", input)
      } else if(is.character(ID)){
        ID = getID(ID)
        url <- paste0("https://statsapi.web.nhl.com/api/v1/teams/", ID, "/", input)
      }
      
      #print(url)
      teamsAPI <- GET(url)
      teamsAPI #check connection

      teamsText <- content(teamsAPI, "text") #convert to JSON text form
      teamsText #check dataset
      teamsList <- fromJSON(teamsText, flatten=TRUE) #convert form
      #teamsList #check dataset
      
      teamsDF <- as.data.frame(teamsList)
      #teamsList #check dataset
      #colnames(teamsDF) <- c() #change df col names

      return(teamsDF)
    }
    #teamsRecord <- teams(12,"?expand=person.names") #test that function works
    #print(knitr::kable(teamsRecord))

    #teamsRecord2 <- teams(54,"?expand=team.schedule.next") #test that function works
    #print(knitr::kable(teamsRecord2))

    #API returns the same team information table across the different modifiers with nested data frame(s) that contain the modifier data. On the discussion board, the instructor noted we can print the output as is and that we shouldn't worry about the last modifier, so I omitted modifier 13 (?stats=statsSingleSeasonPlayoffs).

    #first 7 of 8 modifiers list:https://gitlab.com/dword4/nhlapi/-/blob/master/stats-api.md#teams  

### Wrapper Function

This wrapper function allows the user to access any of the API endpoints
and modifiers included in the above code. The user must input the
endpoint and team ID or name, where team input is applicable. The
endpoints have been coded by numerbers, making user input more friendly.

Endpoint input guide, number 1-5 are from the Records API and numbers
6-12 are from the Stats API: -1: franchise information for all teams  
-2: team totals for all teams  
-3: season records for a team (must specify team ID or name)  
-4: goalie records for a team (must specify team ID or name)  
-5: skater records for a team (must specify team ID or name)  
-6: team roster of active players for a team (must specify team ID or
name)  
-7: active player names for a team (must specify team ID or name)  
-8: upcoming game for a team (must specify team ID or name)  
-9: last game played for a team (must specify team ID or name)  
-10: season stats for a team (must specify team ID or name)  
-11: roster by season for a team (must specify team ID or name)  
-12: franchise information for multiple teams (must specify team IDs or
names)

    accessAPI <- function(endpoint, team=NULL){
      outputDF <- NULL #create output object
      
      #check if team name is character or valid number, get numeric ID if character
      if(is.null(team)){
      } else if(is.character(team)){
        team = getID(team)
      } else if(!is.numeric(team)){
        print("Please enter valid team name or ID.")
      }
      
      #check if endpoint is valid, get url based on endpoint input
      if(endpoint==1){
        outputDF <- franchise()
      } else if(endpoint==2){
        outputDF <- teamTotals()
      } else if(endpoint==3){
        outputDF <- seasonRecords(team)
      } else if(endpoint==4){
        outputDF <- goalieRecords(team)
      } else if(endpoint==5){
        outputDF <- skaterRecords(team)
      } else if(endpoint==6){
        outputDF <- teams(team, "?expand=team.roster")
      } else if(endpoint==7){
        outputDF <- teams(team, "?expand=person.names")
      } else if(endpoint==8){
        outputDF <- teams(team, "?expand=team.schedule.next")
      } else if(endpoint==9){
        outputDF <- teams(team, "?expand=team.schedule.previous")
      } else if(endpoint==10){
        outputDF <- teams(team, "?expand=team.stats")
      } else if(endpoint==11){
        outputDF <- teams(team, "?expand=team.roster&season=20142015")
      } else if(endpoint==12){
        outputDF <- teams(team, "?teamId=4,5,29")
      } else {
        print("Please enter a valid number endpoint or modifier between 1 and 13.")
      }
    return(outputDF)
    }
    #test code, all work
    #franchise1 <- accessAPI(1)
    #print(head(franchise1))

    #goalie1<- accessAPI(4, "Carolina")
    #print(head(goalie1))

    roster1 <- accessAPI(6, team=12)
    print(roster1)

    ##                                                                                                                                                                            copyright
    ## 1 NHL and the NHL Shield are registered trademarks of the National Hockey League. NHL and NHL team marks are the property of the NHL and its teams. © NHL 2020. All Rights Reserved.
    ##   teams.id          teams.name       teams.link teams.abbreviation
    ## 1       12 Carolina Hurricanes /api/v1/teams/12                CAR
    ##   teams.teamName teams.locationName teams.firstYearOfPlay teams.shortName
    ## 1     Hurricanes           Carolina                  1979        Carolina
    ##                teams.officialSiteUrl teams.franchiseId teams.active
    ## 1 http://www.carolinahurricanes.com/                26         TRUE
    ##   teams.venue.id teams.venue.name    teams.venue.link teams.venue.city
    ## 1           5066        PNC Arena /api/v1/venues/5066          Raleigh
    ##   teams.venue.timeZone.id teams.venue.timeZone.offset teams.venue.timeZone.tz
    ## 1        America/New_York                          -4                     EDT
    ##   teams.division.id teams.division.name teams.division.nameShort
    ## 1                18        Metropolitan                    Metro
    ##    teams.division.link teams.division.abbreviation teams.conference.id
    ## 1 /api/v1/divisions/18                           M                   6
    ##   teams.conference.name teams.conference.link teams.franchise.franchiseId
    ## 1               Eastern /api/v1/conferences/6                          26
    ##   teams.franchise.teamName  teams.franchise.link
    ## 1               Hurricanes /api/v1/franchises/26
    ##                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            teams.roster.roster
    ## 1 22, 14, 47, 11, 51, 45, 21, 34, 18, 28, 31, 16, 19, 76, 86, 48, 23, 74, 57, 4, 39, 55, 13, 64, 20, 78, 24, 43, 88, 37, 8477488, 8468508, 8473503, 8473533, 8474581, 8475222, 8475799, 8475852, 8476288, 8476323, 8476341, 8476389, 8476462, 8476869, 8476882, 8476921, 8476934, 8476958, 8477845, 8477938, 8477968, 8477981, 8477998, 8478056, 8478427, 8478904, 8479402, 8479987, 8480039, 8480830, Brett Pesce, Justin Williams, James Reimer, Jordan Staal, Jake Gardiner, Sami Vatanen, Nino Niederreiter, Petr Mrazek, Ryan Dzingel, Max McCormick, Anton Forsberg, Vincent Trocheck, Dougie Hamilton, Brady Skjei, Teuvo Teravainen, Jordan Martinook, Brock McGinn, Jaccob Slavin, Trevor van Riemsdyk, Haydn Fleury, Alex Nedeljkovic, Roland McKeown, Warren Foegele, Clark Bishop, Sebastian Aho, Steven Lorentz, Jake Bean, Morgan Geekie, Martin Necas, Andrei Svechnikov, /api/v1/people/8477488, /api/v1/people/8468508, /api/v1/people/8473503, /api/v1/people/8473533, /api/v1/people/8474581, /api/v1/people/8475222, /api/v1/people/8475799, /api/v1/people/8475852, /api/v1/people/8476288, /api/v1/people/8476323, /api/v1/people/8476341, /api/v1/people/8476389, /api/v1/people/8476462, /api/v1/people/8476869, /api/v1/people/8476882, /api/v1/people/8476921, /api/v1/people/8476934, /api/v1/people/8476958, /api/v1/people/8477845, /api/v1/people/8477938, /api/v1/people/8477968, /api/v1/people/8477981, /api/v1/people/8477998, /api/v1/people/8478056, /api/v1/people/8478427, /api/v1/people/8478904, /api/v1/people/8479402, /api/v1/people/8479987, /api/v1/people/8480039, /api/v1/people/8480830, D, R, G, C, D, D, R, G, C, L, G, C, D, D, L, L, L, D, D, D, G, D, L, C, C, C, D, C, C, R, Defenseman, Right Wing, Goalie, Center, Defenseman, Defenseman, Right Wing, Goalie, Center, Left Wing, Goalie, Center, Defenseman, Defenseman, Left Wing, Left Wing, Left Wing, Defenseman, Defenseman, Defenseman, Goalie, Defenseman, Left Wing, Center, Center, Center, Defenseman, Center, Center, Right Wing, Defenseman, Forward, Goalie, Forward, Defenseman, Defenseman, Forward, Goalie, Forward, Forward, Goalie, Forward, Defenseman, Defenseman, Forward, Forward, Forward, Defenseman, Defenseman, Defenseman, Goalie, Defenseman, Forward, Forward, Forward, Forward, Defenseman, Forward, Forward, Forward, D, RW, G, C, D, D, RW, G, C, LW, G, C, D, D, LW, LW, LW, D, D, D, G, D, LW, C, C, C, D, C, C, RW
    ##         teams.roster.link
    ## 1 /api/v1/teams/12/roster
