Project 1 - Karen Lopez
================
September 18, 2020

-   [Reading and Summarizing Data from the NHL’s
    API](#reading-and-summarizing-data-from-the-nhls-api)
    -   [Required Packages](#required-packages)
    -   [Contact and Get Data](#contact-and-get-data)
    -   [Wrapper Function](#wrapper-function)
    -   [Exploratory Analysis](#exploratory-analysis)

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
      franchiseAPI <- GET(getUrl(NULL))
      franchiseAPI #check connection
      
      franchiseText <- content(franchiseAPI, "text") #convert to JSON text form
      #franchiseText #check dataset
      franchiseList <- fromJSON(franchiseText, flatten=TRUE) #convert form
      franchiseList #check dataset
      
      #franchiseList2 <- fromJSON(franchiseText, flatten=TRUE, simplifyDataFrame=TRUE)
      #franchiseList2
      #franchiseDF2 <- as.data.frame(franchiseList2) #convert form to df, but adds .data to column names
      #franchiseDF2
      
      return(franchiseList)
    }
    franchise()

    ## $data
    ##    id firstSeasonId lastSeasonId mostRecentTeamId teamCommonName teamPlaceName
    ## 1   1      19171918           NA                8      Canadiens      Montréal
    ## 2   2      19171918     19171918               41      Wanderers      Montreal
    ## 3   3      19171918     19341935               45         Eagles     St. Louis
    ## 4   4      19191920     19241925               37         Tigers      Hamilton
    ## 5   5      19171918           NA               10    Maple Leafs       Toronto
    ## 6   6      19241925           NA                6         Bruins        Boston
    ## 7   7      19241925     19371938               43        Maroons      Montreal
    ## 8   8      19251926     19411942               51      Americans      Brooklyn
    ## 9   9      19251926     19301931               39        Quakers  Philadelphia
    ## 10 10      19261927           NA                3        Rangers      New York
    ## 11 11      19261927           NA               16     Blackhawks       Chicago
    ## 12 12      19261927           NA               17      Red Wings       Detroit
    ## 13 13      19671968     19771978               49         Barons     Cleveland
    ## 14 14      19671968           NA               26          Kings   Los Angeles
    ## 15 15      19671968           NA               25          Stars        Dallas
    ## 16 16      19671968           NA                4         Flyers  Philadelphia
    ## 17 17      19671968           NA                5       Penguins    Pittsburgh
    ## 18 18      19671968           NA               19          Blues     St. Louis
    ## 19 19      19701971           NA                7         Sabres       Buffalo
    ## 20 20      19701971           NA               23        Canucks     Vancouver
    ## 21 21      19721973           NA               20         Flames       Calgary
    ## 22 22      19721973           NA                2      Islanders      New York
    ## 23 23      19741975           NA                1         Devils    New Jersey
    ## 24 24      19741975           NA               15       Capitals    Washington
    ## 25 25      19791980           NA               22         Oilers      Edmonton
    ## 26 26      19791980           NA               12     Hurricanes      Carolina
    ## 27 27      19791980           NA               21      Avalanche      Colorado
    ## 28 28      19791980           NA               53        Coyotes       Arizona
    ## 29 29      19911992           NA               28         Sharks      San Jose
    ## 30 30      19921993           NA                9       Senators        Ottawa
    ## 31 31      19921993           NA               14      Lightning     Tampa Bay
    ## 32 32      19931994           NA               24          Ducks       Anaheim
    ## 33 33      19931994           NA               13       Panthers       Florida
    ## 34 34      19981999           NA               18      Predators     Nashville
    ## 35 35      19992000           NA               52           Jets      Winnipeg
    ## 36 36      20002001           NA               29   Blue Jackets      Columbus
    ## 37 37      20002001           NA               30           Wild     Minnesota
    ## 38 38      20172018           NA               54 Golden Knights         Vegas
    ## 
    ## $total
    ## [1] 38

    print(knitr::kable(franchise()))

    ## 
    ## 
    ## <table class="kable_wrapper">
    ## <tbody>
    ##   <tr>
    ##    <td> 
    ## 
    ## | id| firstSeasonId| lastSeasonId| mostRecentTeamId|teamCommonName |teamPlaceName |
    ## |--:|-------------:|------------:|----------------:|:--------------|:-------------|
    ## |  1|      19171918|           NA|                8|Canadiens      |Montréal      |
    ## |  2|      19171918|     19171918|               41|Wanderers      |Montreal      |
    ## |  3|      19171918|     19341935|               45|Eagles         |St. Louis     |
    ## |  4|      19191920|     19241925|               37|Tigers         |Hamilton      |
    ## |  5|      19171918|           NA|               10|Maple Leafs    |Toronto       |
    ## |  6|      19241925|           NA|                6|Bruins         |Boston        |
    ## |  7|      19241925|     19371938|               43|Maroons        |Montreal      |
    ## |  8|      19251926|     19411942|               51|Americans      |Brooklyn      |
    ## |  9|      19251926|     19301931|               39|Quakers        |Philadelphia  |
    ## | 10|      19261927|           NA|                3|Rangers        |New York      |
    ## | 11|      19261927|           NA|               16|Blackhawks     |Chicago       |
    ## | 12|      19261927|           NA|               17|Red Wings      |Detroit       |
    ## | 13|      19671968|     19771978|               49|Barons         |Cleveland     |
    ## | 14|      19671968|           NA|               26|Kings          |Los Angeles   |
    ## | 15|      19671968|           NA|               25|Stars          |Dallas        |
    ## | 16|      19671968|           NA|                4|Flyers         |Philadelphia  |
    ## | 17|      19671968|           NA|                5|Penguins       |Pittsburgh    |
    ## | 18|      19671968|           NA|               19|Blues          |St. Louis     |
    ## | 19|      19701971|           NA|                7|Sabres         |Buffalo       |
    ## | 20|      19701971|           NA|               23|Canucks        |Vancouver     |
    ## | 21|      19721973|           NA|               20|Flames         |Calgary       |
    ## | 22|      19721973|           NA|                2|Islanders      |New York      |
    ## | 23|      19741975|           NA|                1|Devils         |New Jersey    |
    ## | 24|      19741975|           NA|               15|Capitals       |Washington    |
    ## | 25|      19791980|           NA|               22|Oilers         |Edmonton      |
    ## | 26|      19791980|           NA|               12|Hurricanes     |Carolina      |
    ## | 27|      19791980|           NA|               21|Avalanche      |Colorado      |
    ## | 28|      19791980|           NA|               53|Coyotes        |Arizona       |
    ## | 29|      19911992|           NA|               28|Sharks         |San Jose      |
    ## | 30|      19921993|           NA|                9|Senators       |Ottawa        |
    ## | 31|      19921993|           NA|               14|Lightning      |Tampa Bay     |
    ## | 32|      19931994|           NA|               24|Ducks          |Anaheim       |
    ## | 33|      19931994|           NA|               13|Panthers       |Florida       |
    ## | 34|      19981999|           NA|               18|Predators      |Nashville     |
    ## | 35|      19992000|           NA|               52|Jets           |Winnipeg      |
    ## | 36|      20002001|           NA|               29|Blue Jackets   |Columbus      |
    ## | 37|      20002001|           NA|               30|Wild           |Minnesota     |
    ## | 38|      20172018|           NA|               54|Golden Knights |Vegas         |
    ## 
    ##  </td>
    ##    <td> 
    ## 
    ## |  x|
    ## |--:|
    ## | 38|
    ## 
    ##  </td>
    ##   </tr>
    ## </tbody>
    ## </table>

    teamTotals <- function(){
      teamAPI <- GET(getUrl("-team-totals"))
      teamAPI #check connection
      
      teamText <- content(teamAPI, "text") #convert to JSON text form
      #teamText #check dataset
      teamList <- fromJSON(teamText, flatten=TRUE) #convert form
      #teamList #check dataset
      
      #gameTypeId = 2 means regular season games, gameTypeId = 3 means post-season games. Type =1 if you ever see that is preseason
      
      return(teamList)
    }
    #teamTotals()
    #print(knitr::kable(teamTotals()))

It is intended that you can either specify ID or the name. The intention
was to map the names the user gives to the IDs and then just use the ID
in the URL building. This means you should use a switch() or if else or
something to look at the ID input given, determine if it is a number
(use ID directly) or convert it from a franchise name to an ID. If you
can get it to work another way with the user specifying either input,
that is fine too!  
I wrote a function that incorporated filter() & returns data based on
user input of the team name. For ex, using the franchise table, if a
user inputs name=“Tigers” into my function’s arguments..

Wrapper Function
----------------

Exploratory Analysis
--------------------

    #do a join

    #create 2 new variables

    #create some contingency tables

    #create numerical summaries

    #create 5 plots
