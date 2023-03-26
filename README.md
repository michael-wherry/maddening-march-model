# Maddening-March-Model 🏀
## Contributors
* Houssam Hallouch
* Logan Pearce
* Devon Storm
* Michael Wherry

## Introduction ▶
This is a Machine Learning (ML) model designed to create a prediction on who can potentially be the champions for the 2023 NCAA March Madness tournament. To create a prediction, the model will use data from a list of selected metrics. These metrics will include the following:

1) Current seasons team stats: Analyzing past performance can offer valuable insights into a team's ability to perform well in the tournament.
2) Historical seasons team stats from 2008 to 2023: Examining historical data helps identify patterns, trends,strengths, and weaknesses of participating teams over time.

## R Script Directory 📜
*The folowing are two R srcipts that were used to clean data, create matchup predictions, and visualization through the use of a Shiny application.*
* Data cleaning and matchup predictions - QueryScript.R
* Shiny app and visualizations - app.r

## Dictionary 📕
*The following entries are the popular methods used amongst college basketball to calculate various team stats, which are present in our work.*
* Kenpom: This system demostrates how strong a team would be if it played tonight, independent of injuries or emotional factors
* Barttorvik: Very similar to Kenpom, but different in that it attempts to account for the various factors that Kenpom is independent from
* Barthag: Represents the chance a team has of beating any average Division 1 team in the country

## Data Cleaning 🧹
*The following examples of code were used to perform the necessary data cleaning and organization to ensure the input data is relevent and optimized for use by the SVM. These processes include creating valid date columns, replacing team ID numbers with their respective names, and seperating out current season and past season statistics.*
* Create a valid date column in our dataset
```r
df_tournament <- df_tournament %>%
  mutate(Day = lubridate::day(as.Date(DayNum, origin = paste0(Season, "-01-01")))) %>%
  mutate(Month = lubridate::month(as.Date(DayNum, origin = paste0(Season, "-01-01")))) %>%
  mutate(Date =  as.Date(paste(Season, Month, Day, sep = "/"))) %>%
  select(Date, Season, Month, Day, everything(), -DayNum)
  ```
 
 * Left join team names into our dataset as opposed to using the teams ID
 ```r
 df_historical_data <- df_tournament %>%
  left_join(df_teams_id_name, by = c("WTeamID" = "TeamID")) %>%
  select(-FirstD1Season, -LastD1Season) %>%
  rename(WTeam = TeamName) %>%
  left_join(df_teams_id_name, by = c("LTeamID" = "TeamID")) %>%
  select(-FirstD1Season, -LastD1Season, -WLoc) %>%
  rename(LTeam = TeamName)
  ```
  
  * Seperate historical teams stats from the current season's team stats (Kenpom, Barttorvik, Barthag)
  ```r
  df_team_data_historical <- filter(df_team_data, YEAR != 2023)
  ```

## SVM Model Functions 🧮
*The following examples of code were used to organized the data to be utilized by our SVM. Once the data was selected, we needed to train the SVM with the data in order to accurately make predications within the given parameters. In addition, after the SVM was trained, we performed a test to ensure the predication process was performing as intended.*
* Define the range of parameter values to search in the SVM
```r
 tune_params <- list(
   kernel = c("linear", "polynomial", "radial", "sigmoid"),
   gamma = c(0.1, 1, 5, 10),
   cost = c(0.1, 1, 10, 100)
 )
 ```
 * Perform a grid search with 5-fold cross-validation
 ```r
  tune_result <- tune(
   svm, 
   CHAMPION ~ . -YEAR -SEED -ROUND -TEAM, 
   data = df_team_data_historical,
   ranges = tune_params,
   tunecontrol = tune.control(cross = 5)
 )
 ```
 * Train the model with the best parameters
 ```r
 tournament_model <- svm(CHAMPION ~ . -YEAR -SEED -ROUND -TEAM, 
                        data = df_team_data_historical,
                        kernel = tune_result$best.parameters$kernel, 
                        gamma = tune_result$best.parameters$gamma, 
                        cost = tune_result$best.parameters$cost)
```
* Make predictions on the test set
```r
predictions <- predict(tournament_model, df_current_team_data)
```

## Model Predictions 📝
| Team | Prediction |
|------|------------|
| Alabama | 0.0119003369 |
| UConn | 0.0106910112 |
| Creighton | 0.0106004192 |
| Baylor | 0.0101194000 |
| Marquette | 0.0100960477 |
| Duke | 0.0098843257 |
| Memphis | 0.0098839214 |
| Texas | 0.0098106899 |
| Houston | 0.0098073528 |
| UCLA | 0.0097050529 |

## March Madness 2023 Predictions 📈
*Once the SVM was trained and testing was completed, the model was tasked with generating predictions for all 6 rounds of the Championship. Each round, the model would narrow down the brackets based on comparing statistics from the two teams placed within the brackets by the model.*

* To create the bracket within R studio we created keys within which to group teams (REGION, SEED, MATCHUP.KEY, GAME.KEY). 
* To determine a winner of the matchup, we compared the two teams models results and whicher team scored higher advanced to the next round
```r
df_matchup_predictions <- df_matchup_predictions %>%
  left_join(df_champions, by = c("TEAM" = "TEAM")) 
  
df_matchup_predictions_first_round <- df_matchup_predictions %>%
  select(REGION, MATCHUP.KEY, GAME.KEY, TEAM, SEED, PREDICTIONS) %>%
  arrange( REGION, MATCHUP.KEY, GAME.KEY) %>%
  mutate(WINNER = PREDICTIONS > lead(PREDICTIONS))
```
### First Round Results
<img src="images/First Round Winners.png" alt="First Round Winners" width="1600" height="300">

### Second Round Results
<img src="images/Second Round Results.png" alt="Second Round Winners" width="1600" height="300">

### Sweet 16 Results
<img src="images/Sweet 16 Winners.png" alt="Sweet 16 Winners" width="1600" height="300">

### Elite 8 Winners
<img src="images/Elite 8 Winners.png" alt="Elite 8 Winners" width="1600" height="300">

### Final Four Winners
<img src="images/Final Four Winners.png" alt="Final Four Winners" width="1600" height="300">

### National Champions
<img src="images/National Champions.png" alt="National Champions" width="1600" height="300">

## ShinyApp 📊

# Conclusion 🏀
We used both historical and current Kenpom, Barttorvik, and Barthag stats to determine the following predictions using the SVM created:
## Final Four Teams🥈
1) **Marquette**
2) **UConn**
3) **Alabama**
4) **Texas**

The model predicts that Marquette, UConn, Alabama, and Texas will be the teams playing in the Final Four.

## Championship Game Teams 🏅
1) **Alabama**
2) **UConn**

The model predicts that UConn and Alabama will be the teams at the Championship Game.

## National Champions 🏆
**Alabama**

The model predicts that Alabama will be the 2023 National Champions.


