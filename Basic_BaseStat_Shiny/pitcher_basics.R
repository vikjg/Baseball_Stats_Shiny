pitcher_basics <- function(df, top, game){
  library(dplyr)
  ins <- which(df$GamePk == game & df$IsTop == top)[1]
  pit <- data.frame(PitcherId = df[ins, "PitcherId"], Pitches = 0, BB = 0, H = 0, HR = 0, K = 0, batters_faced = 0)
  for (i in 1:nrow(df)){
    #Game 1: Team1 Batting
    if (df[i, "IsTop"] == top & df[i, "GamePk"] == game){
      if (df[i, "PitcherId"] %in% pit$PitcherId){
        j <- which(pit$PitcherId == df[i, "PitcherId"])[1]
        if (df[i, "PitchType"] != "NULL"){
          pit[[j, "Pitches"]] = pit[[j, "Pitches"]] + 1
        }
        if (df[i, "PitchCall"] %in% c("sac_bunt", "field_out", "force_out", "grounded_into_double_play")){
          pit[[j, "batters_faced"]] = pit[[j, "batters_faced"]] + 1
        }
        if (df[i, "PitchCall"] == "strikeout"){
          pit[[j, "K"]] = pit[[j, "K"]] + 1
          pit[[j, "batters_faced"]] = pit[[j, "batters_faced"]] + 1
        }
        if (df[i, "PitchCall"] %in% c("single", "double", "triple")){
          pit[[j, "H"]] = pit[[j, "H"]] + 1
          pit[[j, "batters_faced"]] = pit[[j, "batters_faced"]] + 1
        }
        if (df[i, "PitchCall"] == "home_run"){
          pit[[j, "HR"]] = pit[[j, "HR"]] + 1
          pit[[j, "batters_faced"]] = pit[[j, "batters_faced"]] + 1
        }
        if (df[i, "PitchCall"] == "walk"){
          pit[[j, "BB"]] = pit[[j, "BB"]] + 1
          pit[[j, "batters_faced"]] = pit[[j, "batters_faced"]] + 1
        }
      } else {
        pit[nrow(pit) + 1, ] = c(df[i, "PitcherId"], 0, 0, 0, 0, 0, 0)
        j <- which(pit$PitcherId == df[i, "PitcherId"])[1]
        if (df[i, "PitchType"] != "NULL"){
          pit[[j, "Pitches"]] = pit[[j, "Pitches"]] + 1
        }
        if (df[i, "PitchCall"] %in% c("sac_bunt", "field_out", "force_out", "grounded_into_double_play")){
          pit[[j, "batters_faced"]] = pit[[j, "batters_faced"]] + 1
        }
        if (df[i, "PitchCall"] == "strikeout"){
          pit[[j, "K"]] = pit[[j, "K"]] + 1
          pit[[j, "batters_faced"]] = pit[[j, "batters_faced"]] + 1
        }
        if (df[i, "PitchCall"] %in% c("single", "double", "triple")){
          pit[[j, "H"]] = pit[[j, "H"]] + 1
          pit[[j, "batters_faced"]] = pit[[j, "batters_faced"]] + 1
        }
        if (df[i, "PitchCall"] == "home_run"){
          pit[[j, "HR"]] = pit[[j, "HR"]] + 1
          pit[[j, "batters_faced"]] = pit[[j, "batters_faced"]] + 1
        }
        if (df[i, "PitchCall"] == "walk"){
          pit[[j, "BB"]] = pit[[j, "BB"]] + 1
          pit[[j, "batters_faced"]] = pit[[j, "batters_faced"]] + 1
        }
      }
    }
  }
  return(pit)
}
        
        
