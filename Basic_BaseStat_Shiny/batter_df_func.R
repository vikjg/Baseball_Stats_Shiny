batter_df <- function(df, top, game){
  library(dplyr)
  ins <- which(df$GamePk == game & df$IsTop == top)[1]
  off <- data.frame(BatterId = df[ins, "BatterId"], Pitches = 0, PA = 0, AB = 0, single = 0, double = 0, triple = 0, HR = 0, OB = 0, K = 0)
  for (i in 1:nrow(df)){
    #Game 1: Team1 Batting
    if (df[i, "IsTop"] == top & df[i, "GamePk"] == game){
      if (df[i, "BatterId"] %in% off$BatterId){
        j <- which(off$BatterId == df[i, "BatterId"])[1]
        if (df[i, "PitchType"] != "NULL"){
          off[[j, "Pitches"]] = off[[j, "Pitches"]] + 1
        }
        if (df[i, "PitchCall"] == "single"){
          off[[j, "single"]] = off[[j, "single"]] + 1
          off[[j, "PA"]] = off[[j, "PA"]] + 1
          off[[j, "AB"]] = off[[j, "AB"]] + 1
        }
        if (df[i, "PitchCall"] == "double"){
          off[[j, "double"]] = off[[j, "double"]] + 1
          off[[j, "PA"]] = off[[j, "PA"]] + 1
          off[[j, "AB"]] = off[[j, "AB"]] + 1
        }
        if (df[i, "PitchCall"] == "triple"){
          off[[j, "triple"]] = off[[j, "triple"]] + 1
          off[[j, "PA"]] = off[[j, "PA"]] + 1
          off[[j, "AB"]] = off[[j, "AB"]] + 1
        }
        if (df[i, "PitchCall"] == "home_run"){
          off[[j, "HR"]] = off[[j, "HR"]] + 1
          off[[j, "PA"]] = off[[j, "PA"]] + 1
          off[[j, "AB"]] = off[[j, "AB"]] + 1
        }
        if (df[i, "PitchCall"] == "walk"){
          off[[j, "OB"]] = off[[j, "OB"]] + 1
          off[[j, "PA"]] = off[[j, "PA"]] + 1
        }
        if (df[i, "PitchCall"] == "field_error"){
          off[[j, "OB"]] = off[[j, "OB"]] + 1
          off[[j, "PA"]] = off[[j, "PA"]] + 1
          off[[j, "AB"]] = off[[j, "AB"]] + 1
        }
        if (df[i, "PitchCall"] == "strikeout"){
          off[[j, "K"]] = off[[j, "K"]] + 1
          off[[j, "PA"]] = off[[j, "PA"]] + 1
          off[[j, "AB"]] = off[[j, "AB"]] + 1
        }
        if (df[i, "PitchCall"] %in% c("field_out", "force_out", "	grounded_into_double_play")){
          off[[j, "PA"]] = off[[j, "PA"]] + 1
          off[[j, "AB"]] = off[[j, "AB"]] + 1
        }
        if (df[i, "PitchCall"] == "sac_bunt"){
          off[[j, "PA"]] = off[[j, "PA"]] + 1
        }
      } else {
        off[nrow(off) + 1, ] = c(df[i, "BatterId"], 0, 0, 0, 0, 0, 0, 0, 0, 0)
        j <- which(off$BatterId == df[i, "BatterId"])[1]
        if (df[i, "PitchType"] != "NULL"){
          off[[j, "Pitches"]] = off[[j, "Pitches"]] + 1
        }
        if (df[i, "PitchCall"] == "single"){
          off[[j, "single"]] = off[[j, "single"]] + 1
          off[[j, "PA"]] = off[[j, "PA"]] + 1
          off[[j, "AB"]] = off[[j, "AB"]] + 1
        }
        if (df[i, "PitchCall"] == "double"){
          off[[j, "double"]] = off[[j, "double"]] + 1
          off[[j, "PA"]] = off[[j, "PA"]] + 1
          off[[j, "AB"]] = off[[j, "AB"]] + 1
        }
        if (df[i, "PitchCall"] == "triple"){
          off[[j, "triple"]] = off[[j, "triple"]] + 1
          off[[j, "PA"]] = off[[j, "PA"]] + 1
          off[[j, "AB"]] = off[[j, "AB"]] + 1
        }
        if (df[i, "PitchCall"] == "home_run"){
          off[[j, "HR"]] = off[[j, "HR"]] + 1
          off[[j, "PA"]] = off[[j, "PA"]] + 1
          off[[j, "AB"]] = off[[j, "AB"]] + 1
        }
        if (df[i, "PitchCall"] == "walk"){
          off[[j, "OB"]] = off[[j, "OB"]] + 1
          off[[j, "PA"]] = off[[j, "PA"]] + 1
        }
        if (df[i, "PitchCall"] == "field_error"){
          off[[j, "OB"]] = off[[j, "OB"]] + 1
          off[[j, "PA"]] = off[[j, "PA"]] + 1
          off[[j, "AB"]] = off[[j, "AB"]] + 1
        }
        if (df[i, "PitchCall"] == "strikeout"){
          off[[j, "K"]] = off[[j, "K"]] + 1
          off[[j, "PA"]] = off[[j, "PA"]] + 1
          off[[j, "AB"]] = off[[j, "AB"]] + 1
        }
        if (df[i, "PitchCall"] %in% c("field_out", "force_out", "	grounded_into_double_play")){
          off[[j, "PA"]] = off[[j, "PA"]] + 1
          off[[j, "AB"]] = off[[j, "AB"]] + 1
        }
        if (df[i, "PitchCall"] == "sac_bunt"){
          off[[j, "PA"]] = off[[j, "PA"]] + 1
        }
      }
    }
  }
  return(off)
}