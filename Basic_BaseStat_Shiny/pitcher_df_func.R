pitcher_df <- function(df, top, game){
  library(dplyr)
  ins <- which(df$GamePk == game & df$IsTop == top)[1]
  t2g1_pitch <- data.frame(PitcherId = df[ins, "PitcherId"], PitchType = df[ins, "PitchType"], PitchCount = 0, B = 0, S = 0)
  for (i in 1:nrow(df)){
    if (df[i, "IsTop"] == top & df[i, "GamePk"] == game){
      if (df[i, "PitcherId"] %in% t2g1_pitch$PitcherId){
        j <- which(t2g1_pitch$PitcherId == df[i, "PitcherId"])
        v <- t2g1_pitch[j[1]:(j[1] + length(j)-1), ]
        if (df[i, "PitchType"] %in% v$PitchType){
          t2g1_pitch[[which(t2g1_pitch$PitchType == df[i, "PitchType"] & t2g1_pitch$PitcherId == df[i, "PitcherId"]), "PitchCount"]] = as.numeric(t2g1_pitch[[which(t2g1_pitch$PitchType == df[i, "PitchType"] & t2g1_pitch$PitcherId == df[i, "PitcherId"]), "PitchCount"]]) + 1
          if (df[i, "PitchCall"] == "ball" | df[i, "PitchCall"] == "wild_pitch" | df[i, "PitchCall"] == "blocked_ball"){
            t2g1_pitch[[which(t2g1_pitch$PitchType == df[i, "PitchType"] & t2g1_pitch$PitcherId == df[i, "PitcherId"]), "B"]] = as.numeric(t2g1_pitch[[which(t2g1_pitch$PitchType == df[i, "PitchType"] & t2g1_pitch$PitcherId == df[i, "PitcherId"]), "B"]]) + 1
          } else if(df[i, "PitchType"] != "NULL"){
            t2g1_pitch[[which(t2g1_pitch$PitchType == df[i, "PitchType"] & t2g1_pitch$PitcherId == df[i, "PitcherId"]), "S"]] = as.numeric(t2g1_pitch[[which(t2g1_pitch$PitchType == df[i, "PitchType"] & t2g1_pitch$PitcherId == df[i, "PitcherId"]), "S"]]) + 1
          }
        } else {
          t2g1_pitch[nrow(t2g1_pitch)+1,] = c(df[i, "PitcherId"], df[i, "PitchType"], 1, 0, 0)
          if (df[i, "PitchCall"] == "ball" | df[i, "PitchCall"] == "wild_pitch" | df[i, "PitchCall"] == "blocked_ball"){
            t2g1_pitch[[which(t2g1_pitch$PitchType == df[i, "PitchType"] & t2g1_pitch$PitcherId == df[i, "PitcherId"]), "B"]] = as.numeric(t2g1_pitch[[which(t2g1_pitch$PitchType == df[i, "PitchType"] & t2g1_pitch$PitcherId == df[i, "PitcherId"]), "B"]]) + 1
          } else if(df[i, "PitchType"] != "NULL"){
            t2g1_pitch[[which(t2g1_pitch$PitchType == df[i, "PitchType"] & t2g1_pitch$PitcherId == df[i, "PitcherId"]), "S"]] = as.numeric(t2g1_pitch[[which(t2g1_pitch$PitchType == df[i, "PitchType"] & t2g1_pitch$PitcherId == df[i, "PitcherId"]), "S"]]) + 1
          }
        }
      } else {
        t2g1_pitch[nrow(t2g1_pitch)+1,] = c(df[i, "PitcherId"], df[i, "PitchType"], 1, 0, 0)
        if (df[i, "PitchCall"] == "ball" | df[i, "PitchCall"] == "wild_pitch" | df[i, "PitchCall"] == "blocked_ball"){
          t2g1_pitch[[which(t2g1_pitch$PitchType == df[i, "PitchType"] & t2g1_pitch$PitcherId == df[i, "PitcherId"]), "B"]] = as.numeric(t2g1_pitch[[which(t2g1_pitch$PitchType == df[i, "PitchType"] & t2g1_pitch$PitcherId == df[i, "PitcherId"]), "B"]]) + 1
        } else if(df[i, "PitchType"] != "NULL"){
          t2g1_pitch[[which(t2g1_pitch$PitchType == df[i, "PitchType"] & t2g1_pitch$PitcherId == df[i, "PitcherId"]), "S"]] = as.numeric(t2g1_pitch[[which(t2g1_pitch$PitchType == df[i, "PitchType"] & t2g1_pitch$PitcherId == df[i, "PitcherId"]), "S"]]) + 1
        }
      }
    }
  }
  return(t2g1_pitch)
}