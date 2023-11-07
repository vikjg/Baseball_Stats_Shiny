pitcher_perc <- function(df, df2){
  types = unique(df["PitchType"])
  perc <- data.frame(PitcherId = df["PitcherId"], PitchType = df["PitchType"], pType = 0, pS = 0)
  for (i in 1:nrow(df)){
    perc[i, "pType"] <- c(as.numeric(df[[i, "PitchCount"]])/as.numeric(df2[[which(df2$PitcherId == df[i, "PitcherId"])[1], "Pitches"]]))
    perc[i, "pS"] <- c(as.numeric(df[[i, "S"]])/as.numeric(df[[i, "PitchCount"]]))
  }
  perc1 <- subset(perc, PitchType != "NULL")
  return(perc1)
}