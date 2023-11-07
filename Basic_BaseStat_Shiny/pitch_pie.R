pitch_pie <- function(df){
  library(plotly)
  un <- unique(df["PitcherId"])
  pies <- list()
  for (i in 1:nrow(un)){
    j <- which(df$PitcherId == un[i,])
    pies[[i]] <- plot_ly(values = df[j[1]:(j[1]+length(j)-1), "pType"], labels = df[j[1]:(j[1]+length(j)-1), "PitchType"], title = paste("Pither Identity:", df[j[1], "PitcherId"]), type = "pie")
    
  }
  return(pies)
}