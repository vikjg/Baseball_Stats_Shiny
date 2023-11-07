batter_comp <- function(df){
  stats = data.frame(BatterId = df["BatterId"])
  ba <- (df[["single"]]+df[["double"]]+df[["triple"]]+df[["HR"]])/df[["AB"]]
  obp <- (df[["single"]]+df[["double"]]+df[["triple"]]+df[["HR"]]+df[["OB"]])/df[["PA"]]
  slg <- (df[["single"]]+2*df[["double"]]+3*df[["triple"]]+4*df[["HR"]])/df[["AB"]]
  ops <- obp + slg
  stats["BA"] <- ba
  stats["OBP"] <- obp
  stats["SLG"] <- slg
  stats["OPS"] <- ops
  return(stats)
}