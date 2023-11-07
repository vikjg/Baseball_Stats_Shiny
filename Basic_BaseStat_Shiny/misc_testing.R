library(dplyr)
library(plotly)


pitch_data <- read.csv("PitchData.csv")

source("pitcher_df_func.R")
source("pitcher_perc.R")
source("pitcher_basics.R")
source("batter_df_func.R")
source("batter_comp.R")
source("pitch_pie.R")

#Game 1 Pitcher Data
t1g1p <- pitcher_df(pitch_data, "0", "1")
t2g1p <- pitcher_df(pitch_data, "1", "1")
t1g1pb <- pitcher_basics(pitch_data, "0", "1")
t2g1pb <- pitcher_basics(pitch_data, "1", "1")
t1g1perc <- pitcher_perc(t1g1p, t1g1pb)
t2g1perc <- pitcher_perc(t2g1p, t2g1pb)
pie_t1g1 <- pitch_pie(t1g1perc)
pie_t2g1 <- pitch_pie(t2g1perc)

#Game 2 Pitcher Data
t1g2p <- pitcher_df(pitch_data, "1", "2")
t2g2p <- pitcher_df(pitch_data, "0", "2")
t1g2pb <- pitcher_basics(pitch_data, "1", "2")
t2g2pb <- pitcher_basics(pitch_data, "0", "2")
t1g2perc <- pitcher_perc(t1g2p, t1g2pb)
t2g2perc <- pitcher_perc(t2g2p, t2g2pb)
pie_t1g2 <- pitch_pie(t1g2perc)
pie_t2g2 <- pitch_pie(t2g2perc)


length(pie_t1g1)
length(pie_t2g1)
length(pie_t1g2)
length(pie_t2g2)

p1=pie_t1g1[[1]]
p2=pie_t1g1[[2]]
p3=pie_t1g1[[3]]
p4=pie_t1g1[[4]]
p5=pie_t1g1[[5]]
p6=pie_t1g1[[6]]
p7=pie_t1g1[[7]]
fig <- subplot(p1,p2,p3,p4,p5,p6,p7, nrows = 3) %>%
  layout("Multiple Subplots with Titles")
fig
