library (ggplot2)
library(dplyr)
library(ggrepel)
library(tidyverse)
#read data
mydata  <- read.csv("~/Downloads/ASA All NBA Raw Data.csv")
#This is the data frame for us to calculate all the metrics
#remove the variables that our group not interested
NBA = subset(mydata, select = -c(Inactives, starter, plus_minus, triple_double, 
                                 DKP_per_minute, FDP_per_minute, SDP_per_minute,
                                 pf_per_minute, last_60_minutes_per_game_starting,
                                last_60_minutes_per_game_bench, active_position_minutes))
#This is the data frame to see the total games in NBA till now, then we can 
#analyze them.
new <- mydata[!duplicated(mydata$game_id),  ] 
#if the team pace affects the victory?
  new$pace_affect = ifelse(new$Team_pace > new$Opponent_pace, 
                      ifelse(new$Team_off_rtg > new$Opponent_off_rtg, 1, 0),
                      #return true if team score > opponent score when the condition is true
                      #else return false when team score < opponent when the condition is true
                      ifelse(new$Team_off_rtg > new$Opponent_off_rtg, 0, 1)) 
  mean(new$pace_affect)
#if the team effective goal affect the victory?
   new$efg_affect = ifelse(new$Team_efg_pct > new$Opponent_efg_pct 
                            ,
                          ifelse(new$Team_Score > new$Opponent_Score, 1, 0),
                          ifelse(new$Team_Score > new$Opponent_Score, 0, 1)) 
   mean(new$efg_affect)
   #Turn over effect
   NBA$tov_affect = ifelse(NBA$Team_tov_pct > NBA$Opponent_tov_pct, 
                           ifelse(NBA$Team_Score > NBA$Opponent_Score, 1, 0),
                           ifelse(NBA$Team_Score > NBA$Opponent_Score, 0, 1)) 
   mean(NBA$tov_affect)
   #Offensive rebound effect 
   NBA$orb_affect = ifelse(NBA$Team_orb_pct > NBA$Opponent_orb_pct, 
                           ifelse(NBA$Team_Score > NBA$Opponent_Score, 1, 0),
                           ifelse(NBA$Team_Score > NBA$Opponent_Score, 0, 1))
   mean(NBA$orb_affect)
   
  #team off rtg  a team's offensive performance
   # points scored per 100 possesions.
   #A team is in possession when a player is holding, dribbling or passing the ball.
   NBA$off_rtg_affect = ifelse(NBA$Team_off_rtg > NBA$Opponent_off_rtg, 
                           ifelse(NBA$Team_Score > NBA$Opponent_Score, 1, 0),
                           ifelse(NBA$Team_Score > NBA$Opponent_Score, 0, 1)) 
   mean(NBA$off_rtg_affect)
   #free throw affect
    NBA$fr_affect = ifelse(NBA$Team_ft_rate > NBA$Opponent_ft_rate, 
                          ifelse(NBA$Team_Score > NBA$Opponent_Score, 1, 0),
                          ifelse(NBA$Team_Score > NBA$Opponent_Score, 0, 1)) 
   mean(NBA$fr_affect)
   #from this calculation we can conclude that offensive rebound, team off rtg, and free throw 
   # are the factors that we can predict which team win based on that.
 #if Home or away affect the victory? 
 #Referee bias and the psychological impact of playing at home are two of the biggest factors.
   new$ha_affect = ifelse(new$H_A == 'H', 
                          ifelse(new$Team_Score > new$Opponent_Score, "Win", "Lose"),
                          ifelse(new$Team_Score > new$Opponent_Score, "Lose", "Win")) 
   mean(new$ha_affect)
#each team's index , based on player's id 
UTA_index <- which(NBA$player_id == "mitchdo01")#1  UTA Utah Jazz
DAL_index <- which(NBA$player_id == "finnedo01") #2  DAL Dallas Mavericks
GSW_index <- which(NBA$player_id == "moodymo01") #3 GSW   Golden State Warriors
DEN_index <- which(NBA$player_id == "riverau01") #4  DEN  Denver Nuggets
ATL_index <- which(NBA$player_id == "youngtr01") #5 ATL Atlanta Hawks
DET_index <- which(NBA$player_id == "cunnica01")  #6  DET  Detroit Pistons
HOU_index <- which(NBA$player_id == "greenja05") #7 HOU   Houston Rockets
MIA_index <-  which(NBA$player_id == "robindu01")  #8 MIA  Miami Heat 
ORL_index <- which(NBA$player_id == "fraziti01")  #9   ORL  Orlando Magic
POR_index <-  which(NBA$player_id == "watfotr01") #10   POR  Portland Trail Blazers
MIN_index <-  which(NBA$player_id == "townska01") #11   MIN   Minnesota Timberwolves
CHI_index <-  which(NBA$player_id == "dosunay01")  #12  CHI  Chicago Bulls
PHI_index <-  which(NBA$player_id == "harrito02") #13    PHI  Philadelphia 76ers
NYK_index <-  which(NBA$player_id == "barrerj01") #14    NYK  New York Knicks
SAC_index <-  which(NBA$player_id == "foxde01")  #15      SAC  Sacramento Kings
LAL_index <-  which(NBA$player_id == "monkma01")  #16     LAL   Los Angeles Lakers
SAS_index <-  which(NBA$player_id == "murrade01") #17     SAS   San Antonio Spurs
BRK_index <-  which(NBA$player_id == "claxtni01")  #18   BRK   Brooklyn Nets
BOS_index <-  which(NBA$player_id == "williro04")  #19    BOS  Boston Celtics
TOR_index <-  which(NBA$player_id == "siakapa01")   #20   TOR   Toronto Raptors
CLE_index <- which(NBA$player_id == "garlada01")   #21    CLE  Cleveland Cavaliers
NOP_index <- which(NBA$player_id == "mccolcj01")  #22     NOP  New Orleans Pelicans
MEM_index <- which(NBA$player_id == "jacksja02")  #23    MEM  Memphis Grizzlies
LAC_index <- which(NBA$player_id == "coffeam01")    #24   LAC  Los Angeles Clippers
PHO_index <- which(NBA$player_id == "mcgeeja01")    #25   PHO  Phoenix Suns
MIL_index <- which(NBA$player_id == "middlkh01")   #26    MIL  Milwaukee Bucks
OKC_index <- which(NBA$player_id == "gilgesh01")   #27     OKC  Oklahoma City Thunder
IND_index <- which(NBA$player_id == "brissos01")   #28    IND Indiana Pacers
WAS_index <- which(NBA$player_id == "kuzmaky01") #29     WAS  Washington Wizards
CHO_index <- which(NBA$player_id == "washipj01") #30 CHO  Charlotte Hornets
#performance of each team
#effective goal pct
as.double( NBA$Team_efg_pct[LAC_index])
UTA_efg <- mean(NBA$Team_efg_pct[UTA_index])
DAL_efg <- mean(NBA$Team_efg_pct[DAL_index])
GSW_efg <- mean(NBA$Team_efg_pct[GSW_index])
DEN_efg <- mean(NBA$Team_efg_pct[DEN_index])
ATL_efg <- mean(NBA$Team_efg_pct[ATL_index])
DET_efg <- mean(NBA$Team_efg_pct[DET_index])
HOU_efg <- mean(NBA$Team_efg_pct[HOU_index])
MIA_efg <- mean(NBA$Team_efg_pct[MIA_index])
ORL_efg <- mean(NBA$Team_efg_pct[ORL_index])
POR_efg <- mean(NBA$Team_efg_pct[POR_index])
MIN_efg <- mean(NBA$Team_efg_pct[MIN_index])
CHI_efg <- mean(NBA$Team_efg_pct[CHI_index])
PHI_efg <- mean(NBA$Team_efg_pct[PHI_index])
NYK_efg <- mean(NBA$Team_efg_pct[NYK_index])
SAC_efg <- mean(NBA$Team_efg_pct[SAC_index])
LAL_efg <- mean(NBA$Team_efg_pct[LAL_index])
SAS_efg <- mean(NBA$Team_efg_pct[SAS_index])
BRK_efg <- mean(NBA$Team_efg_pct[BRK_index])
BOS_efg <- mean(NBA$Team_efg_pct[BOS_index])
TOR_efg <- mean(NBA$Team_efg_pct[TOR_index])
CLE_efg <- mean(NBA$Team_efg_pct[CLE_index])
NOP_efg <- mean(NBA$Team_efg_pct[NOP_index])
MEM_efg <- mean(NBA$Team_efg_pct[MEM_index])
LAC_efg <- mean(NBA$Team_efg_pct[LAC_index])
PHO_efg <- mean(NBA$Team_efg_pct[PHO_index])
MIL_efg <- mean(NBA$Team_efg_pct[MIL_index])
OKC_efg <- mean(NBA$Team_efg_pct[OKC_index])
IND_efg <- mean(NBA$Team_efg_pct[IND_index])
WAS_efg <- mean(NBA$Team_efg_pct[WAS_index])
CHO_efg <- mean(NBA$Team_efg_pct[CHO_index])
#Team possessions
UTA_pace <- mean(NBA$Team_pace[UTA_index])
DAL_pace <- mean(NBA$Team_pace[DAL_index])
GSW_pace <- mean(NBA$Team_pace[GSW_index])
DEN_pace <- mean(NBA$Team_pace[DEN_index])
ATL_pace <- mean(NBA$Team_pace[ATL_index])
DET_pace <- mean(NBA$Team_pace[DET_index])
HOU_pace <- mean(NBA$Team_pace[HOU_index])
MIA_pace <- mean(NBA$Team_pace[MIA_index])
ORL_pace <- mean(NBA$Team_pace[ORL_index])
POR_pace <- mean(NBA$Team_pace[POR_index])
MIN_pace <- mean(NBA$Team_pace[MIN_index])
CHI_pace <- mean(NBA$Team_pace[CHI_index])
PHI_pace <- mean(NBA$Team_pace[PHI_index])
NYK_pace <- mean(NBA$Team_pace[NYK_index])
SAC_pace <- mean(NBA$Team_pace[SAC_index])
LAL_pace <- mean(NBA$Team_pace[LAL_index])
SAS_pace <- mean(NBA$Team_pace[SAS_index])
BRK_pace <- mean(NBA$Team_pace[BRK_index])
BOS_pace <- mean(NBA$Team_pace[BOS_index])
TOR_pace <- mean(NBA$Team_pace[TOR_index])
CLE_pace <- mean(NBA$Team_pace[CLE_index])
NOP_pace <- mean(NBA$Team_pace[NOP_index])
MEM_pace <- mean(NBA$Team_pace[MEM_index])
LAC_pace <- mean(NBA$Team_pace[LAC_index])
PHO_pace <- mean(NBA$Team_pace[PHO_index])
MIL_pace <- mean(NBA$Team_pace[MIL_index])
OKC_pace <- mean(NBA$Team_pace[OKC_index])
IND_pace <- mean(NBA$Team_pace[IND_index])
WAS_pace <- mean(NBA$Team_pace[WAS_index])
CHO_pace <- mean(NBA$Team_pace[CHO_index])
#Team off rtg 
UTA_off_rtg <- mean(NBA$Team_off_rtg[UTA_index])
DAL_off_rtg <- mean(NBA$Team_off_rtg[DAL_index])
GSW_off_rtg <- mean(NBA$Team_off_rtg[GSW_index])
DEN_off_rtg <- mean(NBA$Team_off_rtg[DEN_index])
ATL_off_rtg <- mean(NBA$Team_off_rtg[ATL_index])
DET_off_rtg <- mean(NBA$Team_off_rtg[DET_index])
HOU_off_rtg <- mean(NBA$Team_off_rtg[HOU_index])
MIA_off_rtg <- mean(NBA$Team_off_rtg[MIA_index])
ORL_off_rtg <- mean(NBA$Team_off_rtg[ORL_index])
POR_off_rtg <- mean(NBA$Team_off_rtg[POR_index])
MIN_off_rtg <- mean(NBA$Team_off_rtg[MIN_index])
CHI_off_rtg <- mean(NBA$Team_off_rtg[CHI_index])
PHI_off_rtg <- mean(NBA$Team_off_rtg[PHI_index])
NYK_off_rtg <- mean(NBA$Team_off_rtg[NYK_index])
SAC_off_rtg <- mean(NBA$Team_off_rtg[SAC_index])
LAL_off_rtg <- mean(NBA$Team_off_rtg[LAL_index])
SAS_off_rtg <- mean(NBA$Team_off_rtg[SAS_index])
BRK_off_rtg <- mean(NBA$Team_off_rtg[BRK_index])
BOS_off_rtg <- mean(NBA$Team_off_rtg[BOS_index])
TOR_off_rtg <- mean(NBA$Team_off_rtg[TOR_index])
CLE_off_rtg <- mean(NBA$Team_off_rtg[CLE_index])
NOP_off_rtg <- mean(NBA$Team_off_rtg[NOP_index])
MEM_off_rtg <- mean(NBA$Team_off_rtg[MEM_index])
LAC_off_rtg <- mean(NBA$Team_off_rtg[LAC_index])
PHO_off_rtg <- mean(NBA$Team_off_rtg[PHO_index])
MIL_off_rtg <- mean(NBA$Team_off_rtg[MIL_index])
OKC_off_rtg <- mean(NBA$Team_off_rtg[OKC_index])
IND_off_rtg <- mean(NBA$Team_off_rtg[IND_index])
WAS_off_rtg <- mean(NBA$Team_off_rtg[WAS_index])
CHO_off_rtg <- mean(NBA$Team_off_rtg[CHO_index])



#Team_tov_pct team turn over %
#calculate mean of each team
UTA_tov <- mean(NBA$Team_tov_pct[UTA_index])
DAL_tov <- mean(NBA$Team_tov_pct[DAL_index])
GSW_tov <- mean(NBA$Team_tov_pct[GSW_index])
DEN_tov <- mean(NBA$Team_tov_pct[DEN_index])
ATL_tov <- mean(NBA$Team_tov_pct[ATL_index])
DET_tov <- mean(NBA$Team_tov_pct[DET_index])
HOU_tov <- mean(NBA$Team_tov_pct[HOU_index])
MIA_tov <- mean(NBA$Team_tov_pct[MIA_index])
ORL_tov <- mean(NBA$Team_tov_pct[ORL_index])
POR_tov <- mean(NBA$Team_tov_pct[POR_index])
MIN_tov <- mean(NBA$Team_tov_pct[MIN_index])
CHI_tov <- mean(NBA$Team_tov_pct[CHI_index])
PHI_tov <- mean(NBA$Team_tov_pct[PHI_index])
NYK_tov <- mean(NBA$Team_tov_pct[NYK_index])
SAC_tov <- mean(NBA$Team_tov_pct[SAC_index])
LAL_tov <- mean(NBA$Team_tov_pct[LAL_index])
SAS_tov <- mean(NBA$Team_tov_pct[SAS_index])
BRK_tov <- mean(NBA$Team_tov_pct[BRK_index])
BOS_tov <- mean(NBA$Team_tov_pct[BOS_index])
TOR_tov <- mean(NBA$Team_tov_pct[TOR_index])
CLE_tov <- mean(NBA$Team_tov_pct[CLE_index])
NOP_tov <- mean(NBA$Team_tov_pct[NOP_index])
MEM_tov <- mean(NBA$Team_tov_pct[MEM_index])
LAC_tov <- mean(NBA$Team_tov_pct[LAC_index])
PHO_tov <- mean(NBA$Team_tov_pct[PHO_index])
MIL_tov <- mean(NBA$Team_tov_pct[MIL_index])
OKC_tov <- mean(NBA$Team_tov_pct[OKC_index])
IND_tov <- mean(NBA$Team_tov_pct[IND_index])
WAS_tov <- mean(NBA$Team_tov_pct[WAS_index])
CHO_tov <- mean(NBA$Team_tov_pct[CHO_index])
#Team_orb_pct  Offensive Rebound Percentage of each team 
UTA_orb <- mean(NBA$Team_orb_pct[UTA_index])
DAL_orb <- mean(NBA$Team_orb_pct[DAL_index])
GSW_orb <- mean(NBA$Team_orb_pct[GSW_index])
DEN_orb <- mean(NBA$Team_orb_pct[DEN_index])
ATL_orb <- mean(NBA$Team_orb_pct[ATL_index])
DET_orb <- mean(NBA$Team_orb_pct[DET_index])
HOU_orb <- mean(NBA$Team_orb_pct[HOU_index])
MIA_orb <- mean(NBA$Team_orb_pct[MIA_index])
ORL_orb <- mean(NBA$Team_orb_pct[ORL_index])
POR_orb <- mean(NBA$Team_orb_pct[POR_index])
MIN_orb <- mean(NBA$Team_orb_pct[MIN_index])
CHI_orb <- mean(NBA$Team_orb_pct[CHI_index])
PHI_orb <- mean(NBA$Team_orb_pct[PHI_index])
NYK_orb <- mean(NBA$Team_orb_pct[NYK_index])
SAC_orb <- mean(NBA$Team_orb_pct[SAC_index])
LAL_orb <- mean(NBA$Team_orb_pct[LAL_index])
SAS_orb <- mean(NBA$Team_orb_pct[SAS_index])
BRK_orb <- mean(NBA$Team_orb_pct[BRK_index])
BOS_orb <- mean(NBA$Team_orb_pct[BOS_index])
TOR_orb <- mean(NBA$Team_orb_pct[TOR_index])
CLE_orb <- mean(NBA$Team_orb_pct[CLE_index])
NOP_orb <- mean(NBA$Team_orb_pct[NOP_index])
MEM_orb <- mean(NBA$Team_orb_pct[MEM_index])
LAC_orb <- mean(NBA$Team_orb_pct[LAC_index])
PHO_orb <- mean(NBA$Team_orb_pct[PHO_index])
MIL_orb <- mean(NBA$Team_orb_pct[MIL_index])
OKC_orb <- mean(NBA$Team_orb_pct[OKC_index])
IND_orb <- mean(NBA$Team_orb_pct[IND_index])
WAS_orb <- mean(NBA$Team_orb_pct[WAS_index])
CHO_orb <- mean(NBA$Team_orb_pct[CHO_index])
#how many game the team win among all the games they played
NBA$result_wl = ifelse(NBA$Team_Score > NBA$Opponent_Score, "Win", 'Lose')
UTA_win = ifelse(NBA$result_wl[UTA_index] == "Win", 1 , 0 )
DAL_win = ifelse(NBA$result_wl[DAL_index] == "Win", 1 , 0 )
GSW_win = ifelse(NBA$result_wl[GSW_index] == "Win", 1 , 0 )
DEN_win = ifelse(NBA$result_wl[DEN_index] == "Win", 1 , 0 )
ATL_win = ifelse(NBA$result_wl[ATL_index] == "Win", 1 , 0 )
DET_win = ifelse(NBA$result_wl[DET_index] == "Win", 1 , 0 )
HOU_win = ifelse(NBA$result_wl[HOU_index] == "Win", 1 , 0 )
MIA_win = ifelse(NBA$result_wl[MIA_index] == "Win", 1 , 0 )
ORL_win = ifelse(NBA$result_wl[ORL_index] == "Win", 1 , 0 )
POR_win = ifelse(NBA$result_wl[POR_index] == "Win", 1 , 0 )
MIN_win = ifelse(NBA$result_wl[MIN_index] == "Win", 1 , 0 )
CHI_win = ifelse(NBA$result_wl[CHI_index] == "Win", 1 , 0 )
PHI_win = ifelse(NBA$result_wl[PHI_index] == "Win", 1 , 0 )
NYK_win = ifelse(NBA$result_wl[NYK_index] == "Win", 1 , 0 )
SAC_win = ifelse(NBA$result_wl[SAC_index] == "Win", 1 , 0 )
LAL_win = ifelse(NBA$result_wl[LAL_index] == "Win", 1 , 0 )
SAS_win = ifelse(NBA$result_wl[SAS_index] == "Win", 1 , 0 )
BRK_win = ifelse(NBA$result_wl[BRK_index] == "Win", 1 , 0 )
BOS_win = ifelse(NBA$result_wl[BOS_index] == "Win", 1 , 0 )
TOR_win = ifelse(NBA$result_wl[TOR_index] == "Win", 1 , 0 )
CLE_win = ifelse(NBA$result_wl[CLE_index] == "Win", 1 , 0 )
NOP_win = ifelse(NBA$result_wl[NOP_index] == "Win", 1 , 0 )
MEM_win = ifelse(NBA$result_wl[MEM_index] == "Win", 1 , 0 )
LAC_win = ifelse(NBA$result_wl[LAC_index] == "Win", 1 , 0 )
PHO_win = ifelse(NBA$result_wl[PHO_index] == "Win", 1 , 0 )
MIL_win = ifelse(NBA$result_wl[MIL_index] == "Win", 1 , 0 )
OKC_win = ifelse(NBA$result_wl[OKC_index] == "Win", 1 , 0 )
WAS_win = ifelse(NBA$result_wl[WAS_index] == "Win", 1 , 0 )
IND_win = ifelse(NBA$result_wl[IND_index] == "Win", 1 , 0 )
CHO_win = ifelse(NBA$result_wl[CHO_index] == "Win", 1 , 0 )
#the new data frame for us to easy to plot 
graph_frame <- data.frame (team_name  = c("UTA", "DAL", "GSW", "DEN","ATL",
                                          "DET", "HOU", "MIA", "ORL", "POR",
                                          "MIN", "CHI", "PHI", "NYK", "SAC",
                                          "LAL", "SAS", "BRK", "BOS", "TOR",
                                          "CLE", "NOP", "MEM", "LAC", "PHO", 
                                          "MIL", "OKC", "IND", "WAS", "CHO"),
                  team_efg <- c(
                    0.5606875, 0.5300172, 0.5550385,0.5485686,
                    0.5472593,0.4908824, 0.5165455, 0.547,
                    0.4975833, 0.5244359, 0.5288929, 0.5436034, 
                    0.5287115, 0.507, 0.5279091, 0.5348, 0.5270189, 
                    0.5170857, 0.52898, 0.5134043, 0.5408298,0.5376829,
                    0.5179677, 0 ,0.5472143,0.5403846,0.4880652,
                    0.5264194, 0.5234035, 0.5345208
                  ),
   
                               team_pace <- c( 97.28958,  94.78966,  97.85769,  96.89412,  97.36667,
                                  97.53725,  101.1,  94.91017,  95.90833,  97.29231, 
                                  99.52143,  98.0569,  95.46731,  95.12549,  99.20182, 
                                  99.27455,  98.96415,  98.46,  95.986,  95.22979,  95.44255, 
                                  97.19756,  99.17581,  97.84483,  98.8625,  99.21154, 
                                  98.11522, 96.77419,  96.25614,  99.88125),
                  winning_rate <- c( 0.6875, 0.5862069  , 0.7307692  , 0.6078431  , 0.5  ,
                                     0.2745098  , 0.1590909  , 0.6779661  , 0.1666667  , 0.3846154  ,
                                     0.5535714  , 0.6034483  , 0.6346154  , 0.4313725  , 0.4  ,
                                     0.4727273  , 0.4150943  , 0.4285714  , 0.58  , 0.5319149  , 
                                     0.6170213  , 0.4390244  , 0.6774194  , 0.4909091  , 0.8035714,
                                     0.6153846  , 0.326087  , 0.3387097  , 0.4561404  , 0.4583333),
                  team_off_rtg <- c( 118.2792  ,111.6466  ,113.0269  ,113.3314  ,116.763  ,
                                     106.2471  ,105.3795  ,114.4169  ,106.725  ,109.2179  ,
                                     113.7196  ,114.2069  ,112.85  ,108.7647  ,111.7836  ,
                                     110.5255  ,112.5151  ,111.0143  ,112.498  ,113.2809  ,
                                     112.1383  ,112.1927  ,114.3452  ,107.94  ,114.5214  ,
                                     114.5212  ,103.987  ,112.0532  ,110.3316  ,113.725))
#Create a table of matrix to graph team's performance stacked bar chart 

 data_eastern <- read.table(text="
Team,  performance, percentage
ATL, team_efg, 54.72593
ATL, team_orb, 22.42222
ATL, team_off_rtg, 116.763
DET, team_efg, 49.08824
DET, team_orb, 23.12745
DET, team_off_rtg, 106.2471
MIA, team_efg, 54.7
MIA, team_orb, 24.17119
MIA, team_off_rtg, 114.4169
ORL, team_efg, 49.7
ORL, team_orb, 20.60833
ORL, team_off_rtg, 106.725
CHI, team_efg,  54.36034
CHI, team_orb, 20.32931
CHI, team_off_rtg, 114.2069
PHI, team_efg, 52.87115
PHI, team_orb, 18.93269
PHI, team_off_rtg, 112.85
NYK, team_efg, 50.7
NYK, team_orb, 24.6098
NYK, team_off_rtg,108.7647
BRK, team_efg, 51.70857
BRK, team_orb, 23.84286
BRK, team_off_rtg, 111.0143
BOS, team_efg, 52.898
BOS, team_orb, 23.84286
BOS, team_off_rtg, 112.498
TOR, team_efg, 51.34043
TOR, team_orb, 27.37447
TOR, team_off_rtg, 113.2809
CLE, team_efg, 54.08298
CLE, team_orb, 24.11064
CLE, team_off_rtg, 112.1383
MIL, team_efg, 54.03846
MIL, team_orb, 22.92115
MIL, team_off_rtg, 114.5212
IND, team_efg, 52.64194
IND, team_orb, 24.88871
IND, team_off_rtg, 112.0532
WAS, team_efg, 52.34035
WAS, team_orb, 21.10702
WAS, team_off_rtg, 110.3316
CHO, team_efg, 53.45208
CHO, team_orb, 23.85625
CHO, team_off_rtg, 113.725"
     , header=TRUE, sep=",")
 data_western <- read.table(text="
Team,  performance, percentage
UTA, team_efg,  56.1
UTA, team_off_rtg, 118.28
UTA, team_orb, 24.83
DAL, team_efg, 53
DAL, team_orb, 21.5
DAL, team_off_rtg, 111.65
GSW, team_efg, 55.5
GSW, team_orb,22.375
GSW, team_off_rtg, 113.03
DEN, team_efg, 54.8
DEN, team_orb, 21.99608
DEN, team_off_rtg, 113.3314
HOU, team_efg, 49.1
HOU, team_orb, 20.93636
HOU, team_off_rtg, 105.3795
POR, team_efg, 52.44359
POR, team_orb, 21.55128
POR, team_off_rtg, 109.2179
MIN, team_efg, 52.88929
MIN, team_orb,24.66964
MIN, team_off_rtg, 113.7196
SAC, team_efg, 52.79091
SAC, team_orb, 22.37455
SAC, team_off_rtg, 111.7836
LAL, team_efg, 53.48
LAL, team_orb, 21.62727
LAL, team_off_rtg, 110.5255
SAS, team_efg, 52.70189
SAS, team_orb, 23.58679
SAS, team_off_rtg,112.5151
NOP, team_efg, 53.76829
NOP, team_orb, 22.46585
NOP, team_off_rtg,112.1927
MEM, team_efg, 51.79677
MEM, team_orb, 30.18548
MEM, team_off_rtg, 114.3452
LAC, team_efg, 52.48727
LAC, team_orb, 19.10545
LAC, team_off_rtg, 107.94
PHO, team_efg, 54.72143
PHO, team_orb, 22.0375
PHO, team_off_rtg, 114.5214
OKC, team_efg, 48.80652
OKC, team_orb,21.23043
OKC, team_off_rtg, 103.987"
, header=TRUE, sep=",")

pie <- ggplot(new, aes(x = "", fill = factor(ha_affect))) + 
   geom_bar(width = 1) +
   theme(axis.line = element_blank(), 
         plot.title = element_text(hjust=0.5)) + 
   labs(fill="Win or Lose", 
        x=NULL, 
        y=NULL, 
        title="Pie Chart of Home affect winning "
        )
  pie + coord_polar(theta = "y", start=0)



ggplot(data_western, aes(x=reorder(Team, -percentage), y=percentage, fill= performance)) +
   geom_bar(stat="identity", position="stack") +
   theme(axis.text.x = element_text(angle = 90)) +  labs(fill="Team Performance", 
                                                         x= "Team names", 
                                                         y= "Rate", 
                                                         title="Team Performance on Western side") + 
   scale_y_continuous(sec.axis=sec_axis(~.*0.01)) + scale_fill_brewer(palette = 17)+
   theme( plot.title = element_text(hjust=0.5))
 ggplot(data_eastern, aes(x= reorder(Team, -percentage), y=percentage, fill= performance)) +
   geom_bar(stat="identity", position="stack") +
   theme(axis.text.x = element_text(angle = 90)) +  labs(fill="Team Performance", 
                                                         x= NULL, 
                                                         y= NULL, 
                                                         title="Eastern side") + 
   scale_y_continuous(sec.axis=sec_axis(~.*0.01,name="Percentage")) + scale_fill_brewer(palette = 17)+
 theme( plot.title = element_text(hjust=0.5))
