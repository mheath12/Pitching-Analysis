
library(Lahman)
library(dplyr)
library(ggplot2)
library(data.table)
Lahman<-LahmanData
pitchingSplit<-split(Pitching,Pitching$yearID)
sc <- fread("https://raw.githubusercontent.com/bayesball/ABWRdata/master/data/statcast2017.txt")



##FIRST PITCH STRIKES
table(sc$description)
fp<- subset(sc, pitch_number==1)
#subsets first pitches

firstpitch<- function(df,x){
  Pitcher<-subset(df,pitcher == x)
  firststrike<-subset(Pitcher, description %in% c("called_strike", "foul","foul_bunt","foul_tip","hit_into_play","hit_into_play_no_out","hit_into_play_score","missed_bunt","swinging_strike","swinging_strike_blocked"))
  z<-nrow(firststrike)/nrow(Pitcher)
  return(c(x,z))
}
#calculates first pitch strikes for desired pitcher

firstpitch(fp, 477132)
#example of function using Kershaw

fpsperc<-data.frame()
fpsperc<-rbind(fpsperc,firstpitch(fp,477132))
#adds result of function to data frame

fpsperc<-data.frame()
for(i in unique(fp$pitcher)){
  firstpitch(fp,i)
  fpsperc<-rbind(fpsperc,firstpitch(fp,i))
}
#data frame with first pitch strike percentages for every pitcher

pitching2017<-pitchingSplit$`2017`
playerids<-read.csv("/Users/matthewheath/Downloads/master.csv")
colnames(pitching2017)[1]<-"lahman_id"
pitching2017<-aggregate(. ~lahman_id,data=pitching2017,sum)
pitching2017$IP<-pitching2017$IPouts/3
pitching2017$Kper9<-9*(pitching2017$SO/pitching2017$IP)
pitching2017$BBper9<-9*(pitching2017$BB/pitching2017$IP)
pitching2017$ERA<-9*(pitching2017$ER/pitching2017$IP)
pitching2017<-merge(x=pitching2017,y= playerids[,c("mlb_id","mlb_name","lahman_id")], by = "lahman_id")
#created data frame with 2017 season stats for every pitcher

colnames(fpsperc)[1]<-"mlb_id"
colnames(fpsperc)[2]<-"FPS"
pitching2017<-merge(x=pitching2017,y=fpsperc,by = "mlb_id")
#added first pitch strike percentage to season stats

qualpitch2017<- filter(pitching2017, IP>=25)
#filtered out smallest sample sizes

ggplot(qualpitch2017,aes(FPS,ERA))+geom_point()+geom_smooth()+labs(title="Figure 1")+theme(plot.title = element_text(hjust = 0.5))
#visual of effect of FPS% on ERA

summary(lm(qualpitch2017$ERA~qualpitch2017$FPS))
#Low R squared indicates much variability when forecasting

ggplot(qualpitch2017,aes(FPS,Kper9))+geom_point()+geom_smooth()+labs(title="Figure 2")+theme(plot.title = element_text(hjust = 0.5))
summary(lm(qualpitch2017$Kper9~qualpitch2017$FPS))
#very weak realtionship between k/9 and FPS%

ggplot(qualpitch2017,aes(FPS,BBper9))+geom_point()+geom_smooth()+labs(title="Figure 3")+theme(plot.title = element_text(hjust = 0.5))
summary(lm(qualpitch2017$BBper9~qualpitch2017$FPS))
#as FPS increases BB/9 definitively decreases

##EXIT VELOCITY
oppxvelo<- function(df,x){
  Pitcher<-subset(df,pitcher == x)
  xvelo<-mean(Pitcher$launch_speed, na.rm=TRUE)
  return(c(x,xvelo))
}
#Calculates opponents exit velocity against for given pitcher

oppxvelo(sc,477132)

xvelo<-data.frame()
for(i in unique(sc$pitcher)){
  oppxvelo(sc,i)
  xvelo<-rbind(xvelo,oppxvelo(sc,i))
}
#created data frame with exit velocities for every player

colnames(xvelo)[1]<-"mlb_id"
colnames(xvelo)[2]<-"xvelo"
pitching2017<-merge(x=pitching2017,y=xvelo,by = "mlb_id")
#added exit velocities to season stats

qualpitch2017<- filter(pitching2017, IP>=25)
#filtered out smallest sample sizes

ggplot(qualpitch2017,aes(FPS,xvelo))+geom_point()+geom_smooth()
#visual effect of first pitch strikes on opponents exit velocity

summary(lm(qualpitch2017$xvelo~qualpitch2017$FPS))
#no significant relationship between exit velo and fPS%

##SPIN RATE

difpitch<-aggregate(sc$release_spin_rate,by=list(sc$pitch_type,sc$pitcher),data=sc,FUN=mean,na.rm=TRUE)
#Created data frame with average spin rate of each pitch for every pitcher

colnames(difpitch)[1]<-"type"
colnames(difpitch)[2]<-"mlb_id"
colnames(difpitch)[3]<-"spin_rate"
difpitch<-merge(x=difpitch,y= pitching2017[,c("mlb_id","mlb_name")])

velo<-aggregate(sc$release_speed,by=list(sc$pitch_type,sc$pitcher),data=sc,FUN=mean,na.rm=TRUE)
#created data frame with average velocity of each pitch for every pitcher

colnames(velo)[1]<-"type"
colnames(velo)[2]<-"mlb_id"
colnames(velo)[3]<-"velocity"
difpitch<-merge(x=difpitch,y= velo)
#combined velocity and spin rate data frames

exvelo<-aggregate(sc$launch_speed,by=list(sc$pitch_type,sc$pitcher),data=sc,FUN=mean,na.rm=TRUE)
#created data frame with average exit velocity against of each pitch for every pitcher

colnames(exvelo)[1]<-"type"
colnames(exvelo)[2]<-"mlb_id"
colnames(exvelo)[3]<-"exit_velocity"
difpitch<-merge(x=difpitch,y= exvelo)
#added exit velocity to existing data frame

ggplot(difpitch,aes(spin_rate,exit_velocity))+geom_point()+geom_smooth()+facet_wrap(~ type)+labs(title="Figure 4")+theme(plot.title = element_text(hjust = 0.5))
#visual of relationship between exit velocity and spin rate

difpitchsplit<-split(difpitch,difpitch$type)
#created separate data frames for each pitch

FF<-difpitchsplit$FF
summary(lm(FF$exit_velocity~FF$spin_rate))
SL<-difpitchsplit$SL
summary(lm(SL$exit_velocity~SL$spin_rate))

ggplot(difpitch,aes(spin_rate,velocity))+geom_point()+geom_smooth()+facet_wrap(~ type)+labs(title="Figure 5")+theme(plot.title = element_text(hjust = 0.5))
#visual of relationship between spin rate and velocity for each pitch

summary(lm(FF$spin_rate~FF$velocity))
summary(lm(SL$spin_rate~SL$velocity))

ext<-aggregate(sc$release_extension,by=list(sc$pitch_type,sc$pitcher),data=sc,FUN=mean,na.rm=TRUE)
#created data frame with average extension of each pitch for every pitcher

colnames(ext)[1]<-"type"
colnames(ext)[2]<-"mlb_id"
colnames(ext)[3]<-"extension"
difpitch<-merge(x=difpitch,y= ext)

rvelo<-aggregate(sc$effective_speed,by=list(sc$pitch_type,sc$pitcher),data=sc,FUN=mean,na.rm=TRUE)
#created data frame with average effective velocity of each pitch for every pitcher

colnames(rvelo)[1]<-"type"
colnames(rvelo)[2]<-"mlb_id"
colnames(rvelo)[3]<-"effective_velocity"
difpitch<-merge(x=difpitch,y= rvelo)

ggplot(difpitch,aes(extension,effective_velocity))+geom_point()+geom_smooth()+facet_wrap(~ type)+labs(title="Figure 6")+theme(plot.title = element_text(hjust = 0.5))

difpitchsplit<-split(difpitch,difpitch$type)
#created separate data frames for each pitch

FF<-difpitchsplit$FF
summary(lm(FF$effective_velocity~FF$extension))
summary(lm(FF$exit_velocity~FF$extension))
summary(lm(FF$exit_velocity~FF$effective_velocity))

ggplot(FF,aes(effective_velocity,exit_velocity))+geom_point()+geom_smooth()+labs(title="Figure 7")+theme(plot.title = element_text(hjust = 0.5))



