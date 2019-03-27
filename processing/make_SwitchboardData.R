setwd("~/Documents/MPI/SemanticsPragmatics/2015Course/Projects/Slonimska/InitialPhonemeExperiment/SwitchboardStudy/analysis/")

fn = "~/Documents/MPI/Switchboard/NewTorreiraLubbersData/fto_utt_withSynDepthPLUSandPhones6.csv"

d = read.csv(fn,stringsAsFactors = F)

d2 = d[,c('id','time','file','spkA','spkB','turnA','turnB','A.PP','B.PP','A.Scat','B.Scat','dialActA2.last','dialActB2.first','orthA','orthB','turnPhonesA','turnPhonesB','turnDurA','turnDurB')]

d2$gapType = cut(d$dur,c(-Inf,-300,0,300,Inf))


write.csv(d2,file='../Data/fto_utt_V6_FOR_AS.csv',row.names=F)

library(gplots)
plotmeans(d$rateB~cut(d$time,8))

plotmeans(d$turnDurA~cut(d$time/1000,seq(from=0,to=300,by = 50)),xlab='Time in conversation (seconds)',ylab= "Duration of turn (ms)",n.lab=F,main='Switchboard turn durations')

plot(table(cut(d$time/1000,seq(from=0,to=300,by = 25))))

plot(0:1,0:1)
for(f in unique(d$file)[1:100]){
  cx = cumsum(d[d$file==f,]$turnDurA)
  cx = cx /max(cx)
  lines(cx~seq(from=0,to=1,length.out=length(cx)),type='l',col=rgb(0,0,0,0.3))
}
abline(a=0,b=1)


summary(lm(turnDurB~turnDurA+time,data=d))

par(mfrow=c(2,2))
for(i in 2:5){
  
  x = log10(d$turnDurA[1:(nrow(d)-i+1)])
  y = log10(d$turnDurA[i:(nrow(d))])
  plot(
    x,
    y,
    pch=16,col=rgb(0,0,0,0.01),xlab=paste("Turn 0 (log ms)"),ylab=paste("Turn +",i-1,"(log ms)"), xaxt='n',yaxt='n')
  axis(1,at=c(2,3,4),labels=10^c(2,3,4))
  axis(2,at=c(2,3,4),labels=10^c(2,3,4))
  lmx = cor(x,y)
  abline(lm(x~y),col=2,lwd=2)
  text(30000,30000,paste("R=",round(lmx,3)))
}


d$TurnDur = d$turnDurB/1000
d$PrevTurnDur = d$turnDurA/1000
d$TurnType = as.factor(d$B.PP)
d$PrevTurnType = as.factor(d$A.PP)
d = d[d$TurnType!="",]

library(party)

cx = ctree(TurnDur~PrevTurnDur+TurnType,data=d)
plot(cx,inner_panel=node_inner(cx,id=F),terminal_panel=node_barplot(cx,id=F))
