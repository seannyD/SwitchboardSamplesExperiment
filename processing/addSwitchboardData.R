library(stringdist)
setwd("~/Documents/MPI/SemanticsPragmatics/2015Course/Projects/Slonimska/InitialPhonemeExperiment/SwitchboardSamplesExperiment/processing/")


normLev.fnc <- function(a, b, costs=NULL) {
  #dx = adist(a, b, counts=T, costs=costs)
  x = drop(adist(a, b,costs=costs) / nchar(attr(adist(a, b, counts=T), "trafos")))
  attr(x,'counts') <- NULL
  attr(x,'trafos') <- NULL
  return(x)
}

findTran = function(key, file){
  x = sw[sw$file==file,]
  dx = x[grepl(key,x$orthB),]
  print(dx$orthB)
  print(dx$id)
  
  dx =x[grepl(key,x$orthA),]
  print("ORTH A")
  print(dx$orthA)
  print(dx$id)
}

matchTran = function(samplesExtraData, tran,id, useA = F){
  x = samplesExtraData$transcription==tran
  y = sw$id==id
  
  if(useA){
    samplesExtraData[x,]$sex = sw[y,]$sexA
    samplesExtraData[x,]$timeInFile = sw[y,]$time
    samplesExtraData[x,]$speakerID = sw[y,]$spkA
    samplesExtraData[x,]$swTranscription = sw[y,]$orthA
  } else{
    samplesExtraData[x,]$sex = sw[y,]$sexB
    samplesExtraData[x,]$timeInFile = sw[y,]$time
    samplesExtraData[x,]$speakerID = sw[y,]$spkB
    samplesExtraData[x,]$swTranscription = sw[y,]$orthB
  }
  return(samplesExtraData)
}



# Link data to switchboard

allLab = read.csv("../Data/Lab_Processed.csv")

######
# Get data from switchboard

samples = read.csv("../Data/Stimuli_list.csv", stringsAsFactors = F)

sw = read.csv("~/Documents/MPI/Switchboard/NewTorreiraLubbersData/fto_utt_withSynDepthPLUSandPhones6.csv", stringsAsFactors = F)

mx = charmatch(tolower(samples$transcription),sw$orthB)

samplesExtraData = data.frame()

for(file in unique(samples$file)){
  swx = sw[sw$file==file,]
  sampx = samples[samples$file==file,]
  #distNorm = normLev.fnc(c(swx$orthA,swx$orthB),tolower(sampx$transcription))
  #distx = adist(swx$orthB,tolower(sampx$transcription))
  # distx = normLev.fnc(swx$orthB,tolower(sampx$transcription))
  
  #distx2 = stringdistmatrix(swx$orthB,tolower(sampx$transcription),method='lcs')
  
  #distx2 = distx2/ max(distx2)
    
  
  
  distx = adist(swx$orthB,tolower(sampx$transcription), costs=c(ins=1, del=0.5,sub=1))

  distxA = adist(swx$orthA,tolower(sampx$transcription), costs=c(ins=1, del=0.5,sub=1))
  
  matches = apply(distx , 2, function(X){which(X==min(X))})
  matches = sapply(matches, function(X){if(length(X)>1){return(X[1])} else{return(X)}})
  minB = apply(distx , 2, min)
  
  matchesA = apply(distxA , 2, function(X){which(X==min(X))})
  matchesA = sapply(matchesA, function(X){if(length(X)>1){return(X[1])} else{return(X)}})
  minA = apply(distxA , 2, min)
  
  for(i in 1:length(matches)){
    if(minB[i] <= minA[i]){
      sampx$sex[i]  = swx[matches[i],]$sexB
      sampx$timeInFile[i] = swx[matches[i],]$time
      sampx$speakerID[i] = swx[matches[i],]$spkB
      sampx$swTranscription[i] = swx[matches[i],]$orthB
    } else{
      sampx$sex[i]  = swx[matchesA[i],]$sexA
      sampx$timeInFile[i] = swx[matchesA[i],]$time
      sampx$speakerID[i] = swx[matchesA[i],]$spkA
      sampx$swTranscription[i] = swx[matchesA[i],]$orthA
    }
  }
  
  samplesExtraData = rbind(samplesExtraData, sampx)
}

samplesExtraData$er = abs(nchar(samplesExtraData$transcription) - nchar(samplesExtraData$swTranscription))

t(samplesExtraData[samplesExtraData$er>10 & samplesExtraData$er<=20 & !is.na(samplesExtraData$er),c("transcription","swTranscription",'file')])


####
# Manual fixes


#"well so many of them now eem"
findTran("so many","3223")
samplesExtraData = matchTran(samplesExtraData,"well so many of them now eem",33287)


findTran("liked to","3254")
samplesExtraData = matchTran(samplesExtraData,"well i think our i i never have liked to cook food",'34392')

findTran("while ago","3377")
samplesExtraData = matchTran(samplesExtraData,"quite a while ago it's probbaly up to 20 now if I",'37824', T)


samplesExtraData = matchTran(samplesExtraData,"there certainly been ideas surfaced uh recently um","37926")


findTran("bucks","3311")
samplesExtraData = matchTran(samplesExtraData,'right now i am getting around sixty bucks a month','35877')

findTran('ever been', "3254")
samplesExtraData = matchTran(samplesExtraData,"you ever been to Houston on Beltline",34378,T)

findTran("hobbies","3182" )
samplesExtraData = matchTran(samplesExtraData,"do you have any hobbies",32470, T)

findTran('revenue',"3387")
samplesExtraData = matchTran(samplesExtraData,"is it your expectation that that would raise the total revenues collected or or lower them or what","37995",T)

findTran("course",'3377')
samplesExtraData = matchTran(samplesExtraData,"do you have long waits uh to get on the course",37846)

findTran("Porsche",'3550')

findTran("Buick",'4339')
samplesExtraData = matchTran(samplesExtraData,"we have a Buick Century now",50844,T)

findTran("just",'3345')
samplesExtraData = matchTran(samplesExtraData,"just the events that happen arounf the world interest me",37000,T)

findTran("feel",'4812')
samplesExtraData = matchTran(samplesExtraData,"so how do you feel about it",54018,useA=T)

samplesExtraData = matchTran(samplesExtraData,"well you always",37063)
samplesExtraData = matchTran(samplesExtraData,"but that's the way it goes",36986,T)

samplesExtraData = matchTran(samplesExtraData,"they don't really do that",54077)


samplesExtraData = matchTran(samplesExtraData,"it was flat",41189)

findTran("what do","4104")
samplesExtraData = matchTran(samplesExtraData,"what do i do um",48145)

#findTran("expensive","3232")
#"um the Christian college was so much more expensive"



samplesExtraData$responseType = "none"
samplesExtraData$responseType[samplesExtraData$type %in% c("Non-Q1","Q1")] = "wh"
samplesExtraData$responseType[samplesExtraData$type %in% c("Non-Q2","Q2")] = "other"
samplesExtraData$responseSample = paste(samplesExtraData$responseType, samplesExtraData$Stimuli.ID)
samplesExtraData$responseSample[samplesExtraData$responseType == "none"] = "none"

samplesExtraData$contextSample = NA
samplesExtraData$contextSample[samplesExtraData$type=="Initial"] = "IN"
samplesExtraData$contextSample[samplesExtraData$type=="Statement"] = "ST"
samplesExtraData$contextSample[samplesExtraData$type=="Statement"] = "ST"

samplesExtraData$contextSample = paste(samplesExtraData$contextSample,samplesExtraData$Stimuli.ID)

write.csv(samplesExtraData, "../Data/SamplesExtraData.csv", row.names = F)


########
allLab$context.sex = samplesExtraData[match(allLab$contextSample,samplesExtraData$contextSample),]$sex
allLab$response.sex = samplesExtraData[match(allLab$responseSample,samplesExtraData$responseSample),]$sex

trx = gsub("\\[[^]]+\\]","",samplesExtraData$transcription)
trx = gsub(" ",'',trx)

samplesExtraData$response.firstO = substr(trx,0,1)
allLab$response.firstO= samplesExtraData[match(allLab$responseSample,samplesExtraData$responseSample),]$response.firstO

write.csv(allLab,"../Data/Lab_Processed.csv")


########

allDat = read.csv("../Data/Lab_and_Online_data_Processed.csv")


allDat$context.sex = samplesExtraData[match(allDat$contextSample,samplesExtraData$contextSample),]$sex
allDat$response.sex = samplesExtraData[match(allDat$responseSample,samplesExtraData$responseSample),]$sex

trx = gsub("\\[[^]]+\\]","",samplesExtraData$transcription)
trx = gsub(" ",'',trx)

samplesExtraData$response.firstO = substr(trx,0,1)
allDat$response.firstO= samplesExtraData[match(allDat$responseSample,samplesExtraData$responseSample),]$response.firstO

write.csv(allDat,"../Data/Lab_and_Online_data_Processed.csv")
