library(reshape)
library(gplots)

setwd("~/Documents/MPI/SemanticsPragmatics/2015Course/Projects/Slonimska/InitialPhonemeExperiment/SwitchboardSamplesExperiment/processing/")



getOrder = function(dx, dOld){
  # TODO: doesn't work with later experiment blocks
  # because they're coded only with ST_2
  
  newD = data.frame()
  
  for(id in unique(dx$partID)){
    dxx = dx[dx$partID==id,]
    dxx$variable = as.character(dxx$variable)
    
    orderCol = names(dOld)[grepl("DO.BL.SET",names(dOld))]
    
    xorder = dOld[dOld$partID==id, orderCol]
    xorder = strsplit(as.character(xorder[nchar(xorder)>1]),"\\|")[[1]]
    
    dxx$trialNumber = match(dxx$variable,xorder)
    newD = rbind(newD,dxx)
  }
  return(newD)
  
}


getData = function(filename, blockName, individualColumn = "Initials", qType=2){

  d = read.csv(filename, stringsAsFactors = F)
  names(d)[1:10] = d[1,][1:10]
  d = d[-1,]
  
  d$partID = d[,individualColumn]
  
  dOld = d
  
  d = d[,!names(d) %in% c("LocationLatitude","X","LocationLongitude",'LocationAccuracy')]
  d = d[,!grepl("DO\\.",names(d))]
  
  d = d[,!names(d)%in% c("Q2.1")]
  
  #strsplit(d$DO.BL.SET1[4],"\\|")[[1]]
  
  keepUpTo = c(names(d)[1:which(names(d)=="Start_Exp_Online")],"partID")
  
  dx = melt(d, id=keepUpTo)
  
  numCases = nrow(dx)
  dx = dx[dx$value!="",]
  numCases2 = nrow(dx)
  
  if(numCases!=numCases2){
    print("Dropped guesses")
    print(numCases - numCases2)
  }
  
  # get stimulus presentation order
  dx = getOrder(dx,dOld)
  
  #IN or ST
  #Q1-Q4 if blank, it's Q5(no second turn)
  #set
  
  dx$variable = gsub("Q","",dx$variable)
  
  varNames = as.character(unique(dx$variable))
  setNum = rep(rep(1:25,each=2),length.out=length(varNames))
  
  dx$setNum = setNum[match(dx$variable,varNames)]
  
  dx$context = "IN"
  dx$context[grepl("ST",dx$variable)] = "ST"
  
  if(qType==2){
    dx$question = sapply(dx$variable, function(X){
      
      test1 = strsplit(X,"\\.")[[1]][1]
      tx = table(strsplit(test1,"")[[1]])
      if(!"_" %in% names(tx)){
        return(5)
      } else{
        return(strsplit(test1,"_")[[1]][2])
      }
    })
  } else{
    dx$question = sapply(dx$variable, function(X){
      X = gsub("\\.","_",X)
      tx = table(strsplit(X,"")[[1]])
      if("_" %in% names(tx)){
        if(tx["_"]==1){
          # if only 1 underscore
          return(5)
        }
      }
      
      x  =strsplit(X,'_')[[1]]
      return(x[2])
      
    })
  }
  
  dx$question = c("whQ","oQ", "whNonQ","oNonQ","None")[as.numeric(dx$question)]
  dx$responsePhoneme = "wh"
  dx$responsePhoneme[dx$question=='oQ' | dx$question=="oNonQ"] = "other"
  dx$responsePhoneme[dx$question=='None'] = "none"
  
  dx$responseType = "other"
  dx$responseType[dx$question=='oQ' | dx$question=="whQ"] = "Q"
  dx$responseType[dx$question=='None'] = "none"
  
  dx$answer = c("No","Yes")[as.numeric(as.character(dx$value))]
  
  plotmeans(answer=="Yes"~question, data=dx)
  plotmeans(answer=="Yes"~context, data=dx)
  
  plotmeans(answer=="Yes"~paste(question,context), data=dx)
  
  dx$blockName = blockName
  
  dx = dx[order(dx$partID,dx$trialNumber),]
  
  dx$lastAnswer = c("No",dx$answer[1:(nrow(dx)-1)])
  dx[dx$trialNumber==1,]$lastAnswer = "No"  
  
  
  return(dx)
}

online = getData("../Data/Online_Version.csv","Online")

online = online[online$partID %in% names(table(online$partID))[table(online$partID)== 50],]

online$Sex[online$Sex %in% c("F",'female','Female',"FEMALE",'')] = "F"
online$Sex[online$Sex %in% c("M",'male','Male',"MALE")] = "M"

online$contextSample = paste(online$context,online$setNum)
online$responseSample = paste(online$responsePhoneme,online$setNum)

online$partID = paste("O",as.numeric(as.factor(online$partID)),sep='')
online = online[,!names(online) %in% c("Initials","IPAddress")]


write.csv(online,"../Data/OnlineData_Processed.csv")


#########

lab1 = getData("../Data/Dialogue_1.csv","Block1", "ID", qType=1)
lab2 = getData("../Data/Dialogue_2.csv","Block2", "ID", qType=1)
lab3 = getData("../Data/Dialogue_3.csv","Block3", "ID", qType=1)
lab4 = getData("../Data/Dialogue_4.csv","Block4", "ID", qType=1)
lab5 = getData("../Data/Dialogue_5.csv","Block5", "ID", qType=1)

allLab = rbind(lab1,lab2,lab3,lab4,lab5)

sexColumn = allLab$Sex
allLab$Sex = NA
allLab$Sex[sexColumn %in% c("F",'female',"Female")] = "F"
allLab$Sex[sexColumn %in% c("M",'male',"Male")] = "M"

allLab$Nationality[allLab$Nationality=="Canadian "] = "Canadian"
allLab$Nationality[allLab$Nationality=="UK"] = "British"

allLab$Nationality[allLab$Nationality=="dutch"] = "Dutch"

allLab$EnglishType = allLab$Nationality
allLab$EnglishType[allLab$EnglishType %in% c("Dutch","Indian","Swiss, Australian, Belgian","Australian")] = "Other"
allLab$EnglishType[allLab$EnglishType %in% c("USA and Latvia","USA/LV","Latvian and USA","Canadian/Latvian","Canadian","American","Latvian, USA")] = "American"
allLab$EnglishType[allLab$EnglishType %in% c("British-American")] = "British"

allLab$Nationality2 = allLab$Nationality
allLab$Nationality2[allLab$Nationality2 %in% c("Dutch",'Swiss, Australian, Belgian')] = "European"

allLab$Nationality3 = allLab$Nationality2
allLab$Nationality3[allLab$Nationality3 %in% c("British-American","American", "Canadian")] = "Am"
allLab$Nationality3[allLab$Nationality3 %in% c("European","Indian","Australian")] = "Other"


allLab$contextSample = paste(allLab$context,allLab$setNum)
allLab$responseSample = paste(allLab$responsePhoneme,allLab$setNum)
# set the response sample to 'none' if none is included
allLab$responseSample[allLab$responsePhoneme=="none"] = "none"


allLab[allLab$partID=='16',]$Sex = "F"

allLab = allLab[,!names(allLab) %in% c("Initials","IPAddress")]
write.csv(allLab,"../Data/Lab_Processed.csv")




pdf("../results/graphs/IndividualResults.pdf", width=13,height=13)
par(mfrow=c(5,5))
for(part in sort(unique(allLab$partID))){
  plotmeans(answer=="Yes" ~ paste(context, responsePhoneme), 
            data = allLab[allLab$partID==part,],
            main = part,
            ylim=c(0,1))
}
dev.off()

#########

allLab$ESource = "Lab"
online$ESource = "online"

cols = intersect(names(online),names(allLab))


allD = rbind(allLab[,cols], online[,cols])
allD = allD[,!names(allD) %in% c("Initials","IPAddress")]

write.csv(allD,"../Data/Lab_and_Online_data_Processed.csv", row.names = F)

# REMEMBER TO RUN addSwitchboardData.R !!!!