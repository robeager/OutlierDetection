source("C:\\Users\\vmt8844\\OneDrive - AUT University\\R snippets\\setLibraries.r")

#====================== Set up framework ============================================

n = 100
w = 18
buckets = n-w+1

lowerBuffer = w
upperBuffer = (n-w)
minChange = 0
maxChange = 232
Range = maxChange - minChange

#=============== Clear data buckets ====================================
finalDF = NULL
minMax = NULL
Measurements = NULL
#Ranges = NULL


allRecords = c()
allRowIDs = c()
allChange = c()
allMinBits = c()
allMaxBits = c()
allVectors = c()
allOverlap = c()
allAlarms = c()
allTime = c()

#dfList = list()
#vectorList = list()


#=============== get data ======================

Measurements <- as.data.frame(read.csv(file = "C:\\Users\\vmt8844\\OneDrive - AUT University\\Research Project\\outlier-detection\\data\\hr.csv", header = TRUE, sep = ","))



names(Measurements) <- c(
  "Record",
  "Parameter",
  "HH",
  "mm",
  "Value",
  "InHospDeath"
)



Measurements <- Measurements %>%
  group_by(
    Record,
    Parameter
  ) %>%
  mutate(
    RowID = row_number(),
    t = ifelse(HH < 10, t <- as.character(paste0(0,HH,mm)), t <- as.character(paste0(HH,mm))),
    Change = sqrt((Value-lag(Value))^2),
    maxRowID = max(RowID)
  )

Measurements$Parameter <- as.character(paste(Measurements$Parameter))

rows = nrow(Measurements)

finalDF <- Measurements %>%
  mutate(
    I = (floor(buckets*(Change-minChange)/Range)),
    lowerValue = (I-(w/2)),
    upperValue = (I+(w/2))
  )

finalDF <- finalDF[which(!is.na(finalDF$Change)),]

#write.csv(finalDF,file = "C:\\Users\\vmt8844\\OneDrive - AUT University\\Research Project\\outlier-detection\\data\\measurements.csv")


recordID <- finalDF %>%
  ungroup() %>%
  distinct(
    Record
  )

#recordID$Parameter = NULL

totRecords = nrow(recordID)

#minMax <- data.frame(
#  Record = numeric(),
#  RowID = numeric(),
#  minBit = numeric(),
#  maxbit = numeric()
#)


#=============== Encode data streams ====================================

for (r in 1:totRecords) {
  thisRecord = as.numeric(recordID[r,1])
  record <- finalDF[which(finalDF$Record == thisRecord),]
  record$t <- as.numeric(record$t)
#  plot.ts(record$Change)
  totChanges = nrow(record)
  for (c in 1:totChanges) {
    if(c>1){lastMaxBit = maxBit}
    thisRowID = record$RowID[c]
    thisI = record$I[c]
    thisLowerValue = record$lowerValue[c]
    thisUpperValue = record$upperValue[c]
    minFlag = 0
    bitID = 0
    while (minFlag == 0) {
      if ((thisLowerValue<=bitID) & (bitID<thisUpperValue)) {
        minBit = bitID
        maxBit = (bitID+w)
        minFlag = 1
      }
      
      if ((thisLowerValue<minChange) & (bitID<lowerBuffer)) {
        minBit = bitID
        maxBit = (bitID+w)
        minFlag = 1
      }
      
      if ((thisUpperValue>maxChange) & (bitID>=upperBuffer)) {
        minBit = bitID
        maxBit = (bitID+w)
        minFlag = 1
      }
      bitID = bitID+1
    }
    
    thisVector = c()
    if(minBit > 0){
      thisVector[0:(minBit-1)] <- c(0)
      }
    thisVector[minBit:maxBit] = c(1)
    if (maxBit < (n-w)) {
      thisVector[(maxBit+1):n] <- c(0) 
    }
    
    #vectorList[[c]] <- thisVector
    
    
    if(c>1) {
      if ((sqrt((maxBit-lastMaxBit)^2)>w) || (minBit > lastMaxBit)) {
       overlap <- (0/w)
      } else {
        overlap <- ((w-(sqrt((lastMaxBit-maxBit)^2)))/w)
      }
    } else {
      overlap <- "New Record"
    }
    
    if (overlap == "New Record") {
      alarm <- "Alarm reset"
    } else if (overlap > 0.35) {
      alarm <- "OK"
    } else if (overlap >0.05 & overlap <= 0.35) {
      alarm <- "Orange"
    } else {
      alarm <- "RED!"
    }
    
#    if (alarm == "Alarm reset") {
#      cat(black(thisVector),'\n')
#    } else if (alarm == "OK") {
#      cat(blue(thisVector),'\n')
#    } else if (alarm == "Orange") {
#      cat(italic(yellow(thisVector)),'\n')
#    } else {
#      cat(bold(red(thisVector)),'\n')
#    }
    
    print(as.character(paste("RecordID =",thisRecord,"RowID =",thisRowID,"I =",thisI,"MinBit =",minBit,"MaxBit =",maxBit,"Overlap =",round(as.numeric(overlap),2),alarm)))

    allRecords = c(allRecords,thisRecord)
    allRowIDs = c(allRowIDs,thisRowID)
    allMinBits = c(allMinBits,minBit)
    allMaxBits = c(allMaxBits,maxBit)
    allVectors = c(allVectors,thisVector)
    allOverlap = c(allOverlap,overlap)
    allAlarms = c(allAlarms,alarm)
  }
}

allRecords <- as.data.frame(allRecords)
allRowIDs = as.data.frame(allRowIDs)
allMinBits = as.data.frame(allMinBits)
allMaxBits = as.data.frame(allMaxBits)
#allVectors = as.data.frame(allVectors)
allOverlap = as.data.frame(allOverlap)
allAlarms = as.data.frame(allAlarms)

minMax <- cbind(allRecords,allRowIDs,allMinBits,allMaxBits,allOverlap,allAlarms)

names(minMax) <- c(
  "Record",
  "RowID",
  "MinBit",
  "MaxBit",
  "Overlap",
  "Alarm"
)

finalDF <- left_join(finalDF,minMax, by = c("Record","RowID"))



write.csv(finalDF,file = "C:\\Users\\vmt8844\\OneDrive - AUT University\\Research Project\\outlier-detection\\data\\measurements_HR.csv")
