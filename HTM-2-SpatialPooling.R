#======================= Start Spatial Pooling ===============================

#Settings

miniColumnCount = 2048 #Number of minicolumns available
connectedPercent = 0.85 #sets out how much of the input space is covered by the each column (randomly)
connectionThreshold = 0.5

#PRad = 631 #Potential Radius - how is this number calculated?
#LAD = -1 #Local Area Density WTF is this for?
#ST = 1 #stimulus threshold
#SPAI = 0.05 #Synaptic Permanence Active Increment - how much an active column's permanence is increased
#MPODC = 0.001 #Minimum Percent Overlap Duty Cycle - ?
#DCP = 1000 #Duty Cycle Period - Active Duty Cycle = the number of times a column has been active over a period of time
#GI = 1 #Global Inhibition - 1=On, 0=Off

#IAAC = 40 #Active columns per inhibition area
#SPID = 0.008 #Synaptic permanence inactive decrement - the amount that a connection decrements if not included for an input
#SPC = 0.1 # Synaptic permanence connected - the connection threshold
#MPADC = 0.001 #Minimum percent active duty cycle
#MaxBoost = 1

#======================================

#initialise connections

spList <- list()
permList <- list()

for (i in 1:miniColumnCount) {
  k <- sample.int((n+1),(connectedPercent*n))
  kl = length(k)
  for (j in 1:kl) {
    k[j] = (k[j]-1)
  }
  
  rn <- rnorm(n,connectionThreshold,sd = 1)
  ru <- round(pnorm(rn,mean = connectionThreshold, sd = 1),2)
  
  spList[[i]] <- k
  permList[[i]] <- ru
}

sp <- do.call(rbind,spList)
perm <- do.call(rbind,permList)

spDF <- as.data.frame(sp[,1:85])

spDF <- spDF %>%
  mutate(
    colNum = row_number()
  )

permDF <- as.data.frame(perm)

permDF <- permDF %>%
  mutate(
    colNum = row_number()
  )

sDF <- melt(spDF, id = "colNum")

names(sDF) <- c(
  "colNum",
  "allocationNum",
  "bitID"
)


pDF <- melt(permDF, id = "colNum")

names(pDF) <- c(
  "colNum",
  "allocationNum",
  "PermPct"
)

PA <- left_join(sDF,pDF, by = c("colNum","allocationNum"))

filename <- as.character(paste0("C:\\Users\\vmt8844\\OneDrive - AUT University\\Research Project\\outlier-detection\\data\\permanenceAllocation_initial-",connectionThreshold,".csv"))

write.csv(PA,file = filename)


#================================================

#get data input

#use thisVector from HTM-1-ScalarEncoder.r

data <- thisVector

