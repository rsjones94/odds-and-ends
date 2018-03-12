rm(list=ls())

library(pracma)

#set.seed(2)

##########

bkfWidth = 12
bkfDepth = 1
bottomWidth = bkfWidth/2

slopeRange = c(0.040,0.040)
nRange = c(0.050,0.08)

idealStats = c(12.72,1.05,12.3,26.28) # width, depth, area, volumetric flow rate, units are feet and seconds

wWeight = 2 # relative weights for width, depth, area and flow
dWeight = 1
aWeight = 3
qWeight = 4


numGenerations = 15000
popSize = 25
mutaters = 4
elitism = 0
tourneySize = 4

volatility = 1

##########

currentGen = 1

inSlope = runif(1,min=slopeRange[1],max=slopeRange[2])
inN = runif(1,min=nRange[1],max=nRange[2])

weights = c(wWeight,dWeight,aWeight,qWeight)

initialChannelSet = c(bkfWidth, bkfDepth, bottomWidth, inSlope, inN) # seed channel

repairChannel = function(channelSet) { # need something better than set back to 1
  
  if(channelSet[1] < 0) {
    channelSet[1] = 1
  }
  
  if(channelSet[2] < 0) {
    channelSet[2] = 1
  }
  
  if(channelSet[3] < 0) {
    channelSet[3] = 1
  }
  
  if(channelSet[3] > channelSet[1]) {
    channelSet[3] = channelSet[1]
  }
  
  if (channelSet[4] < min(slopeRange)) {
    channelSet[4] = min(slopeRange)
  } else if (channelSet[4] > max(slopeRange)) {
    channelSet[4] = max(slopeRange)
  }
  
  if (channelSet[5] < min(nRange)) {
    channelSet[5] = min(nRange)
  } else if (channelSet[5] > max(nRange)) {
    channelSet[5] = max(nRange)
  }
  
  return(channelSet)
  
}

peturbChannel = function(channelSet) {
  
  channelSet[1] = channelSet[1] + rnorm(1,mean=0,sd=channelSet[1]*volatility)
  channelSet[2] = channelSet[2] + rnorm(1,mean=0,sd=channelSet[2]*volatility)
  channelSet[3] = channelSet[3] + rnorm(1,mean=0,sd=channelSet[3]*volatility)
  
  channelSet[4] = channelSet[4] + rnorm(1,mean=0,sd=diff(slopeRange)*volatility*.25)
  channelSet[5] = channelSet[5] + rnorm(1,mean=0,sd=diff(nRange)*volatility*.25)
  
  channelSet = repairChannel(channelSet)
  
  return(channelSet)
}

##### Chanel data funcs

getChanW = function(channelSet) {
  
  width = channelSet[1]
  return(width)
  
}

getChanA = function(channelSet) {
  
  top = channelSet[1]
  depth = channelSet[2]
  bottom = channelSet[3]
  
  area = (top+bottom)/2 * depth
  return(area)
  
}

getChanD = function(channelSet) {
  
  area = getChanA(channelSet)
  width = getChanW(channelSet)
  depth = area / width
  return(depth)
  
}

getChanWP = function(channelSet) {
  
  top = channelSet[1]
  depth = channelSet[2]
  bottom = channelSet[3]
  
  deltaXSlope = (top-bottom)/2
  deltaySlope = depth
  
  slopeLength = sqrt(deltaXSlope^2 + deltaySlope^2)
  
  wettedPerimeter = bottom+(slopeLength*2)
  return(wettedPerimeter)
  
}

getChanQ = function(channelSet) { #this is the IMPERIAL eqn
  
  n = channelSet[5]
  slope = channelSet[4]
  area = getChanA(channelSet)
  wettedP = getChanWP(channelSet)
  hydRadius = area / wettedP
  
  flow = (1.49/n) * area * hydRadius^(2/3) * sqrt(slope)
  return(flow)
  
}

getChanStats = function(channelSet) { # dims are ft and seconds
  
  
  width = getChanW(channelSet)
  depth = getChanD(channelSet)
  area = getChanA(channelSet)
  flow = getChanQ(channelSet)
  
  statvec = c(width,depth,area,flow)
  return(statvec)
  
}

popChanStats = function(population) { # makes a matrix, each row the w/d/a/q for a population of channels
  
  statMat = matrix(0,nrow(population),4)
  for (i in 1:nrow(population)) {
    
    statMat[i,] = getChanStats(currentPop[i,])
    
  }
  return(statMat)
  
}

objFunction = function(ideal,actual,weights) { # will need a way to penalize roughness; lower obj function scores are better
  
  absoluteOff = actual - ideal
  perOff = absoluteOff / ideal
  weightedOff = perOff * weights
  
  distanceOff = dist(rbind(weightedOff,c(0,0,0,0)))
  
  
  return(as.numeric(distanceOff))
  
}

evaluatePopulationFitness = function(ideal,statMat,weights) {
  
  
  fitnessVec = matrix(0,nrow(statMat),1)
  for (i in 1:nrow(statMat)) {
    
    fitnessVec[i] = objFunction(ideal,statMat[i,],weights)
    
  }
  return(fitnessVec)
}

######## initialize the population
seed = initialChannelSet
secondChannel = peturbChannel(seed)
currentPop = rbind(seed,secondChannel)

for (i in 3:popSize) {
  
  nextChannel = peturbChannel(currentPop[1,])
  currentPop = rbind(currentPop,nextChannel)
  
}

row.names(currentPop) = 1:nrow(currentPop)
colnames(currentPop) = c("Bkf Width", "Bkf Depth", "Bottom Width", "Slope", "n")



recombine = function(channelListA,channelListB) {
  
  channelMat = rbind(channelListA,channelListB)
  
  chooseVec = c(1,2)
  sampler = sample(chooseVec,5,replace=TRUE)
  
  offspring = NULL
  for (i in 1:5) {
    offspring[i] = channelMat[sampler[i],i]
  }
  
  offspring = repairChannel(offspring)
  return(offspring)
  
}

channelAverage = function(channelListA,channelListB) {
  
  offspring = (channelListA+channelListB)/2
  
  offspring = repairChannel(offspring)
  return(offspring)
  
}

reproduce = function(channelListA,channelListB) {
  
  sampler = sample(c(TRUE,FALSE),1)
  
  if (sampler) {
    offspring = recombine(channelListA,channelListB)
  } else {
    offspring = channelAverage(channelListA,channelListB)
  }
  
  return(offspring)
  
}


tournament = function(orderedPopulation, orderedFitnessVector, tourneySize) {
  
  sampler = sample(1:length(orderedFitnessVector), tourneySize)
  winningNumber = min(sampler)
  winner = orderedPopulation[winningNumber,]
  return(winner)
  
}


meanFitnessThroughGens = NULL
minFitnessThroughGens = NULL
while (currentGen <= numGenerations) {
  
  stats = popChanStats(currentPop)
  fitness = evaluatePopulationFitness(idealStats,stats,weights)
  ranking = order(fitness)
  
  currentFitnessMean = mean(fitness)
  currentFitnessMin = min(fitness)
  meanFitnessThroughGens[currentGen] = currentFitnessMean
  minFitnessThroughGens[currentGen] = currentFitnessMin
  
  orderedFitness = fitness[ranking]
  orderedPopulation = currentPop[ranking,]
  
  numNewoffspring = popSize - elitism
  
  # tournament selection - two tournaments per offspring
  newGeneration = NULL
  
  for (i in 1:numNewoffspring) {
    
    tourneyWinnerOne = tournament(orderedPopulation,orderedFitness,tourneySize)
    tourneyWinnerTwo = tournament(orderedPopulation,orderedFitness,tourneySize)
    #offspring = reproduce(tourneyWinnerOne,tourneyWinnerTwo)
    offspring = recombine(tourneyWinnerOne,tourneyWinnerTwo)
    newGeneration = rbind(newGeneration,offspring)
    
  }
  
  for (i in 1:mutaters) {
    
    sampler = sample(1:nrow(newGeneration),1)
    newGeneration[sampler,] = peturbChannel(newGeneration[sampler,])
    
  }
  
  
  if (elitism != 0) {
    for (i in 1:elitism) {
      rbind(orderedPopulation[i,],newGeneration)
    }
  }
  
  
  currentGen = currentGen + 1
  if (currentGen%%20 == 0) {
    print(paste("Current gen: ", currentGen,". Mean fitness: ", round(currentFitnessMean,2),". Min Fitness: ", round(currentFitnessMin,2),".", sep=''))
  }
  
  currentPop = newGeneration
  
}

## finally

stats = popChanStats(currentPop)
fitness = evaluatePopulationFitness(idealStats,stats,weights)
ranking = order(fitness)

meanFitnessThroughGens[currentGen] = mean(fitness)
minFitnessThroughGens[currentGen] = min(fitness)

orderedStats = stats[ranking,]
orderedFitness = fitness[ranking]
orderedPopulation = currentPop[ranking,]


result = orderedPopulation[1,]
resultStats =  orderedStats[1,]

plot(meanFitnessThroughGens,type='l')
plot(minFitnessThroughGens,type='l')


constructChannel = function(channelSet) {
  
  depth = channelSet[2]
  bkfWidth = channelSet[1]
  bottomWidth = channelSet[3]
  
  deltaXSlope = (bkfWidth-bottomWidth)/2
  
  p1 = c(0,depth)
  p2 = c(deltaXSlope,0)
  p3 = c(deltaXSlope+bottomWidth,0)
  p4 = c(bkfWidth,depth)
  
  channel = rbind(p1,p2,p3,p4)
  return(channel)
  
}

resultChannel = constructChannel(result)
lim = max(resultChannel)
plot(resultChannel,type='l',xlim=c(0,lim),ylim=c(0,lim),lty=2)
