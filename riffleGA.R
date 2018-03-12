rm(list=ls())

library(pracma)

set.seed(2)

## NEED TO SAVE KINKINESS TO VECTOR

#use below to create the channel seed

inWidth = 21 # units are feet
inDepth = 1.8

idealStats = c(21,1.8,36,85) # width, depth, area, volumetric flow rate, units are feet and seconds

slopeRange = c(0.030,0.060)
nRange = c(0.050,0.075)

numPoints = 21 #must be odd
# evaluating kinkiness is O(n)

wWeight = 2 # relative weights for width, depth, area and flow
dWeight = 1
aWeight = 3
qWeight = 4

volatility = 4 # muliplier for muvec

numGenerations = 2500
popSize = 50
mutaters = 5
elitism = 2
tourneySize = 5
muvec = c(0.1,0.1,0.1,0.1,0.1,0.25,0.25,0.25,0.25,0.25,0.5,0.5,0.5,1,1,2)*volatility

absSmoothness = 2 # 0 for no kink penalty
centerPenalty = 5 # kink penalty multiplier will be increase linearly from 1 (needs addressing) to 1+centerPenalty at center


##########
currentGen = 1

inSlope = runif(1,min=slopeRange[1],max=slopeRange[2])
inN = runif(1,min=nRange[1],max=nRange[2])

weights = c(wWeight,dWeight,aWeight,qWeight)

inFirstPoint = c(0,inDepth)
inMidPoint = c(inWidth/2,0)

leftChannel = approx(cbind(inFirstPoint,inMidPoint), n = (numPoints-1)/2+1)

leftChannel = cbind(leftChannel$x,leftChannel$y) # x,z

initialChannelList = list(leftChannel, inSlope, inN) #channel morph, slope, manning's n - the seed

# randomly peturb seed model to create initial population

orderChannel = function(channel) {
  
  copy = channel
  copyX = copy[order(copy[,1]),1]
  copyY = rev(copy[order(copy[,2]),2])
  
  copy = cbind(copyX,copyY)
  return(copy)
}


peturbChannel = function(channel,strength) { # strength alters impulse - strength = 1 means the sd of the impulse is the width or depth of channel

  copy = channel
  maxDeltaX = max(copy[,1])/nrow(copy)
  maxDeltaY = max(copy[,2])/nrow(copy)
  
  for (i in 1:nrow(copy)) {
    
    copy[i,1] = copy[i,1] + rnorm(1,mean=0,sd=maxDeltaX*strength)
    copy[i,2] = copy[i,2] + rnorm(1,mean=0,sd=maxDeltaY*strength)
    
  }
  
  # for (i in 1:nrow(copy)) {
  #   
  #   if (copy[i,1] < 0) {
  #     copy[i,1] = 0
  #   }
  #   if (copy[i,2] < 0) {
  #     copy[i,2] = 0
  #   }
  #   
  # }
  copy = orderChannel(copy)
  
  if (copy[1,1] != 0) {
    copy[,1] = copy[,1] - copy[1,1]
  }
  if (copy[nrow(copy),2] != 0) {
    copy[,2] = copy[,2] - copy[nrow(copy),2]
  }
  
  return(copy)
}

peturbSlope = function(slope,range,strength){ # strength = 1 means 1 sd of the impulse is the range of the slope
  
  slope = slope+rnorm(1,mean=0,sd=diff(range)*strength)
  if (slope < min(slopeRange)) {
    slope = min(slopeRange)
  } else if (slope > max(slopeRange)) {
    slope = max(slopeRange)
  }
  return(slope)
}

peturbN = function(n,range,strength){ # strength = 1 means 1 sd of the impulse is the range of the n
  
  n = n+rnorm(1,mean=0,sd=diff(range)*strength)
  if (n < min(range)) {
    n = min(range)
  } else if (n > max(range)) {
    n = max(range)
  }
  return(n)
}

fullyPeturb = function(channelList,slopeRange,nRange,strength) {
  
  newChannel = list(peturbChannel(channelList[[1]],strength),peturbSlope(channelList[[2]],slopeRange,strength),peturbN(channelList[[3]],nRange,strength))
  
}








completeChannel = function(partOfChannel) { # takes the left half of channel morph and mirrors it across the rightmost point
  
  distFromMiddle = max(partOfChannel[,1])-partOfChannel[,1]
  
  rightChannelX = rev(distFromMiddle)+max(partOfChannel[,1])
  rightChannelZ = rev(partOfChannel[,2])
  
  rightChannel = cbind(rightChannelX,rightChannelZ)
  rightChannel = rightChannel[-1,]
  
  wholeChannel = rbind(partOfChannel, rightChannel)
  
  return(wholeChannel)
  
}

getDist = function(a,b) {
  
  dist = sqrt((a[1] - b[1])^2 + (a[2] - b[2])^2)
  return(dist)
}

getAngle = function(p1,p2,p3) {
  
  a = p1-p2
  b = p3-p2
  theta = acos( sum(a*b) / ( sqrt(sum(a * a)) * sqrt(sum(b * b)) ) )
  return(theta)
}

kinkFactor = function(channel,absSmoothness,centerPenalty) {
  
  wholeChannel = completeChannel(channel)
  rows = nrow(wholeChannel)
  
  kink = 0
  
  for (i in 2:(rows-1)) {
    p1 = wholeChannel[i-1,]
    p2 = wholeChannel[i,]
    p3 = wholeChannel[i+1,]
    
    angle = getAngle(p1,p2,p3)
    rawFactor = abs((angle%%(3.141592/2))) # angles close to 90 get 1, close to 0 get zero
    nToCenter = abs(i-(rows/2))
    centerFactor = abs(rows/2 - nToCenter)*centerPenalty
    
    adder = rawFactor*absSmoothness * (centerFactor+1) * rows
    kink = kink+adder
  }
  
  return(kink)
}

# define functions to find W, D, A and Q, then a wrapper to get all at once

getChanW = function(channel) {
  
  width = max(channel[,1])
  return(width)
  
}


getChanA = function(channel) {
  
  underChannel = trapz(channel[,1],channel[,2])
  underBkf= trapz(c(0,max(channel[,1])),c(max(channel[,2]),max(channel[,2])))
  
  area = underBkf - underChannel
  return(area)
  
}

getChanD = function(channel) {
  
  area = getChanA(channel)
  width = getChanW(channel)
  depth = area / width
  return(depth)
  
}

getChanWP = function(channel) {
  
  delta = diff(channel)
  dists = sqrt(delta[,1]^2+delta[,2]^2)
  cumdist = cumsum(dists)
  
  wettedPerimeter = cumdist[length(cumdist)]
  return(wettedPerimeter)
  
}

getChanQ = function(channel,slope,n) { #this is the IMPERIAL eqn
  n = n
  slope = slope
  area = getChanA(channel)
  wettedP = getChanWP(channel)
  hydRadius = area / wettedP
  
  flow = (1.49/n) * area * hydRadius^(2/3) * sqrt(slope)
  return(flow)
  
}

getChanStats = function(channelList) { #note that this function usesa HALF CHANNEL in the channelList, dims are ft and seconds
  
  halfChannel = channelList[[1]]
  completedChannel = completeChannel(halfChannel)
  slope = channelList[[2]]
  n = channelList[[3]]
  
  width = getChanW(completedChannel)
  depth = getChanD(completedChannel)
  area = getChanA(completedChannel)
  flow = getChanQ(completedChannel,slope,n)
  kinkiness = kinkFactor(completedChannel,absSmoothness,centerPenalty)
  
  statvec = c(width,depth,area,flow,kinkiness)
  return(statvec)
  
}

popChanStats = function(population) {
  
  statMat = matrix(0,nrow(population),5)
  for (i in 1:nrow(population)) {
    
    statMat[i,] = getChanStats(currentPop[i,])
    
  }
  return(statMat)
  
}


objFunction = function(ideal,actual,weights) { # will need a way to penalize roughness; lower obj function scores are better
  
  absoluteOff = actual[1:4] - ideal
  kinkiness = absoluteOff[5]
  perOff = absoluteOff /ideal
  weightedOff = perOff * weights
  
  distanceOff = dist(rbind(weightedOff,c(0,0,0,0)))
  
  adjustedDist = distanceOff*kinkiness
  
  
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
seed = initialChannelList
secondChannel = fullyPeturb(seed,slopeRange,nRange,0.25)
currentPop = rbind(seed,secondChannel)

for (i in 3:popSize) {
  
  nextChannel = fullyPeturb(currentPop[i-1,],slopeRange,nRange,0.25)
  currentPop = rbind(currentPop,nextChannel)
  
}
row.names(currentPop) = 1:nrow(currentPop)




reproduce = function(channelListA,channelListB) {
  
  # two point crossover, one offspring
  ranSlice = sort(sample(1:nrow(channelListA[[1]]),2))
  
  newMorph = channelListA[[1]]
  newMorph[ranSlice,] = channelListB[[1]][ranSlice,]
  newMorph = orderChannel(newMorph)
  
  # average n and slope
  ranNum = sample(c(0,1,2),1)
  if (ranNum == 0) {
  newSlope = (channelListA[[2]] + channelListB[[2]]) / 2
  newN = (channelListA[[3]] + channelListB[[3]]) / 2
  } else if (ranNum == 1) {
    newSlope = (channelListA[[2]])
    newN = (channelListA[[3]])
  } else if (ranNum == 2) {
    newSlope = (channelListB[[2]])
    newN = (channelListB[[3]])
  }
  
  newChannel = list(newMorph,newSlope,newN)
  return(newChannel)
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
  
  numNewOffspring = popSize - elitism
  
  # tournament selection - two tournaments per offspring
  newGeneration = NULL
  
  for (i in 1:numNewOffspring) {
    
    tourneyWinnerOne = tournament(orderedPopulation,orderedFitness,tourneySize)
    tourneyWinnerTwo = tournament(orderedPopulation,orderedFitness,tourneySize)
    offspring = reproduce(tourneyWinnerOne,tourneyWinnerTwo)
    newGeneration = rbind(newGeneration,offspring)
    
  }
  
  for (i in 1:mutaters) {
    
    sampler = sample(1:nrow(newGeneration),1)
    mutationStrength = sample(muvec)
    newGeneration[sampler,] = fullyPeturb(newGeneration[sampler,],slopeRange,nRange,mutationStrength)
    
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

plot(completeChannel(result[[1]]),type = 'l')
plot(meanFitnessThroughGens,type='l')
plot(minFitnessThroughGens,type='l')

