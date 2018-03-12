rm(list=ls())



# if you don't have a local copy of package xlsx, install it by uncommenting the next line
#install.packages('xlsx')
library(xlsx)



########### USER INPUT

# note that filepaths for windows users use forward slashes, while Linux and Mac filepaths use backslashes

mywd = 'C:\\Users\\sky.jones\\Desktop\\Excelerate Testbox' # where your file is located
xssource = 'landing_xs_database' # name of the file with XS data

########### END USER INPUT
























### check out the input data


#currentsheet = 1

xssource = paste(xssource,'.xlsx',sep='')

mywd = normalizePath(mywd)
setwd(mywd)
sheets = getSheets(loadWorkbook(xssource))
numsheets = length(sheets)

currentsheet = 5

  
  
  data = read.xlsx(xssource, sheetIndex = currentsheet, header = TRUE, stringsAsFactors=FALSE)
  
  bkf = as.numeric(data[1,2])
  type = data[3,2]
  membership = toString(data[4,2])
  #membership = as.numeric(data[4,2])
  title = data[2,2]
  
  basin = data[5,2]
  watershed = data[6,2]
  drainageArea = data[7,2]
  prepDate = data[10,2]
  crew = data[11,2]
  lowBankHeight = as.numeric(data[12,2])
  chartTitle = data[13,2]
  
  infoString = c('River Basin', 'Watershed', 'XS ID', 'Drainage Area', 'Date', 'Field Crew')
  infoDat = c(basin, watershed, title, drainageArea, prepDate, crew)
  # if the user specifies DELETE for an info field, delete it from the grob table
  toDELETE = which(infoDat == 'DELETE')
  
  if(!length(toDELETE) == 0) { # if there is something to delete
    infoString = infoString[-toDELETE]
    infoDat = infoDat[-toDELETE]
  }
  
  whereImages = data[9,2]
  code = data[8,2]
  
  cutData = data[,-(1:2)]
  numYears = ncol(cutData)/4
  colSeq = (0:(numYears-1))*4
  
  monYears = as.matrix(cutData[1,colSeq+1])
  elevations = as.matrix(cutData[,colSeq+2])
  descriptions = as.matrix(cutData[,colSeq+3])
  stations = as.matrix(cutData[,colSeq+4])
  
  lowest = min(elevations, na.rm = TRUE)
  highest = max(elevations, na.rm = TRUE)
  leftest = min(stations, na.rm = TRUE)
  rightest = max(stations, na.rm = TRUE)
  

  if (type == 'r'){
    bank = bkf
    echanmin = min(na.omit(elevations[,ncol(elevations)])) # elevation of channel minimum
    dmax = bank-echanmin
    fpe = bank + dmax # flood prone elevation, equivalent to 2*dmax + min channel elevation
    highest = max(fpe,highest)
  } else {
    fpe = NA
  }
  

  

    i = 3
    
    stationing = na.omit(stations[,i])
    elevationing = na.omit(elevations[,i])
    
    int = 0.1
    # get unsmoothed interpolation
    interp = approx(stationing, elevationing, xout=seq(from = min(stationing), to = max(stationing),by=int))
    model = ksmooth(interp$x, interp$y, bandwidth=0, x.points=interp$x)
    
    whys = model$y
    exes = model$x
    
    xrange = range(exes)
    buildResponse = function(xrange2,model,order, step2 = 0.1) {
      
      coefs = model$coefficients
      xSample = seq(from = xrange2[1], to = xrange2[2], by = step2)
      vec = rep(0,length(xSample))
      for (i in 1:length(coefs)) {
        if (!is.na(coefs[i])) {
          vec = vec + coefs[i]*xSample^(i-1)
        }
      }
      return(vec)
    }
    
    
    getPrediction = function(exes1,whys1,order, xrange1, step1 = 0.1) {
      
      model = lm(whys1 ~ poly(exes1,order,raw = TRUE))
      
      outVec = buildResponse(xrange1,model,order, step2 = step1)
      
      return(outVec)
    }
 
    samesign = function(a, b) {
      aPositive = a >= 0
      bPositive = b >= 0
      
      isSame = (aPositive == bPositive)
      
      return(isSame)
    }
    
    
    firstDiffs = diff(whys)
    
    minmax = rep(0,(length(firstDiffs)-1))

    for (i in 1:length(minmax)) {
      
      if (!samesign(firstDiffs[i],firstDiffs[i+1])) {
        minmax[i+1] = 1
      }
      
    }
    
    minmax[1] = 1
    minmax[length(minmax)] = 1
    minmaxEx = exes[c(which(minmax != 0))]
    minmaxWhy = whys[c(which(minmax != 0))]
    

    
    
    # now let's break up everything between the minima/maxima, fit a third order polynomial to each section, then find its inflection point (if it has one)
    inflectEx = NULL
    inflectWhy = NULL
    for (i in 1:(length(minmaxEx)-1)) {
      
        currentBounds = c(minmaxEx[i],minmaxEx[i+1])
        indices = match(currentBounds,exes)
        
        exVec = exes[indices[1]:indices[2]]
        whyVec = whys[indices[1]:indices[2]]
        
        thisFit = getPrediction(exVec,whyVec,3, range(exVec), step1 = int)
        
        #lines(exVec,thisFit, col='blue', lty = 2, lwd = 1.5)
        
        deriv2 = diff(diff(thisFit))
        
        if (length(deriv2) < 4) {
          next()
        } 

        inflects = rep(0,(length(deriv2)-1))
        
        
        for (j in 1:length(inflects)) {
          
          if (!samesign(deriv2[j],deriv2[j+1])) {
            inflects[j+1] = 1
          }
          
        }
        
        if (sum(inflects) == 1) { # only add inflection point to vector if there's one and only one inflection
        
          inflectEx = c(inflectEx,exVec[which(inflects == 1)])
          inflectWhy = c(inflectWhy,whyVec[which(inflects == 1)])
          
        }
        
      
    }
    
    
    plot(exes,whys,type = 'l', main = 'Critical Points', xlab = 'Station (ft)', ylab = 'Elevation (ft)') 
    points(minmaxEx,minmaxWhy,col='red')
    points(inflectEx,inflectWhy,col='blue')
    
    keyExes = c(minmaxEx, inflectEx)
    keyWhys = c(minmaxWhy, inflectWhy)
    
    keyDF = as.data.frame(cbind(keyExes,keyWhys))
    
    keyDF = keyDF[order(keyExes),]
    
    ## make the critical surface
    
    #critSurface = getPrediction(keyDF$keyExes,keyDF$keyWhys,order=nrow(keyDF-1), range(exes), step1 = int)
    splineDF = spline(x = keyDF$keyExes, y = keyDF$keyWhys, xout = exes) 
    critSurface = splineDF$y
    
    plot(exes,whys,type = 'l', main = 'Critical Surface', xlab = 'Station (ft)', ylab = 'Elevation (ft)') 
    lines(exes,critSurface,type='l',col='blue')
    
    # this kurv() makes distinction between neg and pos curvature
    kurv = function(xs,ys) { # where length(xs) == length(yes) == 3
      
      x1 = xs[1]
      x2 = xs[2]
      x3 = xs[3]
      
      y1 = ys[1]
      y2 = ys[2]
      y3 = ys[3]
      
      K = 2*((x2-x1)*(y3-y1)-(x3-x1)*(y2-y1)) / sqrt(((x2-x1)^2+(y2-y1)^2)*((x3-x1)^2+(y3-y1)^2)*((x3-x2)^2+(y3-y2)^2))
      #K = 2*abs((x2-x1)*(y3-y1)-(x3-x1)*(y2-y1)) / sqrt(((x2-x1)^2+(y2-y1)^2)*((x3-x1)^2+(y3-y1)^2)*((x3-x2)^2+(y3-y2)^2))
      # for no signs
      return(K)
    }
    
    dfKurv = function(splineFrame) {
      
      kurvVec = rep(0,nrow(splineFrame)-2)
      
      for (i in 1:(nrow(splineFrame)-2)) {
        
        dfSlice = splineFrame[i:(i+2),]
        sliceX = dfSlice[,1]
        sliceY = dfSlice[,2]
        
        kurvVec[i] = kurv(sliceX,sliceY)
        
      }
      
      return(c(0,kurvVec,0))
      
    }
    
    critFrame = as.data.frame(cbind(exes,critSurface))
    splineKurv = dfKurv(critFrame)
    
    #flatten kurv
    #splineKurv[which(splineKurv < -0.5)] = -0.5
    
    rbPal = colorRampPalette(c('red','cornflowerblue'))
    splineColors = rbPal(10)[as.numeric(cut(splineKurv,breaks = 10))]
    
    plot(splineDF,pch = 20, col = splineColors, main = 'Spline Curvature over Spline')
    
    plot(exes,whys,pch = 20, col = splineColors, main = 'Spline Curvature over Original')
    points(minmaxEx,minmaxWhy, col = 'red', pch = 2)
    points(inflectEx,inflectWhy, col = 'blue', pch = 2)
    lines(splineDF, lty = 2, col = 'green')

    ## now find the kurv minima
    
    
    isMin = function(a, b) {
      aNegative = a <= 0
      bPositive = b > 0
      
      isMin = (aNegative == T & bPositive == TRUE & b > a)
      
      return(isMin)
    }
    
    kurvFirstDiffs = diff(splineKurv)
    
    kurvMin = rep(0,(length(kurvFirstDiffs)-1))
    
    for (i in 1:length(kurvMin)) {
      
      if (isMin(kurvFirstDiffs[i],kurvFirstDiffs[i+1])) {
        kurvMin[i+1] = 1
      }
      
    }
    
    kurvMin[1] = 1
    kurvMin[length(kurvMin)] = 1
    kurvMinEx = exes[c(which(kurvMin != 0))]
    kurvMinWhy = critSurface[c(which(kurvMin != 0))]
    
    plot(splineDF,pch = 20, col = splineColors, main = 'Curve Min')
    points(kurvMinEx,kurvMinWhy, col = 'blue', pch = 1, cex = 2)
    lines(exes,whys,type = 'l',lty=2)
    
    
    euc.dist <- function(data, point) {
      
      apply(data, 1, function (row) sqrt(sum((point - row) ^ 2)))
      
    }
    
    projEx = rep(0,length(kurvMinEx))
    projWhy = rep(0,length(kurvMinWhy))
    
    for (i in 1:length(kurvMinEx)) {
      
      
      currentPoint = c(kurvMinEx[i],kurvMinWhy[i])
      
      distances = euc.dist(keyDF,currentPoint)
      
      mindex = which.min(distances)
      
      projEx[i] = keyDF[mindex,1]
      projWhy[i] = keyDF[mindex,2]
      
    }

    projDF = as.data.frame(cbind(projEx,projWhy))
    
    plot(exes,whys,type='l',main = 'Orig and Projected')
    points(kurvMinEx,kurvMinWhy,col='green',cex=1.2)
    points(keyDF,col='blue',cex=1.4)
    points(projDF,col='red',cex=1.6)
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    