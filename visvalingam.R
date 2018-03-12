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

currentsheet = 10



  
  
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
    
    #int = 0.1
    # get unsmoothed interpolation
    #interp = approx(stationing, elevationing, xout=seq(from = min(stationing), to = max(stationing),by=int))
    #model = ksmooth(interp$x, interp$y, bandwidth=0, x.points=interp$x)
    
    #whys = model$y
    #exes = model$x
    
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
    
    isMin = function(a, b) {
      aNegative = a <= 0
      bPositive = b > 0
      
      isMin = (aNegative == T & bPositive == TRUE & b > a)
      
      return(isMin)
    }
    
    euc.dist <- function(data, point) {
      
      apply(data, 1, function (row) sqrt(sum((point - row) ^ 2)))
      
    }
    
    
    euc = function(point,toCompare) {
      
      a = point
      b = toCompare
      d = sqrt(sum((point - toCompare) ^ 2))
      
      return(d)
      
    }

    triArea = function(p1,p2,p3) {
      
      d1 = euc(p1,p2)
      d2 = euc(p1,p3)
      d3 = euc(p2,p3)
      
      hp = (d1 + d2 + d3) / 2 #half the perim
      
      area = sqrt(abs(hp*(hp-d1)*(hp-2)*(hp-d3)))
      
      return(area)
      
    }
    
    lineTriArea = function(line, indices = NULL) { # calculates area of a triangle formed by a point in a data frame and a point above and a point below it
      # note that line is a dataframe
      areas = rep(0,(nrow(line)-2))
      
      if (is.null(indices)) {
        indices = 1:(nrow(line))
      }
      
      indices = indices[-c(1,length(indices))]
      
      for (i in 2:(nrow(line)-1)) {
        
        areas[i-1] = triArea(line[i-1,],line[i,],line[i+1,])
        
      }
      
      areaDF = as.data.frame(cbind(indices,areas,line[-c(1,nrow(line)),]))
      
      return(areaDF)
      
    }
    
    
  
    
    # reduceComplexity = function (line, numToKeep) { # Visvalingam's Algorithm
    #   
    #   lineCopy = line
    #   
    #   rm = NULL # will be populated by indices of all points removed
    #   
    #   while (nrow(lineCopy) > numToKeep) {
    #     
    #     lineArea = lineTriArea(lineCopy)
    #     sortedArea = lineArea[order(lineArea$areas),]
    #     
    #     # remove points with smallest areas until the sortedArea is of size numToKeep
    #     removePoint = sortedArea[1,1]
    #     rm = c(rm,removePoint)
    #     
    #     lineCopy = lineCopy[-removePoint,]
    #     
    #   }
    #   
    #   #out = list[lineCopy,rm]
    #   return(lineCopy)
    # }
    
    reduceComplexity = function (line, keepthw = TRUE) { # Visvalingam's Algorithm. Returns ordered list of points to remove in order of visual change (least to most)
      
      if (keepthw) {
        numToKeep = 3
      } else {
        numToKeep = 2
      }
      
      lineCopy = line
      
      rem = NULL # will be populated by indices of points in the vector, ranked by how much their removal changes the curve
      indices = 1:(nrow(line))
      rowsInFrame = nrow(lineCopy)
      
      while (rowsInFrame > numToKeep) {
        if (!is.null(rem)) {
          lineArea = lineTriArea(lineCopy[-rem,],indices[-rem])
        } else {
          lineArea = lineTriArea(lineCopy,indices)
        }
        
        sortedArea = lineArea[order(lineArea$areas),]
        print(nrow(sortedArea))
        # remove points with smallest areas until the sortedArea is of size numToKeep
        removePoint = sortedArea[1,1]
        
        if (keepthw & sortedArea[1,4]==min(sortedArea[,4])) {
          removePoint = sortedArea[2,1]
        }
        
        rem = c(rem,removePoint)
        rowsInFrame = rowsInFrame - 1
        

      }
      
      return(rem)
      
    }

    xsDF = as.data.frame(cbind(stationing,elevationing))
    
    visv = reduceComplexity(xsDF)
    
    visv = reh
    xsDF = splineDF
    
    percentRemoved = (1:length(visv))/length(visv)*100
    


    interp1 = approx(xsDF,n=10000)$y
    interp1 = matrix(unlist(interp1))
    residvec = NULL
    for (i in 1:length(visv)) {
      
      currentInterp = approx(xsDF[-visv[1:i],],n=10000)$y
      currentInterp = matrix(unlist(currentInterp))
      resid = norm(currentInterp - interp1)
      residvec = c(residvec,resid)
      
    }
    
    par(mfrow=c(2,1))
    
    for (i in seq(from=1,to=length(visv),by = 10)) {
      
      plot(xsDF, main = paste(round(percentRemoved[i]),'% (',nrow(xsDF)-i,' points left)', sep = ''), xlab = 'Station (ft)', ylab = 'Elevation (ft)',type = 'l')
      lines(xsDF[-visv[1:i],],col='red')

      
      plot(percentRemoved,residvec,type='l', xlab = '% Removed', ylab = 'norm(original - model)')
      abline(h=0,col='red')
      points(percentRemoved[i],residvec[i],col='blue',cex=1.2)

      
    }
    
    

    
    