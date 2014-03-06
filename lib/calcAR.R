###########################################################
# calcAR
# a function to calculate AR and plot CAP curve
#
# Date   : 2013/10/23
# Author : FEG Kei HARADA
# Version: 1.0.0
# Update : 
# TODO   : 
###########################################################




###########################################################
# input
# 
# X:     score of the model, numeric vector.
#   higher PD means of higher probability of target event
# y:     target variable
# TARGET: DF==TARGET means target event
# plotCAP
# plotpr
# name 
###########################################################


calcAR <- function (X, y, TARGET, plotCAP = FALSE, plotpr=FALSE, name="MODEL") {
  
  if (!plotCAP){
    plotpr=FALSE
  }
  
  y = as.factor(y)
  X = as.matrix(X)
  if (nrow(X) == 1){
    X = t(X)
  }
  nR = nrow(X)
  nC = ncol(X)
  nY = table(y)
  uL = as.factor(rownames(nY))
  nL = length(nY)
  if (sum(as.character(uL)==TARGET)==0){
    stop("calcCAP: List of labels 'y' have to contain TARGET.")
  }
  if (nL != 2) 
    stop("calcCAP: List of labels 'y' have to contain 2 class labels.")
  if (!is.numeric(X)) 
    stop("calcCAP: 'X' must be numeric")
  if (nR != length(y)) 
    stop("calcCAP: length(y) and nrow(X) must be the same")
  L = matrix(rep(uL, each = nR), nR, nL)
  
  per = matrix(c(1,2),nrow=1,ncol=2)
  
  AR = matrix(0.0, 1, nC)
  rownames(AR) = paste(uL[per[, 1]], " vs. ", uL[per[, 2]], 
                       sep = "")
  colnames(AR) = colnames(X)
  if (plotCAP) {
    plot(c(0, 1), c(0, 1), type = "n", xaxs = "i", yaxs = "i", 
         xlab = "cumulative propotion", 
         ylab = "probability of detection")
    title("CAP Curves")
    abline(h = 0:10/10, v = 0:10/10, col = "lightgray")
    if (nC < 20) {
      S = colnames(AR)
      if (is.null(S)) 
        S = paste("col", 1:nC)
      if (nC == 1){
        S = name
      }
      if (plotpr){
        legend("bottomright", c(S,"perfect","random"), col = c(1:nC+3,2,3), lty = 1, 
               lwd = 1, pch = 20, merge = TRUE, inset = 0.01, 
               bg = "white")
      } else{
        legend("bottomright", S, col = 1:nC, lty = 1, 
               lwd = 1, pch = 20, merge = TRUE, inset = 0.01, 
               bg = "white")        
      }
    }
    if (plotpr){
      nClr = 4
    } else {
      nClr = 1
    }
  }
  
  for (j in 1:nC) {
    x = sort(X[, j], index = TRUE, decreasing = TRUE)
    nunq = which(diff(x$x) == 0)
    nTies = length(nunq)
    if (nTies < nR - 1) {
      idx = y[x$ix]
      d = (matrix(rep(idx, nL), nR, nL) == L)
      colnames(d) = as.character(uL)
      for (i in 1:nL) d[, i] = cumsum(d[, i])
      if (nTies) 
        d = d[-nunq, ]
      d = rbind(matrix(0, 1, nL), d)
      nD = nrow(d)

      
      xx = if (d[nD, 1] > 0 & d[nD, 2] > 0) {
        (d[, 1]+d[, 2])/(d[nD, 1]+d[nD, 2])
      } else {
        c(0, 1)
      }
      yy = if (d[nD, 1] > 0 & d[nD, 2] > 0) {
        d[, TARGET]/d[nD, TARGET]
      } else {
        c(0, 1)
      }
      if (d[nD, 1] > 0 & d[nD, 2] > 0) {
        idx = 2:nD
        AR[1, j] = (sum((xx[idx] - xx[idx-1]) * (yy[idx] + yy[idx-1]))/2 - 0.5) / (0.5 * ( 1 - d[nD, TARGET]/(d[nD, 1]+d[nD, 2])))
      }        
      
      if (plotpr){
        lines(c(0,1), c(0,1), col = 3, type = "o", pch = 20)
        lines(c(0,d[nD, TARGET]/(d[nD, 1]+d[nD, 2]),1), c(0,1,1), col = 2, type = "o", pch = 20)
      }
      
      if (plotCAP) {
        lines(xx, yy, col = nClr, type = "o", pch = 20)
        nClr = nClr + 1
      }
        
    }
  }
  
  return(AR)
}

