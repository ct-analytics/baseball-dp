setwd("~/Documents/baseball dp")

numextrainnings <- 8

maxlead <- 25
dtop <- 2*maxlead+1

##W := Probability of winning
##W(score differential, player at bat, next player at bat)
W <- array(0,c(dtop,9,9))

## compute the required probabilities
source("probabilities.R")

## boundary conditions for bottom half of final extra inning
for (i in seq(1,9)) {
  for (j in seq(1,9)) {
    for (d in seq(1,dtop)) {
      if (d <= maxlead) {
        W[d,i,j]=0
      } else if (d==maxlead+1) {
        W[d,i,j]=0.5
      } else {
        W[d,i,j]=1
      }
}}}

## set parameters for bottom half of final extra inning
psteal <- pstealhome
sacbunt <- sacbunthome
probs <- probshome

## run bottom half of final extra inning
source("inning.R")

## boundary conditions for top half of final extra inning
for (i in seq(1,9)) {
  for (j in seq(1,9)) {
    for (d in seq(1,dtop)) {
      W[d,i,j]= 1 - V[i,dtop-d+1,outs[1],bases$empty,nobody,j]
    }}}

## set parameters for top half of final extra inning
psteal <- pstealhome
sacbunt <- sacbunthome
probs <- probshome

## run top half of final extra inning
source("inning.R")

# loop for regular (non-final) extra innings
for (count in seq(1,numextrainnings)) {
  
  for (i in seq(1,9)) {
    for (j in seq(1,9)) {
      for (d in seq(1,dtop)) {
        if (d <= maxlead) {
          W(d,i,j)=0
        } else if (d==maxlead+1) {
          W(d,i,j)=1-V(i,d,outs0,b0,nobody,j)
        } else {
          W(d,i,j)=1
        }
      }}}
  
  psteal <- pstealhome
  sacbunt <- sacbunthome
  probs <- probshome
  
  source("inning.R")
  
  for (i in seq(1,9)) {
    for (j in seq(1,9)) {
      for (d in seq(1,dtop)) {
        W(d,i,j)=1-V(i,dtop-d+1,outs0,b0,nobody,j)
      }}}
  
  psteal <- pstealaway
  sacbunt <- sacbuntaway
  probs <- probsaway
  
  source("inning.R")
  
}

for (inning in seq(10,1)) {
  # bottom of inning
  for (i in seq(1,9)) {
    for (j in seq(1,9)) {
      for (d in seq(1,dtop)) {
        if (d <= maxlead) {
          W(d,i,j)=0
        } else if (d==maxlead+1) {
          W(d,i,j)=1-V(i,d,outs0,b0,nobody,j)
        } else {
          W(d,i,j)=1
        }
      }
    }
  }
  
  psteal <- pstealhome
  sacbunt <- sacbunthome
  probs <- probshome
  
  source("inning.R")  
  
  # top of inning
  for (i in seq(1,9)) {
    for (j in seq(1,9)) {
      for (d in seq(1,dtop)) {
        W(d,i,j)=1-V(i,dtop-d+1,outs0,b0,nobody,j)
      }}}
  
  psteal=pstealaway
  sacbunt=sacbuntaway
  probs=probsaway
  
  source("inning.R")
}
  