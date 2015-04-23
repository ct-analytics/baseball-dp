##Need to add in row / column names for each matrix

##                    H    2B     3B   HR    BB    SO   HBP  IBB   GO    AO
rawstatshome <- matrix(c(81,  13,     1,  .5,   17,   48,   7,   0,   92,   82, 
                         90,  12,    13,   2,   27,   48,   0,   0,   67,   93, 
                         55,   7,    .5,   7,   14,   34,   0,   2,   43,   39, 
                         69,  12,    .5,  23,  131,   19,   4,  71,   39,   60, 
                         96,  16,     3,  15,   22,   52,  12,   2,   91,   73, 
                         62,  14,   0.5,   2,   14,   35,   1,   0,   60,   56, 
                         26,   5,   0.2,   2,   10,   22,   1,   0,   27,   40, 
                         32,   6,     2,   2,    4,   33,   0,   0,   40,   27, 
                         22,   5,   0.1,   4,    7,   26,   0,   0,   28,   30),ncol=10,byrow=T)

##                    H    2B     3B   HR    BB    SO   HBP  IBB   GO    AO
rawstatsaway <- matrix(c(81,  13,     1,  .5,   17,   48,   7,   0,   92,   82, 
                         90,  12,    13,   2,   27,   48,   0,   0,   67,   93, 
                         55,   7,    .5,   7,   14,   34,   0,   2,   43,   39, 
                         69,  12,    .5,  23,  131,   19,   4,  71,   39,   60, 
                         96,  16,     3,  15,   22,   52,  12,   2,   91,   73, 
                         62,  14,   0.5,   2,   14,   35,   1,   0,   60,   56, 
                         26,   5,   0.2,   2,   10,   22,   1,   0,   27,   40, 
                         32,   6,     2,   2,    4,   33,   0,   0,   40,   27, 
                         22,   5,   0.1,   4,    7,   26,   0,   0,   28,   30),ncol=10,byrow=T)

statshome <- matrix(,nrow=9,ncol=10)
statsaway <- matrix(,nrow=9,ncol=10)

for (i in seq(1,9)) {
  tpahome <- rawstatshome[i,1] + sum(rawstatshome[i,5:10]) - 2 * rawstatshome[i,8]
  tpaaway <- rawstatsaway[i,1] + sum(rawstatsaway[i,5:10]) - 2 * rawstatsaway[i,8]
  statshome[i,1] <- (rawstatshome[i,5] + rawstatshome[i,7] - rawstatshome[i,8]) / tpahome
  statsaway[i,1] <- (rawstatsaway[i,5] + rawstatsaway[i,7] - rawstatsaway[i,8]) / tpaaway
  statshome[i,2] <- (rawstatshome[i,1] - rawstatshome[i,2] - rawstatshome[i,3] - rawstatshome[i,4]) / tpahome
  statsaway[i,2] <- (rawstatsaway[i,1] - rawstatsaway[i,2] - rawstatsaway[i,3] - rawstatsaway[i,4]) / tpaaway
  statshome[i,3] <- rawstatshome[i,2] / tpahome
  statsaway[i,3] <- rawstatsaway[i,2] / tpaaway
  statshome[i,4] <- rawstatshome[i,3] / tpahome
  statsaway[i,4] <- rawstatsaway[i,3] / tpaaway
  statshome[i,5] <- rawstatshome[i,4] / tpahome
  statsaway[i,5] <- rawstatsaway[i,4] / tpaaway
  statshome[i,6] <- rawstatshome[i,6] / tpahome
  statsaway[i,6] <- rawstatsaway[i,6] / tpaaway
  statshome[i,7] <- rawstatshome[i,9] / tpahome
  statsaway[i,7] <- rawstatsaway[i,9] / tpaaway
  statshome[i,8] <- rawstatshome[i,10] / tpahome
  statsaway[i,8] <- rawstatsaway[i,10] / tpaaway
}



sacbunthome <- c(.9,.9,.9,.9,.9,.9,.9,.9,.9)
sacbuntaway <- c(.9,.9,.9,.9,.9,.9,.9,.9,.9)

pstealhome <- c(.55, .55, .5, .5, .5, .5, .5, .45, .45)
pstealaway <- c(.55, .55, .5, .5, .5, .5, .5, .45, .45)

longflyfrac <- .5
hardgrounderfrac <- .5
longsinglefrac <- .5
longdoublefrac <- .5     

probshome <- matrix(rep(0,9*11),nrow=9,ncol=11)
probsaway <- matrix(rep(0,9*11),nrow=9,ncol=11)

for (i in seq(1,9)) {
  probshome[i,1] <- statshome[i,6]+statshome[i,8]*(1-longflyfrac)
  probsaway[i,1] <- statsaway[i,6]+statsaway[i,8]*(1-longflyfrac)
  probshome[i,2] <- statshome[i,8]*longflyfrac
  probsaway[i,2] <- statsaway[i,8]*longflyfrac
  probshome[i,3] <- statshome[i,7]*hardgrounderfrac
  probsaway[i,3] <- statsaway[i,7]*hardgrounderfrac
  probshome[i,4] <- statshome[i,7]*(1-hardgrounderfrac)
  probsaway[i,4] <- statsaway[i,7]*(1-hardgrounderfrac)
  probshome[i,5] <- statshome[i,1]
  probsaway[i,5] <- statsaway[i,1]
  probshome[i,6] <- statshome[i,2]*(1-longsinglefrac)
  probsaway[i,6] <- statsaway[i,2]*(1-longsinglefrac)
  probshome[i,7] <- statshome[i,2]*longsinglefrac
  probsaway[i,7] <- statsaway[i,2]*longsinglefrac
  probshome[i,8] <- statshome[i,3]*(1-longdoublefrac)
  probsaway[i,8] <- statsaway[i,3]*(1-longdoublefrac)
  probshome[i,9] <- statshome[i,3]*longdoublefrac
  probsaway[i,9] <- statsaway[i,3]*longdoublefrac
  probshome[i,10] <- statshome[i,4]
  probsaway[i,10] <- statsaway[i,4]
  probshome[i,11] <- statshome[i,5]
  probsaway[i,11] <- statsaway[i,5]
}

rm(rawstatshome,rawstatsaway,statsaway,statshome,i,longdoublefrac,longsinglefrac,hardgrounderfrac,longflyfrac)
