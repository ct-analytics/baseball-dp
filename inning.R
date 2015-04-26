bases <- list(empty=1,
              first.only=2,
              second.only=3,
              third.only=4,
              first.second=5,
              first.third=6,
              second.third=7,
              full=8)

outs <- c(1,2,3)

nobody <- 1

a <- array(0,c(11,1))
#V:=current state of the game
#V[player at bat,
#  score differential,
#  number of outs,
#  players on base,
#  player on first base,
#  player to lead off the next half inning
#]
V <- array(0,c(9,dtop,3,8,9,9))

strategy <- array(1,c(9,dtop,3,8,9,9))

for (out in seq(1,3)) {
  for (player.on.base in seq(1,8)) {
    for (batter in seq(1,9)) {
      for (onfirst in seq(1,9)) {
        for (leadoff in seq(1,9)) {
          V[batter,dtop,out,player.on.base,onfirst,leadoff] <- 1
}}}}}

for (d in seq(dtop-1,1,-1)) {

## 2 outs, bases loaded
for (batter in seq(1,9)) {
  if (batter == 9) {
    ondeck <- 1
  } else {
    ondeck <- batter+1
  }
  for (onfirst in seq(1,9)) {
  for (leadoff in seq(1,9)) {
    num.outs <- outs[3]
    a[1] <- W[d,leadoff,ondeck]
    a[2] <- a[1]
    a[3] <- a[1]
    a[4] <- a[1]
    a[5] <- V[ondeck,min(dtop,d+1),num.outs,bases$full,batter,leadoff]
    a[6] <- V[ondeck,min(dtop,d+2),num.outs,bases$first.second,batter,leadoff]
    a[7] <- V[ondeck,min(dtop,d+2),num.outs,bases$first.third,batter,leadoff]
    a[8] <- V[ondeck,min(dtop,d+2),num.outs,bases$second.third,nobody,leadoff]
    a[9] <- V[ondeck,min(dtop,d+3),num.outs,bases$second.only,nobody,leadoff]
    a[10] <- V[ondeck,min(dtop,d+3),num.outs,bases$third.only,nobody,leadoff]
    a[11] <- V[ondeck,min(dtop,d+4),num.outs,bases$empty,nobody,leadoff]
    walk <- a[5]
    hitaway <- probs[batter,] %*% a
    V[batter,d,num.outs,bases$full,onfirst,leadoff] <- min(hitaway,walk)
    if (V[batter,d,num.outs,bases$full,onfirst,leadoff] == walk)
      strategy[batter,d,num.outs,bases$full,onfirst,leadoff] <- 4
}}}

## 2 outs, men on 2nd and 3rd
for (batter in seq(1,9)) {
  if (batter == 9) {
    ondeck <- 1
  } else {
    ondeck <- batter+1
  }
  for (onfirst in seq(1,9)) {
    for (leadoff in seq(1,9)) {
      num.outs <- outs[3]
      a[1] <- W[d,leadoff,ondeck]
      a[2] <- a[1]
      a[3] <- a[1]
      a[4] <- a[1]
      a[5] <- V[ondeck,d,num.outs,bases$full,batter,leadoff]
      a[6] <- V[ondeck,min(dtop,d+2),num.outs,bases$first.only,batter,leadoff]
      a[7] <- V[ondeck,min(dtop,d+2),num.outs,bases$first.only,batter,leadoff]
      a[8] <- V[ondeck,min(dtop,d+2),num.outs,bases$second.only,nobody,leadoff]
      a[9] <- V[ondeck,min(dtop,d+2),num.outs,bases$second.only,nobody,leadoff]
      a[10] <- V[ondeck,min(dtop,d+2),num.outs,bases$third.only,nobody,leadoff]
      a[11] <- V[ondeck,min(dtop,d+3),num.outs,bases$empty,nobody,leadoff]
      walk <- a[5]
      hitaway <- probs[batter,] %*% a
      V[batter,d,num.outs,bases$second.third,onfirst,leadoff] <- min(hitaway,walk)
      if (V[batter,d,num.outs,bases$second.third,onfirst,leadoff] == walk)
        strategy[batter,d,num.outs,bases$second.third,onfirst,leadoff] <- 4
}}}

## 2 outs, men on 1st and 3rd
for (batter in seq(1,9)) {
  if (batter == 9) {
    ondeck <- 1
  } else {
    ondeck <- batter+1
  }
  for (onfirst in seq(1,9)) {
    for (leadoff in seq(1,9)) {
      num.outs <- outs[3]
      a[1] <- W[d,leadoff,ondeck]
      a[2] <- a[1]
      a[3] <- a[1]
      a[4] <- a[1]
      a[5] <- V[ondeck,d,num.outs,bases$full,batter,leadoff]
      a[6] <- V[ondeck,min(dtop,d+1),num.outs,bases$first.second,batter,leadoff]
      a[7] <- V[ondeck,min(dtop,d+1),num.outs,bases$first.third,batter,leadoff]
      a[8] <- V[ondeck,min(dtop,d+1),num.outs,bases$second.third,nobody,leadoff]
      a[9] <- V[ondeck,min(dtop,d+2),num.outs,bases$second.only,nobody,leadoff]
      a[10] <- V[ondeck,min(dtop,d+2),num.outs,bases$third.only,nobody,leadoff]
      a[11] <- V[ondeck,min(dtop,d+3),num.outs,bases$empty,nobody,leadoff]
      walk <- a[5]
      hitaway <- probs[batter,] %*% a
      steal <- psteal[onfirst] * V[batter,d,num.outs,bases$second.third,nobody,leadoff] +
        (1-psteal[onfirst]) * W[d,leadoff,batter]
      V[batter,d,num.outs,bases$first.third,onfirst,leadoff] <- min(max(hitaway,steal),walk)
      if (V[batter,d,num.outs,bases$first.third,onfirst,leadoff] == walk)
        strategy[batter,d,num.outs,bases$first.third,onfirst,leadoff] <- 4
      else if (V[batter,d,num.outs,bases$first.third,onfirst,leadoff] == steal)
        strategy[batter,d,num.outs,bases$first.third,onfirst,leadoff] <- 2
}}}

## 2 outs, men on 1st and 2nd
for (batter in seq(1,9)) {
  if (batter == 9) {
    ondeck <- 1
  } else {
    ondeck <- batter+1
  }
  for (onfirst in seq(1,9)) {
    for (leadoff in seq(1,9)) {
      num.outs <- outs[3]
      a[1] <- W[d,leadoff,ondeck]
      a[2] <- a[1]
      a[3] <- a[1]
      a[4] <- a[1]
      a[5] <- V[ondeck,d,num.outs,bases$full,batter,leadoff]
      a[6] <- V[ondeck,min(dtop,d+1),num.outs,bases$first.second,batter,leadoff]
      a[7] <- V[ondeck,min(dtop,d+1),num.outs,bases$first.third,batter,leadoff]
      a[8] <- V[ondeck,min(dtop,d+1),num.outs,bases$second.third,nobody,leadoff]
      a[9] <- V[ondeck,min(dtop,d+2),num.outs,bases$second.only,nobody,leadoff]
      a[10] <- V[ondeck,min(dtop,d+2),num.outs,bases$third.only,nobody,leadoff]
      a[11] <- V[ondeck,min(dtop,d+3),num.outs,bases$empty,nobody,leadoff]
      walk <- a[5]
      hitaway <- probs[batter,] %*% a
      V[batter,d,num.outs,bases$first.second,onfirst,leadoff] <- min(max(hitaway,steal),walk)
      if (V[batter,d,num.outs,bases$first.second,onfirst,leadoff] == walk)
        strategy[batter,d,num.outs,bases$first.third,onfirst,leadoff] <- 4
}}}

## 2 outs, man on 3rd
for (batter in seq(1,9)) {
  if (batter == 9) {
    ondeck <- 1
  } else {
    ondeck <- batter+1
  }
  for (onfirst in seq(1,9)) {
    for (leadoff in seq(1,9)) {
      num.outs <- outs[3]
      a[1] <- W[d,leadoff,ondeck]
      a[2] <- a[1]
      a[3] <- a[1]
      a[4] <- a[1]
      a[5] <- V[ondeck,d,num.outs,bases$first.third,batter,leadoff]
      a[6] <- V[ondeck,min(dtop,d+1),num.outs,bases$first.only,batter,leadoff]
      a[7] <- V[ondeck,min(dtop,d+1),num.outs,bases$first.only,batter,leadoff]
      a[8] <- V[ondeck,min(dtop,d+1),num.outs,bases$second.only,nobody,leadoff]
      a[9] <- V[ondeck,min(dtop,d+1),num.outs,bases$second.only,nobody,leadoff]
      a[10] <- V[ondeck,min(dtop,d+1),num.outs,bases$third.only,nobody,leadoff]
      a[11] <- V[ondeck,min(dtop,d+2),num.outs,bases$empty,nobody,leadoff]
      walk <- a[5]
      hitaway <- probs[batter,] %*% a
      V[batter,d,num.outs,bases$third.only,onfirst,leadoff] <- min(max(hitaway,steal),walk)
      if (V[batter,d,num.outs,bases$third.only,onfirst,leadoff] == walk)
        strategy[batter,d,num.outs,bases$third.only,onfirst,leadoff] <- 4
}}}

## 2 outs, man on 2nd
for (batter in seq(1,9)) {
  if (batter == 9) {
    ondeck <- 1
  } else {
    ondeck <- batter+1
  }
  for (onfirst in seq(1,9)) {
    for (leadoff in seq(1,9)) {
      num.outs <- outs[3]
      a[1] <- W[d,leadoff,ondeck]
      a[2] <- a[1]
      a[3] <- a[1]
      a[4] <- a[1]
      a[5] <- V[ondeck,d,outs,bases$first.second,batter,leadoff]
      a[6] <- V[ondeck,min(dtop,d+1),outs,bases$first.only,batter,leadoff]
      a[7] <- V[ondeck,min(dtop,d+1),outs,bases$first.only,batter,leadoff]
      a[8] <- V[ondeck,min(dtop,d+1),outs,bases$second.only,nobody,leadoff]
      a[9] <- V[ondeck,min(dtop,d+1),outs,bases$second.only,nobody,leadoff]
      a[10] <- V[ondeck,min(dtop,d+1),outs,bases$third.only,nobody,leadoff]
      a[11] <- V[ondeck,min(dtop,d+2),outs,bases$empty,nobody,leadoff]
      walk <- a[5]
      hitaway <- probs[batter,] %*% a
      V[batter,d,outs,bases$second.only,onfirst,leadoff] <- min(hitaway,walk)
      if (V[batter,d,outs,bases$second.only,onfirst,leadoff]==walk)
        strategy[batter,d,outs,bases$second.only,onfirst,leadoff] <- 4
}}}

## 2 outs, man on 1st
for (batter in seq(1,9)) {
  if (batter == 9) {
    ondeck <- 1
  } else {
    ondeck <- batter+1
  }
  for (onfirst in seq(1,9)) {
    for (leadoff in seq(1,9)) {
      num.outs <- outs[3]
      a[1] <- W[d,leadoff,ondeck]
      a[2] <- a[1]
      a[3] <- a[1]
      a[4] <- a[1]
      a[5] <- V[ondeck,d,outs,bases$first.second,batter,leadoff]
      a[6] <- V[ondeck,d,outs,bases$first.second,batter,leadoff]
      a[7] <- V[ondeck,d,outs,bases$first.third,batter,leadoff]
      a[8] <- V[ondeck,d,outs,bases$second.third,nobody,leadoff]
      a[9] <- V[ondeck,min(dtop,d+1),outs,bases$second.only,nobody,leadoff]
      a[10] <- V[ondeck,min(dtop,d+1),outs,bases$third.only,nobody,leadoff]
      a[11] <- V[ondeck,min(dtop,d+2),outs,bases$empty,nobody,leadoff]
      walk <- a[5]
      hitaway <- probs[batter,] %*% a
      steal <- psteal[onfirst]*V[batter,d,outs,bases$second.only,nobody,leadoff] +
        (1-psteal[onfirst])*W[d,leadoff,batter]
      V[batter,d,outs,bases$first.only,onfirst,leadoff] <- min(max(hitaway,steal),walk)
      if (V[batter,d,outs,bases$first.only,onfirst,leadoff]==walk) {
        strategy[batter,d,outs,bases$first.only,onfirst,leadoff] <- 4
      } else if (V[batter,d,outs,bases$first.only,onfirst,leadoff]==steal) { 
        strategy[batter,d,outs,bases$first.only,onfirst,leadoff] <- 2 
      }
}}}

## 2 outs, bases empty
for (batter in seq(1,9)) {
  if (batter == 9) {
    ondeck <- 1
  } else {
    ondeck <- batter+1
  }
  for (onfirst in seq(1,9)) {
    for (leadoff in seq(1,9)) {
      num.outs <- outs[3]
      a[1] <- W[d,leadoff,ondeck]
      a[2] <- a[1]
      a[3] <- a[1]
      a[4] <- a[1]
      a[5] <- V[ondeck,d,outs,bases$first.only,batter,leadoff]
      a[6] <- V[ondeck,d,outs,bases$first.only,batter,leadoff]
      a[7] <- V[ondeck,d,outs,bases$first.only,batter,leadoff]
      a[8] <- V[ondeck,d,outs,bases$second.only,nobody,leadoff]
      a[9] <- V[ondeck,d,outs,bases$second.only,nobody,leadoff]
      a[10] <- V[ondeck,d,outs,bases$third.only,nobody,leadoff]
      a[11] <- V[ondeck,min(dtop,d+1),outs,bases$empty,nobody,leadoff]
      walk <- a[5]
      hitaway <- probs[batter,] %*% a
      V[batter,d,outs,bases$empty,onfirst,leadoff] <- min(hitaway,walk)
      if (V[batter,d,outs,bases$empty,onfirst,leadoff]==walk)
        strategy[batter,d,outs,bases$empty,onfirst,leadoff] <- 4
}}}

## 1 out, bases loaded
for (batter in seq(1,9)) {
  if (batter == 9) {
    ondeck <- 1
  } else {
    ondeck <- batter+1
  }
  for (onfirst in seq(1,9)) {
    for (leadoff in seq(1,9)) {
      num.outs <- outs[2]
      a[1] <- V[ondeck,d,outs2,bases$full,onfirst,leadoff]
      a[2] <- V[ondeck,min(dtop,d+1),outs2,bases$first.second,onfirst,leadoff]
      a[3] <- W[d,leadoff,ondeck]
      a[4] <- V[ondeck,d,outs2,bases$full,batter,leadoff]
      a[5] <- V[ondeck,min(dtop,d+1),outs,bases$full,batter,leadoff]
      a[6] <- V[ondeck,min(dtop,d+2),outs,bases$first.second,batter,leadoff]
      a[7] <- V[ondeck,min(dtop,d+2),outs,bases$first.third,batter,leadoff]
      a[8] <- V[ondeck,min(dtop,d+2),outs,bases$second.third,nobody,leadoff]
      a[9] <- V[ondeck,min(dtop,d+3),outs,bases$second.only,nobody,leadoff]
      a[10] <- V[ondeck,min(dtop,d+3),outs,bases$third.only,nobody,leadoff]
      a[11] <- V[ondeck,min(dtop,d+4),outs,bases$empty,nobody,leadoff]
      walk <- a[5]
      hitaway <- probs[batter,] %*% a
      V[batter,d,outs,bases$full,onfirst,leadoff] <- min(hitaway,walk)
      if (V[batter,d,outs,bases$full,onfirst,leadoff]==walk)
        strategy[batter,d,outs,bases$full,onfirst,leadoff] <- 4
}}}

## 1 out, men on 2nd and 3rd
for (batter in seq(1,9)) {
  if (batter == 9) {
    ondeck <- 1
  } else {
    ondeck <- batter+1
  }
  for (onfirst in seq(1,9)) {
    for (leadoff in seq(1,9)) {
      num.outs <- outs[2]
      a[1] <- V[ondeck,d,outs2,bases$second.third,nobody,leadoff]
      a[2] <- V[ondeck,min(dtop,d+1),outs2,bases$third.only,nobody,leadoff]
      a[3] <- a[1]
      a[4] <- V[ondeck,min(dtop,d+1),outs2,bases$third.only,nobody,leadoff]
      a[5] <- V[ondeck,d,outs,bases$full,batter,leadoff]
      a[6] <- V[ondeck,min(dtop,d+2),outs,bases$first.only,batter,leadoff]
      a[7] <- V[ondeck,min(dtop,d+2),outs,bases$first.only,batter,leadoff]
      a[8] <- V[ondeck,min(dtop,d+2),outs,bases$second.only,nobody,leadoff]
      a[9] <- V[ondeck,min(dtop,d+2),outs,bases$second.only,nobody,leadoff]
      a[10] <- V[ondeck,min(dtop,d+2),outs,bases$third.only,nobody,leadoff]
      a[11] <- V[ondeck,min(dtop,d+3),outs,bases$empty,nobody,leadoff]
      walk <- a[5]
      hitaway <- probs[batter,] %*% a
      V[batter,d,outs,bases$second.third,onfirst,leadoff] <- min(hitaway,walk)
      if (V[batter,d,outs,bases$second.third,onfirst,leadoff]==walk)
        strategy[batter,d,outs,bases$second.third,onfirst,leadoff] <- 4
}}}

## 1 out, men on 1st and 3rd
for (batter in seq(1,9)) {
  if (batter == 9) {
    ondeck <- 1
  } else {
    ondeck <- batter+1
  }
  for (onfirst in seq(1,9)) {
    for (leadoff in seq(1,9)) {
      num.outs <- outs[2]
      a[1] <- V[ondeck,d,outs2,bases$first.third,onfirst,leadoff]
      a[2] <- V[ondeck,min(dtop,d+1),outs2,bases$first.only,onfirst,leadoff]
      a[3] <- W[d,leadoff,ondeck]
      a[4] <- V[ondeck,min(dtop,d+1),outs2,bases$second.only,nobody,leadoff]
      a[5] <- V[ondeck,d,outs,bases$full,batter,leadoff]
      a[6] <- V[ondeck,min(dtop,d+1),outs,bases$first.second,batter,leadoff]
      a[7] <- V[ondeck,min(dtop,d+1),outs,bases$first.third,batter,leadoff]
      a[8] <- V[ondeck,min(dtop,d+1),outs,bases$second.third,nobody,leadoff]
      a[9] <- V[ondeck,min(dtop,d+2),outs,bases$second.only,nobody,leadoff]
      a[10] <- V[ondeck,min(dtop,d+2),outs,bases$third.only,nobody,leadoff]
      a[11] <- V[ondeck,min(dtop,d+3),outs,bases$empty,nobody,leadoff]
      walk <- a[5]
      hitaway <- probs[batter,] %*% a
      steal <- psteal[onfirst]*V[batter,d,outs,bases$second.third,nobody,leadoff] +
        (1-psteal[onfirst])*V[batter,d,outs2,bases$third.only,nobody,leadoff]
      V[batter,d,outs,bases$first.third,onfirst,leadoff] <- min(max(hitaway,steal),walk)
      if (V[batter,d,outs,bases$first.third,onfirst,leadoff]==walk) {
      strategy[batter,d,outs,bases$first.third,onfirst,leadoff] <- 4
      } else if (V[batter,d,outs,bases$first.third,onfirst,leadoff]==steal) {
      strategy[batter,d,outs,bases$first.third,onfirst,leadoff] <- 2 
      }
}}}

## 1 out, men on 1st and 2nd
for (batter in seq(1,9)) {
  if (batter == 9) {
    ondeck <- 1
  } else {
    ondeck <- batter+1
  }
  for (onfirst in seq(1,9)) {
    for (leadoff in seq(1,9)) {
      num.outs <- outs[2]
      a[1] <- V[ondeck,d,outs2,bases$first.second,onfirst,leadoff]
      a[2] <- V[ondeck,d,outs2,bases$first.third,onfirst,leadoff]
      a[3] <- W[d,leadoff,ondeck]
      a[4] <- V[ondeck,d,outs2,bases$first.second,batter,leadoff]
      a[5] <- V[ondeck,d,outs,bases$full,batter,leadoff]
      a[6] <- V[ondeck,min(dtop,d+1),outs,bases$first.second,batter,leadoff]
      a[7] <- V[ondeck,min(dtop,d+1),outs,bases$first.third,batter,leadoff]
      a[8] <- V[ondeck,min(dtop,d+1),outs,bases$second.third,nobody,leadoff]
      a[9] <- V[ondeck,min(dtop,d+2),outs,bases$second.only,nobody,leadoff]
      a[10] <- V[ondeck,min(dtop,d+2),outs,bases$third.only,nobody,leadoff]
      a[11] <- V[ondeck,min(dtop,d+3),outs,bases$empty,nobody,leadoff]
      walk <- a[5]
      bunt <- sacbunt(batter)*V[ondeck,d,outs2,bases$second.third,nobody,leadoff] +
        (1-sacbunt(batter))*V[ondeck,d,outs2,bases$first.second,batter,leadoff]
      hitaway <- probs[batter,] %*% a
      V[batter,d,outs,bases$first.second,onfirst,leadoff] <- min(max(bunt,hitaway),walk)
      if (V[batter,d,outs,bases$first.second,onfirst,leadoff]==walk) {
        strategy[batter,d,outs,bases$first.second,onfirst,leadoff] <- 4
      } else if (V[batter,d,outs,bases$first.second,onfirst,leadoff]==bunt) { 
        strategy[batter,d,outs,bases$first.second,onfirst,leadoff] <- 3 
      }
}}}

## 1 out, man on 3rd
for (batter in seq(1,9)) {
  if (batter == 9) {
    ondeck <- 1
  } else {
    ondeck <- batter+1
  }
  for (onfirst in seq(1,9)) {
    for (leadoff in seq(1,9)) {
      num.outs <- outs[2]
      a[1] <- V[ondeck,d,outs2,bases$third.only,nobody,leadoff]
      a[2] <- V[ondeck,min(dtop,d+1),outs2,bases$empty,nobody,leadoff]
      a[3] <- a[1]
      a[4] <- a[2]
      a[5] <- V[ondeck,d,outs,bases$first.third,batter,leadoff]
      a[6] <- V[ondeck,min(dtop,d+1),outs,bases$first.only,batter,leadoff]
      a[7] <- V[ondeck,min(dtop,d+1),outs,bases$first.only,batter,leadoff]
      a[8] <- V[ondeck,min(dtop,d+1),outs,bases$second.only,nobody,leadoff]
      a[9] <- V[ondeck,min(dtop,d+1),outs,bases$second.only,nobody,leadoff]
      a[10] <- V[ondeck,min(dtop,d+1),outs,bases$third.only,nobody,leadoff]
      a[11] <- V[ondeck,min(dtop,d+2),outs,bases$empty,nobody,leadoff]
      walk <- a[5]
      hitaway <- probs[batter,] %*% a
      V[batter,d,outs,bases$third.only,onfirst,leadoff] <- min(hitaway,walk)
      if (V[batter,d,outs,bases$third.only,onfirst,leadoff]==walk)
        strategy[batter,d,outs,bases$third.only,onfirst,leadoff] <- 4
}}}

## 1 out, man on 2nd
for (batter in seq(1,9)) {
  if (batter == 9) {
    ondeck <- 1
  } else {
    ondeck <- batter+1
  }
  for (onfirst in seq(1,9)) {
    for (leadoff in seq(1,9)) {
      num.outs <- outs[2]
      a[1] <- V[ondeck,d,outs2,bases$second.only,nobody,leadoff]
      a[2] <- V[ondeck,d,outs2,bases$third.only,nobody,leadoff]
      a[3] <- a[1]
      a[4] <- a[2]
      a[5] <- V[ondeck,d,outs,bases$first.second,batter,leadoff]
      a[6] <- V[ondeck,min(dtop,d+1),outs,bases$first.only,batter,leadoff]
      a[7] <- V[ondeck,min(dtop,d+1),outs,bases$first.only,batter,leadoff]
      a[8] <- V[ondeck,min(dtop,d+1),outs,bases$second.only,nobody,leadoff]
      a[9] <- V[ondeck,min(dtop,d+1),outs,bases$second.only,nobody,leadoff]
      a[10] <- V[ondeck,min(dtop,d+1),outs,bases$third.only,nobody,leadoff]
      a[11] <- V[ondeck,min(dtop,d+2),outs,bases$empty,nobody,leadoff]
      walk <- a[5]
      hitaway <- probs[batter,] %*% a
      V[batter,d,outs,bases$second.only,onfirst,leadoff] <- min(hitaway,walk)
      if (V[batter,d,outs,bases$second.only,onfirst,leadoff]==walk)
        strategy[batter,d,outs,bases$second.only,onfirst,leadoff] <- 4
}}}

## 1 out, man on 1st
for (batter in seq(1,9)) {
  if (batter == 9) {
    ondeck <- 1
  } else {
    ondeck <- batter+1
  }
  for (onfirst in seq(1,9)) {
    for (leadoff in seq(1,9)) {
      num.outs <- outs[2]
      a[1] <- V[ondeck,d,outs2,bases$first.only,onfirst,leadoff]
      a[2] <- a[1]
      a[3] <- W[d,leadoff,ondeck]
      a[4] <- V[ondeck,d,outs2,bases$first.only,batter,leadoff]
      a[5] <- V[ondeck,d,outs,bases$first.second,batter,leadoff]
      a[6] <- V[ondeck,d,outs,bases$first.second,batter,leadoff]
      a[7] <- V[ondeck,d,outs,bases$first.third,batter,leadoff]
      a[8] <- V[ondeck,d,outs,bases$second.third,nobody,leadoff]
      a[9] <- V[ondeck,min(dtop,d+1),outs,bases$second.only,nobody,leadoff]
      a[10] <- V[ondeck,min(dtop,d+1),outs,bases$third.only,nobody,leadoff]
      a[11] <- V[ondeck,min(dtop,d+2),outs,bases$empty,nobody,leadoff]
      walk <- a[5]
      bunt <- sacbunt(batter)*V[ondeck,d,outs2,bases$second.only,nobody,leadoff] +
        (1-sacbunt(batter))*V[ondeck,d,outs2,bases$first.only,batter,leadoff]
      hitaway <- probs[batter,] %*% a
      steal <- psteal[onfirst]*V[batter,d,outs,bases$second.only,nobody,leadoff] +
        (1-psteal[onfirst])*V[batter,d,outs2,bases$empty,nobody,leadoff]
      V[batter,d,outs,bases$first.only,onfirst,leadoff] <- min(max(hitaway,steal,bunt),walk)
      if (V[batter,d,outs,bases$first.only,onfirst,leadoff]==walk) {
        strategy[batter,d,outs,bases$first.only,onfirst,leadoff] <- 4
      } else if (V[batter,d,outs,bases$first.only,onfirst,leadoff]==steal) {
        strategy[batter,d,outs,bases$first.only,onfirst,leadoff] <- 2 
      } else if (V[batter,d,outs,bases$first.only,onfirst,leadoff]==bunt) {
        strategy[batter,d,outs,bases$first.only,onfirst,leadoff] <- 3   
      }
}}}

## 1 out, bases empty
for (batter in seq(1,9)) {
  if (batter == 9) {
    ondeck <- 1
  } else {
    ondeck <- batter+1
  }
  for (onfirst in seq(1,9)) {
    for (leadoff in seq(1,9)) {
      num.outs <- outs[2]
      a[1] <- V[ondeck,d,outs2,bases$empty,nobody,leadoff]
      a[2] <- a[1]
      a[3] <- a[1]
      a[4] <- a[1]
      a[5] <- V[ondeck,d,outs,bases$first.only,batter,leadoff]
      a[6] <- V[ondeck,d,outs,bases$first.only,batter,leadoff]
      a[7] <- V[ondeck,d,outs,bases$first.only,batter,leadoff]
      a[8] <- V[ondeck,d,outs,bases$second.only,nobody,leadoff]
      a[9] <- V[ondeck,d,outs,bases$second.only,nobody,leadoff]
      a[10] <- V[ondeck,d,outs,bases$third.only,nobody,leadoff]
      a[11] <- V[ondeck,min(dtop,d+1),outs,bases$empty,nobody,leadoff]
      walk <- a[5]
      hitaway <- probs[batter,] %*% a
      V[batter,d,outs,bases$empty,onfirst,leadoff] <- min(hitaway,walk)
      if (V[batter,d,outs,bases$empty,onfirst,leadoff]==walk)
        strategy[batter,d,outs,bases$empty,onfirst,leadoff] <- 4
}}}

## 0 out, bases loaded
for (batter in seq(1,9)) {
  if (batter == 9) {
    ondeck <- 1
  } else {
    ondeck <- batter+1
  }
  for (onfirst in seq(1,9)) {
    for (leadoff in seq(1,9)) {
      num.outs <- outs[1]
      a[1] <- V[ondeck,d,outs1,bases$full,onfirst,leadoff]
      a[2] <- V[ondeck,min(dtop,d+1),outs1,bases$first.second,onfirst,leadoff]
      a[3] <- min(V[ondeck,min(dtop,d+1),outs2,bases$third.only,nobody,leadoff],
                V[ondeck,d,outs1,bases$full,batter,leadoff])
      a[4] <- V[ondeck,d,outs1,bases$full,batter,leadoff]
      a[5] <- V[ondeck,min(dtop,d+1),outs,bases$full,batter,leadoff]
      a[6] <- V[ondeck,min(dtop,d+2),outs,bases$first.second,batter,leadoff]
      a[7] <- V[ondeck,min(dtop,d+2),outs,bases$first.third,batter,leadoff]
      a[8] <- V[ondeck,min(dtop,d+2),outs,bases$second.third,nobody,leadoff]
      a[9] <- V[ondeck,min(dtop,d+3),outs,bases$second.only,nobody,leadoff]
      a[10] <- V[ondeck,min(dtop,d+3),outs,bases$third.only,nobody,leadoff]
      a[11] <- V[ondeck,min(dtop,d+4),outs,bases$empty,nobody,leadoff]
      walk <- a[5]
      hitaway <- probs[batter,] %*% a
      V[batter,d,outs,bases$full,onfirst,leadoff] <- min(hitaway,walk)
      if (V[batter,d,outs,bases$full,onfirst,leadoff]==walk)
        strategy[batter,d,outs,bases$full,onfirst,leadoff] <- 4
}}}

## 0 out, men on 2nd and 3rd
for (batter in seq(1,9)) {
  if (batter == 9) {
    ondeck <- 1
  } else {
    ondeck <- batter+1
  }
  for (onfirst in seq(1,9)) {
    for (leadoff in seq(1,9)) {
      num.outs <- outs[1]
      a[1] <- V[ondeck,d,outs1,bases$second.third,nobody,leadoff]
      a[2] <- V[ondeck,min(dtop,d+1),outs1,bases$third.only,nobody,leadoff]
      a[3] <- a[1]
      a[4] <- V[ondeck,min(dtop,d+1),outs1,bases$third.only,nobody,leadoff]
      a[5] <- V[ondeck,d,outs,bases$full,batter,leadoff]
      a[6] <- V[ondeck,min(dtop,d+2),outs,bases$first.only,batter,leadoff]
      a[7] <- V[ondeck,min(dtop,d+2),outs,bases$first.only,batter,leadoff]
      a[8] <- V[ondeck,min(dtop,d+2),outs,bases$second.only,nobody,leadoff]
      a[9] <- V[ondeck,min(dtop,d+2),outs,bases$second.only,nobody,leadoff]
      a[10] <- V[ondeck,min(dtop,d+2),outs,bases$third.only,nobody,leadoff]
      a[11] <- V[ondeck,min(dtop,d+3),outs,bases$empty,nobody,leadoff]
      walk <- a[5]
      hitaway <- probs[batter,] %*% a
      V[batter,d,outs,bases$second.third,onfirst,leadoff] <- min(hitaway,walk)
      if (V[batter,d,outs,bases$second.third,onfirst,leadoff]==walk)
        strategy[batter,d,outs,bases$second.third,onfirst,leadoff] <- 4
}}}

## 0 out, men on 1st and 3rd
for (batter in seq(1,9)) {
  if (batter == 9) {
    ondeck <- 1
  } else {
    ondeck <- batter+1
  }
  for (onfirst in seq(1,9)) {
    for (leadoff in seq(1,9)) {
      num.outs <- outs[1]
      a[1] <- V[ondeck,d,outs1,bases$first.third,onfirst,leadoff]
      a[2] <- V[ondeck,min(dtop,d+1),outs1,bases$first.only,onfirst,leadoff]
      a[3] <- min(V[ondeck,min(dtop,d+1),outs2,bases$empty,nobody,leadoff], 
                V[ondeck,d,outs1,bases$first.third,batter,leadoff])
      a[4] <- V[ondeck,min(dtop,d+1),outs1,bases$second.only,nobody,leadoff]
      a[5] <- V[ondeck,d,outs,bases$full,batter,leadoff]
      a[6] <- V[ondeck,min(dtop,d+1),outs,bases$first.second,batter,leadoff]
      a[7] <- V[ondeck,min(dtop,d+1),outs,bases$first.third,batter,leadoff]
      a[8] <- V[ondeck,min(dtop,d+1),outs,bases$second.third,nobody,leadoff]
      a[9] <- V[ondeck,min(dtop,d+2),outs,bases$second.only,nobody,leadoff]
      a[10] <- V[ondeck,min(dtop,d+2),outs,bases$third.only,nobody,leadoff]
      a[11] <- V[ondeck,min(dtop,d+3),outs,bases$empty,nobody,leadoff]
      walk <- a[5]
      hitaway <- probs[batter,] %*% a
      steal <- psteal[onfirst]*V[batter,d,outs,bases$second.third,nobody,leadoff] +
        (1-psteal[onfirst])*V[batter,d,outs1,bases$third.only,nobody,leadoff]
      V[batter,d,outs,bases$first.third,onfirst,leadoff] <- min(max(hitaway,steal),walk)
      if (V[batter,d,outs,bases$first.third,onfirst,leadoff]==walk) {
        strategy[batter,d,outs,bases$first.third,onfirst,leadoff] <- 4
      } else if (V[batter,d,outs,bases$first.third,onfirst,leadoff]==steal) {
        strategy[batter,d,outs,bases$first.third,onfirst,leadoff] <- 2 
      }
}}}

## 0 out, men on 1st and 2nd
for (batter in seq(1,9)) {
  if (batter == 9) {
    ondeck <- 1
  } else {
    ondeck <- batter+1
  }
  for (onfirst in seq(1,9)) {
    for (leadoff in seq(1,9)) {
      num.outs <- outs[1]
      a[1] <- V[ondeck,d,outs1,bases$first.second,onfirst,leadoff]
      a[2] <- V[ondeck,d,outs1,bases$first.third,onfirst,leadoff]
      a[3] <- V[ondeck,d,outs2,bases$third.only, nobody,leadoff]
      a[4] <- V[ondeck,d,outs1,bases$first.second,batter,leadoff]
      a[5] <- V[ondeck,d,outs,bases$full,batter,leadoff]
      a[6] <- V[ondeck,min(dtop,d+1),outs,bases$first.second,batter,leadoff]
      a[7] <- V[ondeck,min(dtop,d+1),outs,bases$first.third,batter,leadoff]
      a[8] <- V[ondeck,min(dtop,d+1),outs,bases$second.third,nobody,leadoff]
      a[9] <- V[ondeck,min(dtop,d+2),outs,bases$second.only,nobody,leadoff]
      a[10] <- V[ondeck,min(dtop,d+2),outs,bases$third.only,nobody,leadoff]
      a[11] <- V[ondeck,min(dtop,d+3),outs,bases$empty,nobody,leadoff]
      walk <- a[5]
      bunt <- sacbunt(batter)*V[ondeck,d,outs1,bases$second.third,nobody,leadoff] +
        (1-sacbunt(batter))*V[ondeck,d,outs1,bases$first.second,batter,leadoff]
      hitaway <- probs[batter,] %*% a
      V[batter,d,outs,bases$first.second,onfirst,leadoff] <- min(max(hitaway,bunt),walk)
      if (V[batter,d,outs,bases$first.second,onfirst,leadoff]==walk) {
        strategy[batter,d,outs,bases$first.second,onfirst,leadoff] <- 4
      } else if (V[batter,d,outs,bases$first.second,onfirst,leadoff]==bunt) { 
        strategy[batter,d,outs,bases$first.second,onfirst,leadoff] <- 3 
      }
}}}

## 0 out, man on 3rd
for (batter in seq(1,9)) {
  if (batter == 9) {
    ondeck <- 1
  } else {
    ondeck <- batter+1
  }
  for (onfirst in seq(1,9)) {
    for (leadoff in seq(1,9)) {
      num.outs <- outs[1]
      a[1] <- V[ondeck,d,outs1,bases$third.only,nobody,leadoff]
      a[2] <- V[ondeck,min(dtop,d+1),outs1,bases$empty,nobody,leadoff]
      a[3] <- a[1]
      a[4] <- a[2]
      a[5] <- V[ondeck,d,outs,bases$first.third,batter,leadoff]
      a[6] <- V[ondeck,min(dtop,d+1),outs,bases$first.only,batter,leadoff]
      a[7] <- V[ondeck,min(dtop,d+1),outs,bases$first.only,batter,leadoff]
      a[8] <- V[ondeck,min(dtop,d+1),outs,bases$second.only,nobody,leadoff]
      a[9] <- V[ondeck,min(dtop,d+1),outs,bases$second.only,nobody,leadoff]
      a[10] <- V[ondeck,min(dtop,d+1),outs,bases$third.only,nobody,leadoff]
      a[11] <- V[ondeck,min(dtop,d+2),outs,bases$empty,nobody,leadoff]
      walk <- a[5]
      hitaway <- probs[batter,] %*% a
      V[batter,d,outs,bases$third.only,onfirst,leadoff] <- min(hitaway,walk)
      if (V[batter,d,outs,bases$third.only,onfirst,leadoff]==walk)
        strategy[batter,d,outs,bases$third.only,onfirst,leadoff] <- 4
}}}

## 0 out, man on 2nd
for (batter in seq(1,9)) {
  if (batter == 9) {
    ondeck <- 1
  } else {
    ondeck <- batter+1
  }
  for (onfirst in seq(1,9)) {
    for (leadoff in seq(1,9)) {
      num.outs <- outs[1]
      a[1] <- V[ondeck,d,outs1,bases$second.only,nobody,leadoff]
      a[2] <- V[ondeck,d,outs1,bases$third.only,nobody,leadoff]
      a[3] <- a[1]
      a[4] <- a[2]
      a[5] <- V[ondeck,d,outs,bases$first.second,batter,leadoff]
      a[6] <- V[ondeck,min(dtop,d+1),outs,bases$first.only,batter,leadoff]
      a[7] <- V[ondeck,min(dtop,d+1),outs,bases$first.only,batter,leadoff]
      a[8] <- V[ondeck,min(dtop,d+1),outs,bases$second.only,nobody,leadoff]
      a[9] <- V[ondeck,min(dtop,d+1),outs,bases$second.only,nobody,leadoff]
      a[10] <- V[ondeck,min(dtop,d+1),outs,bases$third.only,nobody,leadoff]
      a[11] <- V[ondeck,min(dtop,d+2),outs,bases$empty,nobody,leadoff]
      walk <- a[5]
      hitaway <- probs[batter,] %*% a
      V[batter,d,outs,bases$second.only,onfirst,leadoff] <- min(hitaway,walk)
      if (V[batter,d,outs,bases$second.only,onfirst,leadoff]==walk)
        strategy[batter,d,outs,bases$second.only,onfirst,leadoff] <- 4
}}}

## 0 out, man on 1st
for (batter in seq(1,9)) {
  if (batter == 9) {
    ondeck <- 1
  } else {
    ondeck <- batter+1
  }
  for (onfirst in seq(1,9)) {
    for (leadoff in seq(1,9)) {
      num.outs <- outs[1]
      a[1] <- V[ondeck,d,outs1,bases$first.only,onfirst,leadoff]
      a[2] <- a[1]
      a[3] <- V[ondeck,d,outs2,bases$empty,nobody,leadoff]
      a[4] <- V[ondeck,d,outs1,bases$first.only,batter,leadoff]
      a[5] <- V[ondeck,d,outs,bases$first.second,batter,leadoff]
      a[6] <- V[ondeck,d,outs,bases$first.second,batter,leadoff]
      a[7] <- V[ondeck,d,outs,bases$first.third,batter,leadoff]
      a[8] <- V[ondeck,d,outs,bases$second.third,nobody,leadoff]
      a[9] <- V[ondeck,min(dtop,d+1),outs,bases$second.only,nobody,leadoff]
      a[10] <- V[ondeck,min(dtop,d+1),outs,bases$third.only,nobody,leadoff]
      a[11] <- V[ondeck,min(dtop,d+2),outs,bases$empty,nobody,leadoff]
      walk <- a[5]
      bunt <- sacbunt(batter)*V[ondeck,d,outs1,bases$second.only,nobody,leadoff] + 
        (1-sacbunt(batter))*V[ondeck,d,outs1,bases$first.only,batter,leadoff]
      hitaway <- probs[batter,] %*% a
      steal <- psteal[onfirst]*V[batter,d,outs,bases$second.only,nobody,leadoff] +
        (1-psteal[onfirst])*V[batter,d,outs1,bases$empty,nobody,leadoff]
      V[batter,d,outs,bases$first.only,onfirst,leadoff] <- min(max(hitaway,steal,bunt),walk)
      if (V[batter,d,outs,bases$first.only,onfirst,leadoff]==walk) {
        strategy[batter,d,outs,bases$first.only,onfirst,leadoff] <- 4
      } else if (V[batter,d,outs,bases$first.only,onfirst,leadoff]==steal) {
        strategy[batter,d,outs,bases$first.only,onfirst,leadoff] <- 2 
      } else if (V[batter,d,outs,bases$first.only,onfirst,leadoff]==bunt) {
        strategy[batter,d,outs,bases$first.only,onfirst,leadoff] <- 3   
      }
}}}

## 0 out, bases empty
for (batter in seq(1,9)) {
  if (batter == 9) {
    ondeck <- 1
  } else {
    ondeck <- batter+1
  }
  for (onfirst in seq(1,9)) {
    for (leadoff in seq(1,9)) {
      num.outs <- outs[1]
      a[1] <- V[ondeck,d,outs1,bases$empty,nobody,leadoff]
      a[2] <- a[1]
      a[3] <- a[1]
      a[4] <- a[1]
      a[5] <- V[ondeck,d,outs,bases$first.only,batter,leadoff]
      a[6] <- V[ondeck,d,outs,bases$first.only,batter,leadoff]
      a[7] <- V[ondeck,d,outs,bases$first.only,batter,leadoff]
      a[8] <- V[ondeck,d,outs,bases$second.only,nobody,leadoff]
      a[9] <- V[ondeck,d,outs,bases$second.only,nobody,leadoff]
      a[10] <- V[ondeck,d,outs,bases$third.only,nobody,leadoff]
      a[11] <- V[ondeck,min(dtop,d+1),outs,bases$empty,nobody,leadoff]
      walk <- a[5]
      hitaway <- probs[batter,] %*% a
      V[batter,d,outs,bases$empty,onfirst,leadoff] <- min(hitaway,walk)
      if (V[batter,d,outs,bases$empty,onfirst,leadoff]==walk)
        strategy[batter,d,outs,bases$empty,onfirst,leadoff] <- 4
}}}

}
