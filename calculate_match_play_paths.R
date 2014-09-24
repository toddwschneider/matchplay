numberOfPaths = function(numberOfHoles=18) {
  memoizedResults = c()
  memoKey = function(holesPlayed, score) paste(holesPlayed, score, sep=",")
  
  matchIsOver = function(holesPlayed, score) {
    holesRemaining = numberOfHoles - holesPlayed
    return(abs(score) > holesRemaining | holesRemaining == 0)
  }
  
  matchContinues = function(holesPlayed, score) {
    return(as.numeric(!matchIsOver(holesPlayed, score)))
  }
  
  intermediatePaths = function(holesPlayed, score) {
    k = memoKey(holesPlayed, score)
    
    val = if (holesPlayed == 0 & score == 0) {
      1
    } else if (abs(score) > holesPlayed) {
      0
    } else if (!is.na(memoizedResults[k])) {
      memoizedResults[k]
    } else {
      intermediatePaths(holesPlayed - 1, score - 1) * matchContinues(holesPlayed - 1, score - 1) +
        intermediatePaths(holesPlayed - 1, score) * matchContinues(holesPlayed - 1, score) +
        intermediatePaths(holesPlayed - 1, score + 1) * matchContinues(holesPlayed - 1, score + 1)
    }
    
    return(val)
  }
  
  terminalPoints = c()
  
  for(holesPlayed in 0:numberOfHoles) {
    maxPossibleScore = min(c(holesPlayed, numberOfHoles - holesPlayed + 2))
    
    for(score in -maxPossibleScore:maxPossibleScore) {
      paths = intermediatePaths(holesPlayed, score)
      k = memoKey(holesPlayed, score)
      memoizedResults[k] = paths
      
      if (matchIsOver(holesPlayed, score)) {
        terminalPoints[k] = paths
      }
    }
  }
  
  return(list(totalPaths=sum(terminalPoints), memoizedResults=memoizedResults, terminalPoints=terminalPoints))
}

# numberOfPaths(18)$totalPaths
# 169,688,089

# numberOfPaths(36)$totalPaths
# 4.765e+16
