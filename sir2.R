##### stochastic simulation two #####

make.event <- function(eName,source,deltaList) {
  return(list(name=eName,source=source,deltas=deltaList))
}

gen.delta.matrix <- function(stateNames,events) {
  eventNames <- sapply(events,function(e) e$name)
  
  D <- matrix(0.0,nrow=length(events),ncol=length(stateNames))
  colnames(D) <- stateNames
  rownames(D) <- eventNames

  for (e in events) {
    eName <- e$name
    deltas <- e$deltas
    for(s in names(deltas)) {
      D[eName,s] <- deltas[[s]]
    }
  }
  return(D)
}

gen.model.simulator <- function(stateNames,events,f.rates.dt) {
  eventSources <- sapply(sirEvents,function(e) e$source)
  numEventTypes <- length(sirEvents) 
  # create matrix of deltas (changes triggered by events)
  deltaMatrix <- gen.delta.matrix(stateNames,events)

  f.sample <- function(state,rates) {
    numEvents <- numeric(numEventTypes)
    for(j in seq_len(length(eventSources))) {
          eSrc <- eventSources[j]
          numEvents[j] <- rbinom(1,size=state[eSrc],prob=rates[j])
    }
    return(numEvents)
  }

  f <- function(state0,params,times,nsim=1) {
    res <- as.list(seq_len(nsim))
    for(sim in seq(nsim)) {
      state <- state0[stateNames]
      i <- 1
      states <- list()
      states[[i]] <- c(t=times[1], state)
      for (t in times[-1]) {
        i <- i+1
        dt <- times[i]-times[i-1]
        rates <- f.rates.dt(state,params,t,dt)
	numEvents <- f.sample(state,rates)
	totDelta <- colSums(deltaMatrix * numEvents)
   	state <- state + totDelta
   	states[[i]] <- c(t=t,state)
      } # end t loop
      res[[sim]] <- as.data.frame(do.call(rbind,states))
    }
    return(res)
  }

  return(f)
}


