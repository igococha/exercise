library(deSolve)
library(ggplot2)
library(reshape2)

# as required by deSolve
sir.ode <- function(t,state,params) {
  with(as.list(c(state, params)), {
    N <- S+I+R
    dS <- -beta*S*I/N
    dI <- beta*S*I/N - sigma*I
    dR <- sigma*I
    return(list(c(dS, dI, dR)))
 })
}

gen.ode.model <- function(f) {
  fret <- function(state,times,params) {
    ode.out <- ode(y = state, times = times, func = sir.ode, parms = params)
    return(as.data.frame(ode.out))
  }
}

# auxiliary functions
gen.plot.ode <- function(traj.df) {
  traj.m = melt(traj.df, id.vars='time')
  p <- ggplot(traj.m, aes(x = time, y = value, color = variable)) +
    theme_bw() + geom_line()
  return(p)
}

## stochastic simulation


sir.dt <- function(t,dt,state,params) {
 with(as.list(c(state, params)), {
    N <- S+I+R
    probSI <- beta*I*dt/N
    probIR <- sigma*dt
    flowSI <- rbinom(n=1,size=S,prob=probSI)
    flowIR <- rbinom(n=1,size=I,prob=probIR)
    # removed list constructor
    return(c(-flowSI, flowSI-flowIR, flowIR))
 })
}

sim.dt.binomial <- function(f.dt,state0,params,times,nsim=1) {
  res <- as.list(seq_len(nsim))
  for(sim in seq(nsim)) {
    state <- state0
    i <- 1
    states <- list()
    states[[i]] <- c(t=times[1], state)
    for (t in times[-1]) {
      i <- i+1
      dt <- times[i]-times[i-1]
      state <- state + f.dt(t,dt,state,params)
      states[[i]] <- c(t=t,state)
    }
    res[[sim]] <- as.data.frame(do.call(rbind,states))
  }
  return(res)
}

gen.plot.sim <- function(sim.df) {
  sim.m = melt(sim.df, id.vars='t')
  p <- ggplot(sim.m, aes(x = t, y = value, color = variable)) +
    theme_bw() + geom_line()
  return(p)
}

get_peak <- function(sim.df,tlabel,dem) {
  idx <- which.max(sapply(sim.df[deme], function(x) x))
  return( c( t=mean(sim.df[idx,tlabel]), deme=mean(sim.df[idx,deme] )))
}


##### generalising #####

make.event <- function(eName,source,deltaList) {
  return(list(name=eName,source=source,deltas=deltaList))
}

stateNames <- c('S','I','R')

sirEvents <- list(
	  make.event('infection','S', list('I'=1,'S'=-1)),
	  make.event('recovery', 'I', list('I'=-1,'R'=1))
	  )

sir.eRates <- function(state,params,t,dt) {
  rates <- numeric(2) # must equal number of events
  with(as.list(c(state, params)), {
    N <- S+I+R
    rates[1] <- beta*I*dt/N
    rates[2] <- sigma*dt
    return(rates)
 })
}

# as in fucntion call
events <- sirEvents
f.eRates <- sir.eRates 

times <- times.sim

stateSize <- length(stateNames)
numEventTypes <- length(sirEvents)

eventNames <- sapply(sirEvents,function(e) e$name)
eventSources <- sapply(sirEvents,function(e) e$source)

deltaArray <- matrix(0.0,nrow=numEventTypes,ncol=stateSize)
colnames(deltaArray) <- stateNames
rownames(deltaArray) <- eventNames

for (e in events) {
  eName <- e$name
  deltas <- e$deltas
  for(s in names(deltas)) {
    # print(paste("setting delta[",eName,",",s,"] = ",deltas[[s]],sep='')  )
    deltaArray[eName,s] <- deltas[[s]]
  }
}

# one simulation
state <- state0[stateNames]
i <- 1
states <- list()
states[[i]] <- c(t=times[1], state)
numEvents <- numeric(numEventTypes) ## events generated for (t,dt)

for (t in times[-1]) {
   i <- i+1
   dt <- times[i]-times[i-1]
   rates <- f.eRates(state,params,t,dt)
   for(j in seq_len(length(eventSources))) {
     eSrc <- eventSources[j]
     # print(paste("rbinom ",state[eSrc],rates[i]))
     numEvents[j] <- rbinom(1,size=state[eSrc],prob=rates[j])
   }
   print(paste("------------ i= ",i))
   print("rates")
   print(rates)
   print(paste("numEvents=",numEvents, " t=",t))
   totDelta <- colSums(deltaArray * numEvents)
   print(totDelta)
   state <- state + totDelta
   print(paste("state=",state))
   #state <- state + f(t,dt,state,params)
   #states[[i]] <- c(t=t,state)
}


gen.stochastic.model <- function(stateNames,events,f.eRates) {



}



