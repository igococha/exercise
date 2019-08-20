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

## stochastic simulation one


sir.dt <- function(t,dt,state,params) {
 with(as.list(c(state, params)), {
    N <- S+I+R
    probSI <- beta*I*dt/N # infection
    probIR <- sigma*dt    # recovery
    flowSI <- rbinom(n=1,size=S,prob=probSI)
    flowIR <- rbinom(n=1,size=I,prob=probIR)
    # removed list constructor
    return(c(-flowSI, flowSI-flowIR, flowIR))
 })
}


gen.simulator <- function(f.dt) {
  f <- function(state0,params,times,nsim=1) {
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
  return(f)
}

gen.plot.sim <- function(simdf) {
  simM = melt(simdf, id.vars='t')
  p <- ggplot(simM, aes(x = t, y = value, color = variable)) +
    theme_bw() + geom_line()
  return(p)
}

get_peak <- function(simdf,tlabel,deme) {
  idx <- which.max(sapply(simdf[deme], function(x) x))
  return( c( t=mean(simdf[idx,tlabel]), deme=mean(simdf[idx,deme] )))
}






