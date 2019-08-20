source("sir.R")

t0 <- 0
t1 <- 5
timesODE <- seq(t0,t1,by=0.005)

params <- c(beta=10,sigma=1)
state0 <- c(S=1000,I=1,R=0)

sir.ode.model <- gen.ode.model(f=sir.ode)

sirODEdf <- sir.ode.model(state=state0,times=timesODE,params=params)

plotODE <- gen.plot.ode(sirODEdf)
print(plotODE)


## stochastic simulation

dt = 0.01
timesSim = seq(t0,t1,by=dt)

sim.sir <- gen.simulator(f.dt=sir.dt)
set.seed(0)
simDfs <- sim.sir(state0=state0,params=params,times=timesSim,nsim=10)

print( gen.plot.sim(simDfs[[3]]) )

peaksI <- sapply(simDfs,function(simDf) get_peak(simDf,'t','I'))
badSims <- which(peaksI[2,]<=1)
badSims

print( gen.plot.sim(simDfs[[9]]) )

simDfs <- simDfs[-badSims]

meanIpeak <- mean(peaksI[2,-badSims])
meanTpeak <- mean(peaksI[1,-badSims])
print(c(meanTpeak, meanIpeak))

# averaging over dfs - plot against ODE results

nsims <- length(simDfs)
avgI <- lapply(seq(length(timesSim)), 
       function(t) mean(sapply(seq(nsims), function(nsim) simDfs[[nsim]][t,'I'] )))
approxAvgI <- approx(timesSim,avgI,timesODE)


sirCmpDf <- sirODEdf
sirCmpDf[,'avgI'] <- approxAvgI$y

plotCmp <- gen.plot.ode(sirCmpDf)

#pdf("ode_avg_I.pdf")
print(plotCmp)
#dev.off()

########### stochastic simulation two
source('sir2.R')

stateNames <- c('S','I','R')

sirEvents <- list(
	  make.event('infection','S', list('I'=1,'S'=-1)),
	  make.event('recovery', 'I', list('I'=-1,'R'=1))
	  )

sir.rates.dt <- function(state,params,t,dt) {
  rates <- numeric(2) # must equal to number of events
  with(as.list(c(state, params)), {
    N <- S+I+R
    return(c( (beta*I/N)*dt, sigma*dt   ))
 })
}

sim.sir2 <- gen.model.simulator(stateNames,sirEvents,sir.rates.dt)

set.seed(0)
sirDfs2 <- sim.sir2(state0,params,timesSim,nsim=10)

print(simPlots2[[3]])

peaksI2 <- sapply(sirDfs2,function(simDf) get_peak(simDf,'t','I'))
badSims2 <- which(peaksI[2,]<=1)
badSims2

sirDfs2 <- sirDfs2[-badSims2]

meanIpeak2 <- mean(peaksI2[2,-badSims2])
meanTpeak2 <- mean(peaksI2[1,-badSims2])
print(c(meanTpeak2, meanIpeak2))

############ plot avg 100, 500, 1000

do.avg <- function(dfs) {
  peaksI <- sapply(dfs,function(simDf) get_peak(simDf,'t','I'))
  badSims <- which(peaksI[2,]<=1)
  dfs <- dfs[-badSims]

  nsims <- length(dfs)
  avgI <- lapply(seq(length(timesSim)), 
       function(t) mean(sapply(seq(nsims), function(nsim) dfs[[nsim]][t,'I'] )))
  approxAvgI <- approx(timesSim,avgI,timesODE)
  return(approxAvgI)
}

sirCmpDf <- sirODEdf

set.seed(0)
sirDfs2 <- sim.sir2(state0,params,timesSim,nsim=100)
avg2 <- do.avg(sirDfs2)
sirCmpDf[,'avgI_100'] <- avg2$y

set.seed(0)
sirDfs2 <- sim.sir2(state0,params,timesSim,nsim=500)
avg2 <- do.avg(sirDfs2)
sirCmpDf[,'avgI_500'] <- avg2$y

set.seed(0)
sirDfs2 <- sim.sir2(state0,params,timesSim,nsim=1000)
avg2 <- do.avg(sirDfs2)
sirCmpDf[,'avgI_1000'] <- avg2$y

plotCmp <- gen.plot.ode(sirCmpDf)

#pdf("sir_cmp.pdf")
print(plotCmp)
#dev.off()


