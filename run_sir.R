load("sir.R")

sir.model <- gen.ode.model(f=sir.ode)

t0=0
t1=5

times.ode <- seq(t0,t1,by=0.005)

params <- c(beta=10,sigma=1)
state0 <- c(S=1000,I=1,R=0)

sir.df <- sir.model(state=state0,times=times.ode,params=params)

plot_ode <- gen.plot.ode(sir.df)
print(plot_ode)

## stochastic simulation


sir.dt0 <- function(t,dt,state,params) {
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

sim.dt.binomial0 <- function(f,state0,params,times,nsim=1) {
  res <- as.list(seq_len(nsim))
  for(sim in seq(nsim)) {
    state <- state0
    i <- 1
    states <- list()
    states[[i]] <- c(t=times[1], state)
    for (t in times[-1]) {
      i <- i+1
      dt <- times[i]-times[i-1]
      state <- state + f(t,dt,state,params)
      states[[i]] <- c(t=t,state)
    }
    res[[sim]] <- as.data.frame(do.call(rbind,states))
  }
  return(res)
}


dt = 0.01
times.sim = seq(t0,t1,by=dt)


sim.dfs <- sim.dt.binomial(f.dt=sir.dt,state0=state0,params=params,times=times.sim,nsim=10)


simplots <- lapply(sim.dfs, function(sim.df) gen.plot.sim(sim.df) )

print(simplots[[3]])


peaksI <- sapply(sim.dfs,function(sim.df) get_peak(sim.df,'t','I'))

bad.sims <- which(peaksI[2,]<=1)

bad.sims

sim.dfs <- sim.dfs[-bad.sims]

mean(peaksI[2,-bad.sims])

mean(peaksI[1,-bad.sims])

#
# averaging over dfs
#

nsims <- length(sim.dfs)
avg.I <- lapply(seq(length(times.sim)), 
       function(t) mean(sapply(seq(nsims), function(nsim) sim.dfs[[nsim]][t,'I'] )))

length(avg.I)
length(times.sim)
length(times.ode)

approx.avg.I <- approx(times.sim,avg.I,times.ode)

new.sir.df <- sir.df

new.sir.df[,'avgI'] <- approx.avg.I$y

plot.ode.avg <- gen.plot.ode(new.sir.df)

pdf("ode_avg_I.pdf")
print(plot.ode.avg)
dev.off()







