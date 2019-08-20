# SIR Model

The files in this repository implement two stochastic simulators for the SIR model presented in exercise.pdf. The main files are:
* sir.R: Implementation of SIR as a deterministic model (as a set of differential equations passed to an ODE solver) and as a stochastic model using population-based simulation.
* sir2.R: Presents a reworking of the first simulator by implementing a simulator generator that takes an input a descrition of the stochastic model (events and rate formulae) - an attempt to a more general approach.
* run_sir.R: Sample code that uses both simulators. 

The code presented in the following sections can be found in run_sir.R

## Deterministic Model

The deterministic solves the SIR differential equations defined by `sir.ode` by calling deSolve.ode on some initial values and parameters. The derivatives are defined in file sir.R:
```
sir.ode <- function(t,state,params) {
  with(as.list(c(state, params)), {
    N <- S+I+R
    dS <- -beta*S*I/N
    dI <- beta*S*I/N - sigma*I
    dR <- sigma*I
    return(list(c(dS, dI, dR)))
 })
}
```

The initial condition and parameters are the same thoughout the example:
```
t0 <- 0
t1 <- 5
params <- c(beta=10,sigma=1)
state0 <- c(S=1000,I=1,R=0)
```
The solver is called as follows:
```
source("sir.R")
sir.ode.model <- gen.ode.model(f=sir.ode)
sirODEdf <- sir.ode.model(state=state0,times=timesODE,params=params)
plotODE <- gen.plot.ode(sirODEdf)
print(plotODE)
```
The function `sir.ode.model` calls the solver and returns a dataframe, which is then plotted by `gen.plot.ode`.

![Deterministic](sir_ode.png)


## Stochastic Model v1

![Stochastic Run](sir_sim3.png)

![ODE vs average](sir_cmp.png)

## Stochastic Model v2

![Comparison several runs](sir_cmp_all.png)




