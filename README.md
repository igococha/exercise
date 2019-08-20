# SIR Model

The files in this repository implement two stochastic simulators for the SIR model presented in exercise.pdf. The main files are:
* sir.R: Implementation of SIR as a deterministic model (as a set of differential equations passed to an ODE solver) and as a stochastic model using population-based simulation.
* sir2.R: Presents a reworking of the first simulator by implementing a simulator generator that takes an input a descrition of the stochastic model (events and rate formulae) - an attempt to a more general approach.
* run_sir.R: Sample code that uses both simulators. 

The code presented in the following sections can be found in run_sir.R

## Deterministic Model


![Deterministic](sir_ode.pdf)


## Stochastic Model v1





## Stochastic Model v2




