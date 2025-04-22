---
editor_options: 
  markdown: 
    wrap: 72
---

A R package for binary risky choice and intertemporal choice modeling

**Risky Choice models include:**

1.  Expected Utility Theory: $U = p*A^a$
2.  Risk-Return: $U=EV - b*Var$
3.  Coefficient of Variation: $U=EV-b*CV$ where $CV=\sqrt{Var}/EV$
4.  Hyperbolic: $U=A/(1+h*\theta)$ where $\theta = (1-p)/p$

$A$ is the payoff amount, $p$ is the probability of winning that
outcome, $EV$ is Expected value ($Ap$), and is variance
($p(A-EV)^2 +(1-p)(-EV)^2$)

The util_rc function takes risky choice data (probability, payoffs, and
decisions) of 2 options as inputs, fits a model to the data, and returns
a output vector that stores the estimated parameter, inverse
temperature, fit metrics, model type, and number of observations.

**Intertemporal Choice model include:**

1.  Exponential: $U=A*e^{-kD}$
2.  Hyperbolic: $U=A/(1+kD)$
3.  Quasi-Hyperbolic: $U=A\beta e^{-kD}$
4.  Generalized Hyperbolic: $U=A/(1+kD)^{s}$

$A$ is the payoff amount, $D$ is delay, and $\beta$ and $s$ are extra
parameters for Quasi Hyperbolic and Generalized Hyperbolic models
respectively.

The util_itc function takes a model type and data (choices, payoffs, and
delays) for two options and fits a model to the data. It returns a
output vector that stores the estimated parameters (k, inverse
temperature, an extra parameter s or b), fit metrics, model type, and
number of observations

Warnings will be issued if all choices in the input data are one-sided
(all 0 or 1), or if the fitted model predicts all one-sided choices.

To obtain fitted parameters optimized by package functions,

```{=R}
# util_itc example
  > example <- util_itc("Q", c(1,0,1), c(20,20,20), c(1,1,1), c(30,40,50), c(2,4,6)) 
  > example
  [[1]]
  0.4054651 

  [[2]]
  3.159927 

  [[3]]
  0.2074759 

  [[4]]
  [1] "L-BFGS-B"

  [[5]]
  [1] "Risk Return"

  [[6]]
  [1] 3

  > est_k <- example[[1]]
  > est_invtemp <- example[[2]] 
  > est_b <-example[[3]]
```
```{=R}
# util_rc example
  > example <- util_rc("E", c(0,0,1), c(10,10,10), c(1,1,1), c(20,30,40), c(0.6,0.5,0.4))
  > example
  [[1]]
  0.7369656
  [[2]]
  1.800271  
  [[3]]
  [1] "L-BFGS-B"
  [[4]]
  [1] "Expected Utility Theory"
  [[5]]
  [1] 3
  > est_a <- example[[1]]
  > est_invtemp <- example[[2]] 
```
