### Introduction

In econometrics, the autoregressive conditional heteroscedasticity (ARCH) model is a statistical model for time series data that describes the variance of the current error term or innovation as a function of the actual sizes of the previous time periods' error terms; often the variance is related to the squares of the previous innovations. The ARCH model is appropriate when the error variance in a time series follows an autoregressive (AR) model; if an autoregressive moving average (ARMA) model is assumed for the error variance, the model is a generalized autoregressive conditional heteroskedasticity (GARCH) model.

In that case, the GARCH ($p$, $q$) model (where $p$ is the order of the GARCH terms $\sigma^{2}$ $\sigma ^{2}$ and $q$ is the order of the ARCH terms $\epsilon ^{2}$), following the notation of the original paper, is given by

$$ y_{t}=x'_{t}b+\epsilon _{t}$$

$$ \epsilon _{t}|\psi _{t-1}\sim \mathcal {N}(0,\sigma _{t}^{2})$$

$$ \sigma _{t}^{2}=\omega +\sum _{i=1}^{q}\alpha _{i}\epsilon _{t-i}^{2}+\sum _{i=1}^{p}\beta _{i}\sigma _{t-i}^{2}$$


Sources: [https://en.wikipedia.org/wiki/Autoregressive_conditional_heteroskedasticity](https://en.wikipedia.org/wiki/Autoregressive_conditional_heteroskedasticity)