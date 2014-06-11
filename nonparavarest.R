# Function estimateSigmaSq<-function (explanatory, response) 
# Gasser - Sroka - Jennen - Steinmetz nonparametric variance estimator
# Arguments :
#    x: Design points (n,m)
#    y: Function observations(n,m)
#    Here n is the sample seze ad m is the number of functions or datasets
estimateSigmaSq<-function (explanatory, response) 
{
  ind = order(explanatory, decreasing = FALSE)
  if (sum(diff(ind) < 0) != 0) {
    explanatory = explanatory[ind]
    response = response[ind]
  }
  n = length(response)
  a = b = eps = rep(0, n - 2)
  for (i in 2:(n - 1)) {
    x = explanatory[(i - 1):(i + 1)]
    a[i - 1] = (x[3] - x[2])/(x[3] - x[1])
    b[i - 1] = (x[2] - x[1])/(x[3] - x[1])
    eps[i - 1] = a[i - 1] * response[i - 1] + b[i - 1] * 
      response[i + 1] - response[i]
  }
  cSq = 1/(a^2 + b^2 + 1)
  list(sigmaSq = 1/(n - 2) * sum(cSq * eps^2), a = a, b = b, 
       eps = eps)
}
