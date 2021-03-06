S.NW<- function (tt, h = NULL, Ker = Ker.norm, w = NULL, cv = FALSE) 
{
  if (is.matrix(tt)) {
    if (ncol(tt) != nrow(tt)) {
      if (ncol(tt) == 1) {
        tt = as.vector(tt)
        tt = abs(outer(tt, tt, "-"))
      }
    }
  }
  else if (is.vector(tt)) 
    tt = abs(outer(tt, tt, "-"))
  else stop("Error: incorrect arguments passed")
  if (is.null(h)) {
    h = quantile(tt, probs = 0.15, na.rm = TRUE)
    cat("h=")
    print(h)
  }
  if (cv) 
    diag(tt) = Inf
  tt2 <- as.matrix(sweep(tt, 1, h, FUN = "/"))
  k <- Ker(tt2)
  if (is.null(w)) 
    w <- rep(1, len = ncol(tt))
  k1 <- sweep(k, 2, w, FUN = "*")
  S = k1/rowSums(k1)
  return(S)
}

S.LLR<- function (tt, h, Ker = Ker.norm, w = NULL, cv = FALSE) 
{
  if (is.matrix(tt)) {
    if (ncol(tt) != nrow(tt)) {
      if (ncol(tt) == 1) {
        tt = as.vector(tt)
        tt = abs(outer(tt, tt, "-"))
      }
    }
  }
  else if (is.vector(tt)) 
    tt = abs(outer(tt, tt, "-"))
  else stop("Error: incorrect arguments passed")
  if (cv) 
    diag(tt) = Inf
  k = Ker(tt/h)
  if (cv) 
    diag(k) = 0
  S1 = rowSums(k * tt, na.rm = TRUE)
  S2 = rowSums(k * (tt^2), na.rm = TRUE)
  b = k * (S2 - tt * S1)
  if (cv) 
    diag(b) = 0
  if (is.null(w)) 
    w <- rep(1, nrow(b))
  b <- sweep(b, 1, w, FUN = "*")
  res = b/rowSums(b, na.rm = TRUE)
  return(res)
}
