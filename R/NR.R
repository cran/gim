
# Newton-Raphson algorithm
NR <- function(para, map, family, data, ref, V, bet0, sample.info, outcome, type, silent){
  
  inv.V <- solve(V)
  np <- length(para)
  para.null <- rep(NA, np)
  
  i <- 0
  while(i<1000){
    #fn <- compute.obj(para, map, family, data, ref, inv.V, bet0, sample.info, outcome, type)
    
    s0 <- compute.score(para, map, family, data, ref, inv.V, bet0, sample.info, outcome, type)
    
    if(!silent) cat('iter = ', i+1, '\t', formatC(max(abs(s0)), digits = 2, format = 'e'), '           \r')
    
    if(all(abs(s0) < 1e-6)){
      break
    }
    
    h0 <- compute.hess(para, map, family, data, ref, inv.V, bet0, sample.info, outcome, type)
    
    t0 <- try(d0 <- solve(h0, s0), silent = TRUE)
    if('try-error' %in% class(t0)){
      stop('hess fails')
    }
    #d0 <- d0/sqrt(sum(p^2))
    
    para <- para - d0
    
    i <- i + 1
    
  }
  
  if(all(abs(s0) > 1e-6)){
    stop('NR does not converge')
  }
  
  list(coefficients = para, score = s0, conv = ifelse(all(abs(s0) < 1e-6), 1, 0))
  
}
