forest_growth <- function(t, C, params) {
  with(as.list(c(params, C)), {
    r <- pre_canopy_closure_rate
    g <- post_canopy_closure_rate
    K <- carrying_capacity
    canopy_closure_threshold <- canopy_closure_threshold
    
    if (C < canopy_closure_threshold) {
      dC <- r * C
    } else {
      dC <- g * (1 - C / K)
    }
    
    return(list(dC))
  })
}