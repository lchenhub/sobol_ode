#' Forest Growth Model
#'
#' @param t Time
#' @param C Forest size
#' @param params List of parameters including `pre_canopy_closure_rate`, `post_canopy_closure_rate`, `carrying_capacity`, and `canopy_closure_threshold`
#' @return A list containing the rate of change of forest size
#' @export
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
