
# use palin_num function 
problem4 <- function() {
  max <- 0
  for (i in 990:100) {
    vals <- 999:100 * i
    palins <- vals[unlist(lapply(vals, palin_num))]
    max <- max(c(max, palins)) 
  }
  max
}
