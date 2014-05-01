
fibonacci_length <- function(n) {
  fibvals <- numeric(n)
  fibvals[1] = 1
  fibvals[2] = 2
  for (i in 3:n) {
    fibvals[i] = fibvals[i - 1] + fibvals[i - 2]
  }
  fibvals 
}

fibonacci_max <- function(n) {
  nums <- c(1, 2)
  repeat {
    len <- length(nums)
    n1 <- nums[len - 1]
    n2 <- nums[len]
    if ((n2 + n1) >= n) 	 
      break;
    nums <- c(nums, n1 + n2)
  }
  nums 
}

prime_factors <- function(num) {
  # Calculates factors of num. If num is not prime, excludes 1 and itself; 
  # otherwise, gives 1 and itself
  factor <- 2
  factors <- c()
  max <- num 
  while (factor <= max) {
    if (num %% factor == 0) {
        max <- num / factor
      if (length(factors[factor %% factors == 0]) == 0) {
        factors <- c(factors, factor)
      }
    }
    factor <- factor + 1
  }  
  factors
}



factors <- function(num) {
  # Calculates factors of num. If num is not prime, excludes 1 and itself; 
  # otherwise, gives 1 and itself
  factor <- 2
  max.factor <- num   # upper bound for factor
  factors <- c()
  while (factor <= max.factor) {
    if (num %% factor == 0) {
      max.factor <- num / factor
      factors <- c(factors, factor, max.factor)
    }
    factor <- factor + 1
  }  
  factors <- sort(factors)
  factors <- unique(factors)
  factors
}

palin_num <- function(num) {
  vals <- strsplit(as.character(num), "")
  revchars <- rev(unlist(vals))
  revstr <- paste(revchars, collapse = "")
  num == as.numeric(revstr)
}

primes_sieve <- function(n) {
   n <- as.integer(n)
   if(n > 1e6) stop("n too large")
   primes <- rep(TRUE, n)
   primes[1] <- FALSE
   last.prime <- 2L
   for(i in last.prime:floor(sqrt(n)))
   {
      primes[seq.int(2L*last.prime, n, last.prime)] <- FALSE
      last.prime <- last.prime + min(which(primes[(last.prime+1):n]))
   }
   which(primes)
}