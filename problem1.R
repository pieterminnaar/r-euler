# If we list all the natural numbers below 10 that are multiples of 3 or 5, 
# we get 3, 5, 6 and 9. The sum of these multiples is 23. 
# 
# Find the sum of all the multiples of 3 or 5 below 1000.

problem1 <- function(n) {
  nums <- 1:n
  mods <- ((nums %% 3) == 0) | ((nums %% 5) == 0)
  sum(nums[mods])  
}