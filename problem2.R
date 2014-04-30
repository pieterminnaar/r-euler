
problem2 <- function() {
  nums <- fibonacci_max(4e6)
  evens <- nums %% 2 == 0
  sum(nums[evens])
}