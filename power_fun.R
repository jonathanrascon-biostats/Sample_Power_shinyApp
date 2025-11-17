
library(pwr)

power_fun <- function(x, y, w, z, a){
  power_level <- seq(from = x, to = y, by = .05)
  cohensd_seq <- seq(from = w, to = z, by =.05)
  results.data <- data.frame(d = numeric(), power = numeric(), n = numeric())
  for (i in cohensd_seq) {
    for (j in power_level) {
      test <- pwr.t.test(d = i, sig.level = a, power = j, type = "two.sample",
                         alternative = "two.sided")
      row <- data.frame(d = i, power = j, n = test$n)
      results.data <- rbind(results.data, row)
    }
  }
  results.data$n <- round(results.data$n)
  results.data$d <- factor(results.data$d)
  return(results.data)
}


