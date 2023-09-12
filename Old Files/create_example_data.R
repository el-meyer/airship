
library(mvtnorm)

input1 <- c("A", "B", "C")
input2 <- c(1, 2, 3)
input3 <- c("Z", "Y", "X")
input4 <- c(11, 12, 13)
replications <- 1:1000

scenarios <- 
  expand.grid(
    replications = replications,
    input1 = input1,
    input2 = input2,
    input3 = input3,
    input4 = input4
  )

for (i in 1:nrow(scenarios)) {
  
  var <- ifelse(scenarios$input1[i] == "A", 1, 10)
  cor <- ifelse(scenarios$input3[i] == "Z", 0.7, 0.1)
  
  out <- rmvnorm(
    1, 
    mean = c(scenarios$input1[i], scenarios$input3[i]),
    sigma = matrix(c(var, cor, cor, var), nrow = 2)
    )
  
  scenarios$output1[i] <- out[1]
  scenarios$output2[i] <- out[2]
  
}

write.csv(scenarios, file = "ExampleData.csv", row.names = FALSE)
