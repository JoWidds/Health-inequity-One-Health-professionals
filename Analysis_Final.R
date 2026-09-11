#### Final Analysis for paper ####
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path)) #set file location as working directory
if(!require(tidyr)){
  install.packages("tidyr")
  library(tidyr)
}

rm(list=ls())  

dt <- read.csv("C:/Users/jw0104/OneDrive - University of Surrey/Documents/Elicitation/WOHC/Analysis and code/Arthur Stata analysis Sept 24/long_data_equity_including_new_respondents.csv")

dt1 <- subset(dt, part2 == 0 & part3 == 0)  # Both part2 and part3 are 0
dt2 <- subset(dt, part2 == 1 & part3 == 0) # part2 is 1, part3 is 0
dt3 <- subset(dt, part3 == 1 & part2 == 0) # part3 is 1, part2 is 0

length(unique(dt$id))
tapply(dt$id, dt$survey, function(x) length(unique(x))) ## sum of ppl by survey enrolment 
sum(tapply(dt$id, dt$survey, function(x) length(unique(x)))) #(total number of ppl) 

### Question 36 preference - Likert scale 
#Strong.agree	1, Somewhat.agree	2, Neither agree/disagree	3, Somehwhat.dis	4, Strong.dis	5

tmp <- unique(dt[c("id", "q36preference")])

tmp$q36preference <- trimws(as.character(tmp$q36preference))
tmp$q36preference <- gsub(",", ".", tmp$q36preference)

# Convert to numeric, preserving NAs
tmp$q36preference <- suppressWarnings(as.numeric(tmp$q36preference))

table(
  factor(tmp$q36preference, levels = c(1, 2, 3, 4, 5)),
  useNA = "ifany"
)

## Question 40 - Equity concept - Likert scale - 
## Strong.agree	1, Somewhat.agree	2, Neither	3, Somehwhat.dis	4, Strong.dis	5


tmp <- unique(dt[c("id", "q40equity_concept")])
tmp$q40equity_concept <- trimws(as.character(tmp$q40equity_concept))
tmp$q40equity_concept <- gsub(",", ".", tmp$q40equity_concept)

# Convert to numeric, preserving NAs
tmp$q40equity_concept <- suppressWarnings(as.numeric(tmp$q40equity_concept))

table(
  factor(tmp$q40equity_concept, levels = c(1, 2, 3, 4, 5)),
  useNA = "ifany"
)



### 
scenario <- 0

if (scenario == 1) {
  dt <- dt1
} else if (scenario == 2) {
  dt <- dt2
} else if (scenario == 3) {
  dt <- dt3
} else if (scenario == 0) { ##all scenarios combined 
  dt <- dt
}

## Define model
model <- function(beta){
  cesL = 0.5*(dt$a1t^beta)+0.5*(dt$v1t^beta)
  cesR = 0.5*(dt$a2t^beta)+0.5*(dt$v2t^beta)
  cesDiff = cesL - cesR
  
  ln <- ifelse(dt$q == 1, log(pnorm(cesDiff)), log(pnorm(-cesDiff)))  # choice A = 0, B = 1
  return(-sum(ln, na.rm = T))
}

## Run OPTIM function 
beta <- 0.5

result <- optim(beta,model,lower = -1, upper=1, method = "L-BFGS-B", hessian = TRUE)

# Compute the variance (inverse of the Hessian)
variance <- solve(result$hessian)

# Standard deviation (square root of the variance)
std_error <- sqrt(diag(variance))

# Print results
cat("Estimated beta:", result$par, "\n")
cat("Standard error:", std_error, "\n")


modelKolm <- function(beta){
  cesL = dt$HaveA-(1/beta * log(0.5*exp(beta*(dt$HaveA-dt$a1t))+0.5*exp(beta*(dt$HaveA-dt$v1t))))
  cesR = dt$HaveB-(1/beta * log(0.5*exp(beta*(dt$HaveB-dt$a2t))+0.5*exp(beta*(dt$HaveB-dt$v2t))))
  cesDiff = cesL - cesR
  ln <- ifelse(dt$q == 1, log(pnorm(cesDiff)), log(pnorm(-cesDiff)))  # choice A = 0, B = 1
  return(-sum(ln, na.rm = T))
}

result <- optim(beta,modelKolm,lower = -1, upper=1, method = "L-BFGS-B", hessian = T)

variance <- solve(result$hessian)
std_error <- sqrt(diag(variance))
cat("Estimated beta:", result$par, "\n")
cat("Standard error:", std_error, "\n")


modelAtkinson <- function(beta){
  cesL = dt$HaveA* (0.5 * ((dt$a1t/dt$HaveA)^(1-beta)) + 0.5*((dt$v1t/dt$HaveA)^(1-beta)))^(1/(1-beta))
  cesR = dt$HaveB* (0.5 * ((dt$a2t/dt$HaveB)^(1-beta)) + 0.5*((dt$v2t/dt$HaveB)^(1-beta)))^(1/(1-beta))
  cesDiff = cesL - cesR
  ln <- ifelse(dt$q == 1, log(pnorm(cesDiff)), log(pnorm(-cesDiff)))  # choice A = 0, B = 1
  return(-sum(ln, na.rm = T))
}

result <- optim(beta,modelAtkinson,lower = -1, upper=1, method = "L-BFGS-B", hessian = T)

variance <- solve(result$hessian)
std_error <- sqrt(diag(variance))
cat("Estimated beta:", result$par, "\n")
cat("Standard error:", std_error, "\n")


