# Master branch

# The birthday paradox refers to the counterintuitive fact that only 23 people 
# are needed for that probability to exceed 50%.

# Birthday paradox
rm(list=ls())

gen_same_bday_prob <- function(people_count){
  numerator <- 1
  for (i in (365 - people_count + 1):365) {
    numerator <- numerator * i
  }
  
  denominator <- 365^people_count
  
  print(paste0(round((1 - numerator / denominator)*100,2), "%"))
}


gen_same_bday_prob(27)

# Changes on branch 1
gen_same_bday_prob(25)

