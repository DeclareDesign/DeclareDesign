
library(DeclareDesign)
df <- data.frame(ID = 1:10, block_var = rep(c("A", "B"), 5, 5))

f_1 <- declare_assignment(m = 5)
f_1(df)

f_1 <- declare_assignment(num_arms = 2)
f_1(df)

f_1 <- declare_assignment(num_arms = 3)
f_1(df)

##debugonce(declare_assignment)
f_1 <- declare_assignment(block_var = block_var)
f_1(df)


# what about inside a function?

new_fun <- function(num_arms){
  f_1 <- declare_assignment(num_arms = num_arms)
  f_1(df)
}
# Breaks rn
#new_fun(3)


