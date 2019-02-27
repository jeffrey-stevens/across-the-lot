stacks <- c(rep(1:14, each=14), rep(15:27, each=30))

# Now randomize
set.seed(399)
stacks_rand <- sample(stacks)

# Split into 6 columns
stacks_rand_2 <- matrix(stacks_rand, ncol=4)

write.csv(stacks_rand_2, "../Plate maps/MfgRandomization.csv", row.names=FALSE)
