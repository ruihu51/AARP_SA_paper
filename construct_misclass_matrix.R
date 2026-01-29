#########################
# generate 9 misclassification matrix
########################
# sym 1 mild
true_prob <- 0.9
false_prob <- 1 - true_prob
p_ij.1.1 <- matrix(c(true_prob, false_prob/2, 0, 0, 0, 0,
                     false_prob, true_prob, false_prob/2, 0, 0, 0,
                     0, false_prob/2, true_prob, false_prob/2, 0, 0,
                     0, 0, false_prob/2, true_prob, false_prob/2, 0,
                     0, 0, 0, false_prob/2, true_prob, false_prob,
                     0, 0, 0, 0, false_prob/2, true_prob), nrow = 6, byrow = TRUE)
dimnames(p_ij.1.1) <- list(levels(aarp_data$SMOKE_DOSE), levels(aarp_data$SMOKE_DOSE))
p_ij.1.1 <- build.mc.matrix(p_ij.1.1, method = "jlt") # do it until the output of the next command is TRUE
check.mc.matrix(list(p_ij.1.1))

# moderate
true_prob <- 0.65
false_prob <- 1 - true_prob
p_ij.1.2 <- matrix(c(true_prob, false_prob/2, 0, 0, 0, 0,
                     false_prob, true_prob, false_prob/2, 0, 0, 0,
                     0, false_prob/2, true_prob, false_prob/2, 0, 0,
                     0, 0, false_prob/2, true_prob, false_prob/2, 0,
                     0, 0, 0, false_prob/2, true_prob, false_prob,
                     0, 0, 0, 0, false_prob/2, true_prob), nrow = 6, byrow = TRUE)
dimnames(p_ij.1.2) <- list(levels(aarp_data$SMOKE_DOSE), levels(aarp_data$SMOKE_DOSE))
p_ij.1.2 <- build.mc.matrix(p_ij.1.2, method = "jlt") # do it until the output of the next command is TRUE
check.mc.matrix(list(p_ij.1.2))

# extreme
true_prob <- 0.35
false_prob <- 1 - true_prob
p_ij.1.3 <- matrix(c(true_prob, false_prob/2, 0, 0, 0, 0,
                     false_prob, true_prob, false_prob/2, 0, 0, 0,
                     0, false_prob/2, true_prob, false_prob/2, 0, 0,
                     0, 0, false_prob/2, true_prob, false_prob/2, 0,
                     0, 0, 0, false_prob/2, true_prob, false_prob,
                     0, 0, 0, 0, false_prob/2, true_prob), nrow = 6, byrow = TRUE)
dimnames(p_ij.1.3) <- list(levels(aarp_data$SMOKE_DOSE), levels(aarp_data$SMOKE_DOSE))
p_ij.1.3 <- build.mc.matrix(p_ij.1.3, method = "jlt") # do it until the output of the next command is TRUE
check.mc.matrix(list(p_ij.1.3))

# asym 1 mild
true_prob <- 0.9
false_prob <- 1 - true_prob
prop <- 1 + 1/2 + 1/4 + 1/8 + 1/16
row <- c(true_prob, false_prob*(1/prop), false_prob*(1/(2*prop)),
         false_prob*(1/(4*prop)), false_prob*(1/(8*prop)), false_prob*(1/(16*prop)))

p_ij.3.1 <- matrix(c(row[1],row[2:6],
                     row[2:1],row[3:6],
                     row[3:1],row[4:6],
                     row[4:1],row[5:6],
                     row[5:1],row[6],
                     row[6:1]), nrow = 6, byrow = FALSE)
check.mc.matrix(list(p_ij.3.1))
dimnames(p_ij.3.1) <- list(levels(aarp_data$SMOKE_DOSE), levels(aarp_data$SMOKE_DOSE))

# asym 1 moderate
true_prob <- 0.7
false_prob <- 1 - true_prob
row <- c(true_prob, false_prob*(1/prop), false_prob*(1/(2*prop)),
         false_prob*(1/(4*prop)), false_prob*(1/(8*prop)), false_prob*(1/(16*prop)))
p_ij.3.2 <- matrix(c(row[1],row[2:6],
                     row[2:1],row[3:6],
                     row[3:1],row[4:6],
                     row[4:1],row[5:6],
                     row[5:1],row[6],
                     row[6:1]), nrow = 6, byrow = FALSE)
check.mc.matrix(list(p_ij.3.2))
dimnames(p_ij.3.2) <- list(levels(aarp_data$SMOKE_DOSE), levels(aarp_data$SMOKE_DOSE))

# asym 1 extreme
true_prob <- 0.5
false_prob <- 1 - true_prob
row <- c(true_prob, false_prob*(1/prop), false_prob*(1/(2*prop)),
         false_prob*(1/(4*prop)), false_prob*(1/(8*prop)), false_prob*(1/(16*prop)))
p_ij.3.3 <- matrix(c(row[1],row[2:6],
                     row[2:1],row[3:6],
                     row[3:1],row[4:6],
                     row[4:1],row[5:6],
                     row[5:1],row[6],
                     row[6:1]), nrow = 6, byrow = FALSE)
check.mc.matrix(list(p_ij.3.3))
dimnames(p_ij.3.3) <- list(levels(aarp_data$SMOKE_DOSE), levels(aarp_data$SMOKE_DOSE))

# asym 2 mild
true_prob <- 0.9
false_prob <- 1 - true_prob
prop <- 1 + 0.75 + (0.75)^2 + (0.75)^3 + (0.75)^4
row <- c(true_prob, false_prob*(1/prop), false_prob*(0.75/prop),
         false_prob*(0.75^2/prop), false_prob*(0.75^3/prop), false_prob*(0.75^4/prop))
p_ij.4.1 <- matrix(c(row[1],row[2:6],
                     row[2:1],row[3:6],
                     row[3:1],row[4:6],
                     row[4:1],row[5:6],
                     row[5:1],row[6],
                     row[6:1]), nrow = 6, byrow = FALSE)
check.mc.matrix(list(p_ij.4.1))
dimnames(p_ij.4.1) <- list(levels(aarp_data$SMOKE_DOSE), levels(aarp_data$SMOKE_DOSE))

# asym 2 moderate
true_prob <- 0.7
false_prob <- 1 - true_prob
row <- c(true_prob, false_prob*(1/prop), false_prob*(0.75/prop),
         false_prob*(0.75^2/prop), false_prob*(0.75^3/prop), false_prob*(0.75^4/prop))
p_ij.4.2 <- matrix(c(row[1],row[2:6],
                     row[2:1],row[3:6],
                     row[3:1],row[4:6],
                     row[4:1],row[5:6],
                     row[5:1],row[6],
                     row[6:1]), nrow = 6, byrow = FALSE)
check.mc.matrix(list(p_ij.4.2))
dimnames(p_ij.4.2) <- list(levels(aarp_data$SMOKE_DOSE), levels(aarp_data$SMOKE_DOSE))

# asym 2 extreme
true_prob <- 0.5
false_prob <- 1 - true_prob
row <- c(true_prob, false_prob*(1/prop), false_prob*(0.75/prop),
         false_prob*(0.75^2/prop), false_prob*(0.75^3/prop), false_prob*(0.75^4/prop))
p_ij.4.3 <- matrix(c(row[1],row[2:6],
                     row[2:1],row[3:6],
                     row[3:1],row[4:6],
                     row[4:1],row[5:6],
                     row[5:1],row[6],
                     row[6:1]), nrow = 6, byrow = FALSE)
check.mc.matrix(list(p_ij.4.3))
dimnames(p_ij.4.3) <- list(levels(aarp_data$SMOKE_DOSE), levels(aarp_data$SMOKE_DOSE))
