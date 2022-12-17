# Final Project 
# Sandhya Rajagopalan


ANES <- load("ANES.rda")

###############################################################################
# Difference of Means Test #
###############################################################################

########## STEP 1: Data Clean-up
# Getting rid of the NA's and replacing them with the value "NA"
# Getting rid of anything besides the scalar verison of the difficulty voting scale
da38034.0001$V202119 <- replace(da38034.0001$V202119, da38034.0001$V202119== "(-9) -9. Refused", NA)
da38034.0001$V202119 <- replace(da38034.0001$V202119, da38034.0001$V202119== "(-7) -7. No post-election data, deleted due to incomplete interview", NA)
da38034.0001$V202119 <- replace(da38034.0001$V202119, da38034.0001$V202119== "(-6) -6. No post-election interview", NA)
da38034.0001$V202119 <- replace(da38034.0001$V202119, da38034.0001$V202119== "(-5) -5. Interview breakoff (sufficient partial IW)", NA)
da38034.0001$V202119 <- replace(da38034.0001$V202119, da38034.0001$V202119== "(-1) -1. Inapplicable", NA)

#Getting rid of those who did not answer the question about identifying their race
da38034.0001$V201549X <- replace(da38034.0001$V201549X, da38034.0001$V201549X== "(-9) -9. Refused", NA)
da38034.0001$V201549X <- replace(da38034.0001$V201549X, da38034.0001$V201549X== "(-8) -8. Don't know", NA)

#Making a new numeric variable for the difficulty in voting
da38034.0001$diffvoting <- NA
da38034.0001$diffvoting[da38034.0001$V202119== "(1) 1. Not difficult at all"] <- 1
da38034.0001$diffvoting[da38034.0001$V202119== "(2) 2. A little difficult"] <- 2
da38034.0001$diffvoting[da38034.0001$V202119== "(3) 3. Moderately difficult"] <- 3
da38034.0001$diffvoting[da38034.0001$V202119== "(4) 4. Very difficult"] <- 4
da38034.0001$diffvoting[da38034.0001$V202119== "(5) 5. Extremely difficult"] <- 5

class (da38034.0001$diffvoting)
da38034.0001$diffvoting<- as.numeric(da38034.0001$diffvoting)

########## STEP 2: Data Organization
# Creating subsets for the two groups that we are studying: individuals that are white and individuals that are black

# subset = white people 
white <- subset (da38034.0001,V201549X == "(1) 1. White, non-Hispanic")
# check to see if this works/if the t-test will work
meanofdiffvotingforwhite <- mean(white$diffvoting, na.rm=T)

# subset = black people 
black <- subset (da38034.0001,V201549X == "(2) 2. Black, non-Hispanic")
# check to see if this works/if the t-test will work
meanofdiffvotingforblack <- mean(black$diffvoting, na.rm=T)


########## STEP 3: T-Test
test <- t.test(white$diffvoting, black$diffvoting, na.rm=T)

#check to see if the t-test works
meanofdiffvotingforwhite - meanofdiffvotingforblack
# answer is -0.04202247, which falls between the 95% Confidence Interval.

meanvalues <- c(1.164027, 1.206049)

########## STEP 4: Graphs 

barplot (meanvalues,
         ylim = c(0, 1.4),
         names = c ("White", "Black"),
         col = "skyblue3",
         main = "Mean Opinions about Difficulty in Voting Between White and Black Individuals",
         cex.main = .9,
         ylab = "Mean Opinions", 
         cex.lab = .9,
         xlab = "Different Groups of Individuals",
         las = 1)
text(x = meanvalues, labels = as.numeric(meanvalues), pos = 3, cex = 0.8, col = "black")
abline(h=0)
abline(h=1.164027)



