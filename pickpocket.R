# This is a piece of code for the simulation of pickpocketing in The Elder Scrolls: Skyrim.

pickpocket <- function(P = 0.9) {
    i <- 0
    while (sample(c(1, 0), 1, prob = c(P, 1 - P))) {
        i <- i + 1
    }
    return(i)
}

# The code gets the probability of success and heist count to give a histogram of possibly stolen item count.

trial <- function(P = 0.95, R = 10000) {
    items <- replicate(R, pickpocket(P))
    hist(items)
}
