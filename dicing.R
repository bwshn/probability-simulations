dicing <- function(n, x = 0) {
    library(tidyr)
    if (x == 0) {
        par(mfrow = c(2, 1))
        #need to find a better alternative for expand.grid
        all <- expand.grid(as.data.frame(matrix(1:6, 6, n)))
        x_break <- seq(1, 6 * n, 1)
        prob <- c()
        cum_prob <- c()
        for (i in x_break) {
            prob <- c(prob, mean(rowSums(all) == i))
            cum_prob <- c(cum_prob, mean(rowSums(all) <= i))
        }
        plot(x_break, prob)
        plot(x_break, cum_prob)
    } else if ((x < 1 * n) | (x > 6 * n)) {
        print("Invalid entry!")
    } else {
        #need to find a better alternative for expand.grid
        all <- expand.grid(as.data.frame(matrix(1:6, 6, n)))
        tibble(
            Equal = mean(rowSums(all) == x),
            Not_eq = mean(rowSums(all) != x),
            More_eq = mean(rowSums(all) >= x),
            Less_eq = mean(rowSums(all) <= x),
            More = mean(rowSums(all) > x),
            Less = mean(rowSums(all) < x)
        )
    }
}
