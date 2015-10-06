
dominate.greedy <- function (g)
{
    od <- degree(g, mode = "out") + 1
    S <- NULL
    A <- get.adjacency(g)
    diag(A) <- 0
    n <- nrow(A)
    covered <- rep(0, n)
    while (sum(covered) < n ) {
        i <- which.max(od)
        covered[A[i, ] > 0] <- 1
        covered[i] <- 1
        S <- c(S, i)
        A[, covered > 0] <- 0
        h <- graph.adjacency(A, mode = "directed")
        od <- degree(h, mode = "out") + 1 - covered
    }
    S
}

