library(aricode)
library(tidyverse)
sample_size <- 10

summary_results<- read_csv('n_trials_summary_results.csv')  
membership_matrix <- as.matrix(read_csv('n_trials_membership.csv'))
mts <- c( "FG", "IM", "ML", "WT")
method <- summary_results$method
nmis <- data.frame()
print("Hello1")
for (met1 in 1:length(mts)) {
    for (met2 in met1:length(mts)) {
        if (met1 != met2) {
            mb1 <- membership_matrix[,  method == mts[met1]]
            mb2 <- membership_matrix[,  method == mts[met2]]
            sammple_i <- min(ncol(mb1), sample_size)
            sammple_j <- min(ncol(mb2), sample_size)
            for (i in 1:sammple_i) {
                for (j in i:sammple_j) {
                    if (i != j) {
                        nmi_m1_m2 <- aricode::NMI(mb1[, i], mb2[, j])
                        nmis <-rbind(nmis, data.frame(method[met1], method[met2], met1, met2, nmi_m1_m2))
                        print(paste(met1, met2, i,j, nmi_m1_m2))
                    }
                }
            }
            

        }
    }
}
nmis %>% write_csv('NMIs.csv')
print("Hello")

