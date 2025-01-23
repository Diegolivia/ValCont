#R-Code für adjusted Average Deviation of Mean
#Lohse-Bossenz, H., Kunina-Habenicht, O. & Kunter, M. (2014). Estimating Within-group Agreement in Small Groups. A proposed adjustment for the average deviation index. European Journal of Work and Organizational Psychology, 23(3), 456–468. https://doi.org/10.1080/1359432X.2012.748189

#Code based on function from package "multilevel" (Bliese)

library(multilevel)

ad.m.adj<-function (x, grpid, type = "mean")
{
    NEWDAT <- data.frame(x, grpid = grpid)
    NEWDAT <- na.exclude(NEWDAT)
    DATSPLIT <- split(NEWDAT[, 1:(ncol(NEWDAT) - 1)], NEWDAT$grpid)
    if (ncol(as.matrix(x)) > 1) {
        ans1 <- lapply(DATSPLIT, function(Q) {
            if (nrow(Q) > 1) {
                mean(apply(Q, 2, function(AD) {
                  sum(abs(AD - eval(call(paste(type), AD))))/length(AD)
                }))
            }
            else {
                NA
            }
        })
        ans2 <- lapply(DATSPLIT, nrow)
        ans1 <- unlist(ans1)
        ans2 <- unlist(ans2)
        OUTPUT <- data.frame(grpid = names(DATSPLIT), AD.M = ans1,
            gsize = ans2)
        return(OUTPUT)
        stop()
    }
    ans1 <- lapply(DATSPLIT, function(AD) {
        sum(abs(AD - eval(call(paste(type), AD))))/length(AD)
    })
    ans2 <- lapply(DATSPLIT, length)
    ans1 <- unlist(ans1)
    ans2 <- unlist(ans2)
    ans1[ans2 == 1] <- NA
    OUTPUT <- data.frame(grpid = names(DATSPLIT), AD.M = ans1,
        gsize = ans2)
    return(OUTPUT)
}
<environment: namespace:multilevel>
