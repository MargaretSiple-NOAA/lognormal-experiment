#' Simulate the distribution of sums of lognormal random
#'   variables. These will not necessarily be lognormal even
#'   though we often assume they are. See refs and discussion
#'   here:
#'   https://stats.stackexchange.com/questions/238529/the-sum-of-independent-lognormal-random-variables-appears-lognormal
#' @param means Mean in log space
#' @param sds SD in log space
#' @param nsims Number of simulation replicates to do
#' @param plot Whether to plot the results
simulate_LN_sums <- function(means, sds, nsims=10000, plot=TRUE){
  sims <- sapply(1:length(sds), function(i) rnorm(nsims, mean=means[i], sd=sds[i])) # log(x) normally distributed in each of 4 strata
  sums <- log(rowSums(exp(sims))) # sum x (abundance or biomass) across strata for each sim, in log space. The non-logged version of this is the product that we normally pass to the assessment ppl. 
  test <- ks.test((sums-mean(sums))/sd(sums),pnorm) # test for normality
  swtest <- shapiro.test(x = sample(x = sums,size = 5000)) #shapiro-wilk test for normality
  pval <- test$p.value
  if(plot){
    m <- rbind(c(1,1), c(2,3))
    layout(m)
    plot(0,0, xlim=range(sims), type='n',
         ylim=c(0,max(dnorm(means,means,sds))))
    trash <- sapply(1:length(sds), function(i)
      curve(dnorm(x,means[i], sds[i]), from=min(sims),
            to=max(sims), add=TRUE))
    qqnorm(sums, main=paste("p.value=", pval));qqline(sums) #If s.a. assumption is correct, this should be normal. But it's not!
    hist(sums, freq=FALSE, breaks=nsims/200)
    curve(dnorm(x, mean=mean(sums), sd=sd(sums)), add=TRUE,
          from=min(sums), to=max(sums),
          lwd=2)
  }
}

## real example from combining species, are model estimates in a
## single year for 4 spp
simulate_LN_sums(means = c(5.9,4.3,3.6,4.7), sds =  c(.6,.5,1.9,.7))

## If you shrink the SDs it converges to normal
simulate_LN_sums(c(5.9,4.3,3.6,4.7), c(.6,.5,1.9,.7)/3)
## Or if you increase means by a lot
simulate_LN_sums(c(5.9,4.3,3.6,4.7)*3, c(.6,.5,1.9,.7))


