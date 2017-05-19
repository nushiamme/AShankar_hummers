# Jags model file for analysis of hierarchy based on multi-level radon model in Gelman & Hill p. 350

model {
  for (i in 1:N) {
    y[i] ~ dbern (pr.hat[i])	#no need to change a thing	
    logit(pr.hat[i]) <-  a[species[i]]  + b1*x1	#x1 is individual mass you give it to model
    
    e.y[i]<- ilogit(y[i] - logit(pr.hat[i]))	#data-level errors, to estimate R2, need to be rescaled --inverse logit-- 
    #or they give nonsensical results
  }
  
  b1 ~ dnorm (0, 0.0001)
  
  CME[1:J] ~ dmnorm(a.hat[],tau_resid[,])	#multivariate normal distribution with correlated residuals
  for (j in 1:J) {	#J = total species
    a[j] ~ dnorm (CME[j], tau.a)	#scaled back to normal distribution as binomial yielded only negative b0s
    
    a.hat[j] <- mu.a		#no predictors
    
    e.a[j] <- a[j] - a.hat[j]	#group-level errors, to estimate R2
  }
  tau_resid[1:J,1:J] ~ dwish(invA[,],J) #where invA is vcv matrix based on phylogeny
  mu.a ~ dnorm (0, 0.0001)
  tau.a <- pow(sigma.a, -2)
  sigma.a ~ dunif (0, 100)
}
