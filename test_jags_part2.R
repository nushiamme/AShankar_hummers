sink("Bayesian/LogN.jags")

cat("
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


    
    #Assess Model Fit
    
    #Fit discrepancy statistics
    eval[x]<-mu[Bird[x],Plant[x],Time[x]]
    E[x]<-pow((Yobs[x]-eval[x]),2)/(eval[x]+0.5)
    
    ynew[x]~dnorm(mu[Bird[x],Plant[x],Time[x]],tau_obs)
    E.new[x]<-pow((ynew[x]-eval[x]),2)/(eval[x]+0.5)
    
    }
    
    for (i in 1:Birds){
    alpha[i] ~ dnorm(intercept,tau_alpha)
    beta1[i] ~ dnorm(gamma1,tau_beta1)
    beta2[i] ~ dnorm(gamma2,tau_beta2)    
    beta3[i] ~ dnorm(gamma3,tau_beta3)    
    }
    
    #Hyperpriors
    #Slope grouping
    gamma1~dnorm(0,0.0001)
    gamma2~dnorm(0,0.0001)
    gamma3~dnorm(0,0.0001)
    
    #Intercept grouping
    intercept~dnorm(0,0.0001)
    
    # Group intercept variance
    tau_alpha ~ dgamma(0.0001,0.0001)
    sigma_int<-pow(1/tau_alpha,0.5) 
    
    #Observation variance, turning precision to sd
    tau_obs ~ dgamma(0.0001,0.0001)
    sigma_obs<-pow(1/tau_obs,0.5)
    
    #Slope variance, turning precision to sd
    tau_beta1 ~ dgamma(0.0001,0.0001)
    sigma_slope1<-pow(1/tau_beta1,0.5)
    
    tau_beta2 ~ dgamma(0.0001,0.0001)
    sigma_slope2<-pow(1/tau_beta2,0.5)
    
    tau_beta3 ~ dgamma(0.0001,0.0001)
    sigma_slope3<-pow(1/tau_beta3,0.5)
    
    #derived posterior check
    fit<-sum(E[]) #Discrepancy for the observed data
    fitnew<-sum(E.new[])
    
    }
    ",fill=TRUE)

sink()