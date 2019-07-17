#"Thomspon Algorithm" - really simple idea: for each iteration just sample from the posterior distribution of the parameters and then your "action" is based on whichever one has the "best" value.
#i.e. if we have machines that each pay out 1 dollar per play with probability p_i, where i is the machine index 0,1,2, then the machine we play each turn is chosen by drawing p_0, p_1, and p_2 from their posterior distributions, and then playing whichever machine has the highest drawn value this turn.
#This naturally creates an "optimal" algorithm that maximizes the expected return compared to a greedy algorithm or anything else.
#Note that for the prior when initializing we can either use flat prior for each, or, if we do have actual information, we can be even better than any naive method by actually using appropriate prior.

#we will do the slot machines first, using p_i~beta(a_i,b_i) prior.Therefore posterior at any point in time will just be p_i|data_i ~ beta(a_i+#success_i)/(b_i+#failures_i)
true_p_s = c(.6, .5, .7)

#We will use beta(1,1) ~ unif(0,1) priors to reflect our initial lack of knowledge. This is equivalent to 0 pseudo observations
hist( rbeta(n=1000,shape1=1,shape2 = 1))

prior_a_s = c(1, 1, 1)
prior_b_s = c(1, 1, 1)

#now initialize data (with 0's obviously)
data_success_counts = c(0,0,0)
data_failure_counts = c(0,0,0)

choices=rep(NA,1000)
for(iteration in 1:1000){ #this is a pull of a slot machine
  p_stars = c(0,0,0) #this stores our parameter values sampled from the posterior for the round
  for(i in 1:3){ #sample p's from the posterior
    p_stars[i] = rbeta(n=1,shape1=prior_a_s[i]+data_success_counts[i],shape2 = prior_b_s[i]+data_failure_counts[i]) #this is just the posterior  p_i|data_i ~ beta(a_i+#success_i)/(b_i+#failures_i)
  }
  machine_to_pull = which.max(p_stars) #the action we take is based on the "best" parameter sampled- in this case we want to win money, so we want to choose the machine with the maximum p. In another scenario we may choose the group with the lowest mu or sigma whatever, or, if our goal was to find the fairest machine we could actually choose the machine with the p closest to .5
  
  choices[iteration]=machine_to_pull #store the choice just for diagnostics
  
  #now actually pull that machine
  result = rbinom(1,1,true_p_s[machine_to_pull])
  
  #update the data:
  data_success_counts[machine_to_pull] = data_success_counts[machine_to_pull] + result
  data_failure_counts[machine_to_pull] = data_failure_counts[machine_to_pull] + (1-result)
}

data_success_counts
data_failure_counts
plot(choices)


#note that for these parameters, if we rerun the experiment we can get dramaticaly different sequences of choices, but that the total success and failure counts always reflect a huge preference for the machine with the highest p.
#this is because the beginning has high variance, so sometimes we quickly learn that one or both of the "bad" machines are bad, or other times we keep trying one or both over time. however, in all cases we "converge" quickly on the best machine, the only thing different is how much and how long we continue to explore the other two. This is unaviodable of course, since we know nothing about them.



#now do the same experiment with a greed algorithm:
#now initialize data (with 0's obviously)
data_success_counts_greedy = c(0,0,0)
data_failure_counts_greedy = c(0,0,0)
choices_greedy=rep(NA,1000)
for(iteration in 1:1000){ #this is a pull of a slot machine
  for(i in 1:3){
    if(data_success_counts_greedy[i]+data_failure_counts_greedy[i]==0){
      p_stars[i] =.5
    }else{
      p_stars[i] = data_success_counts_greedy[i]/(data_success_counts_greedy[i]+data_failure_counts_greedy[i]) - runif(1)*.1 #this dithering is strictly negative and therefore at least 1 exploration of each
    } 
  }

  machine_to_pull = which.max(p_stars)
  choices_greedy[iteration]=machine_to_pull #store the choice just for diagnostics
  
  #now actually pull that machine
  result = rbinom(1,1,true_p_s[machine_to_pull])
  
  #update the data:
  data_success_counts_greedy[machine_to_pull] = data_success_counts_greedy[machine_to_pull] + result
  data_failure_counts_greedy[machine_to_pull] = data_failure_counts_greedy[machine_to_pull] + (1-result)
}

data_success_counts_greedy
data_failure_counts_greedy
plot(choices_greedy)

#note that what can happen with this greedy algorithm is that sometimes you are completely right but other times you are completely wrong
#to avoid failure you would have to include more and more dither, which reduces your advantage.

#clearly this is SUPER fragile and fails really easily since it never actually explores.
#we could add jitter but that is dumb, since we would never know whether the maginitude of jitter was appriopriate.
#Anyone with any statistical training should of course realize the problem with decision making based on a point estimate of a parameter (when we compute the p_stars, with any formula, we are creating a point estimate). What if we calculate and use standard error to improve our decision making? Well, if we did a sample from the sample statistic distribution (i.e. z distribution with mean p and standard deviation = standard error, or t distribution etc) then we are basically approximating the bayesian approach, but it is still inferior to the correct bayesian approach, since the bayesian approach uses the posterior distribution and we are not using that then by definition we are not correct. Examples of this can be seen in how, if we use t or z distribution then we will sample values less than 0 or greater than 1, which are not possible. Of course we can still find argmax and use argmax to make our decision, but this shows something is not really right.




#now lets repeat both examples 100 times, storing the action probability at each time period:
library(ggplot2)
library(tidyr)
library(dplyr)

greedy<-function(m, data_success_counts_greedy = c(0,0,0), data_failure_counts_greedy = c(0,0,0)){
  choices_greedy=rep(NA,m)
  for(iteration in 1:m){ #this is a pull of a slot machine
    for(i in 1:3){
      if(data_success_counts_greedy[i]+data_failure_counts_greedy[i]==0){
        p_stars[i] =.5+ runif(1)*.1
      }else{
        p_stars[i] = data_success_counts_greedy[i]/(data_success_counts_greedy[i]+data_failure_counts_greedy[i]) - runif(1)*.1 #this dithering is strictly negative and therefore at least 1 exploration of each
      } 
    }
    
    machine_to_pull = which.max(p_stars)
    choices_greedy[iteration]=machine_to_pull #store the choice just for diagnostics
    
    #now actually pull that machine
    result = rbinom(1,1,true_p_s[machine_to_pull])
    
    #update the data:
    data_success_counts_greedy[machine_to_pull] = data_success_counts_greedy[machine_to_pull] + result
    data_failure_counts_greedy[machine_to_pull] = data_failure_counts_greedy[machine_to_pull] + (1-result)
  }
  
  return(choices_greedy)
}

bayes<-function(m, data_success_counts = c(0,0,0), data_failure_counts = c(0,0,0), true_p_s = c(.6, .5, .7), prior_a_s = c(1, 1, 1),prior_b_s = c(1, 1, 1)){
  num_machines=length(data_success_counts)
  choices=rep(NA,m)
  for(iteration in 1:m){ #this is a pull of a slot machine
    p_stars = c(0,0,0) #this stores our parameter values sampled from the posterior for the round
    for(i in 1:num_machines){ #sample p's from the posterior
      p_stars[i] = rbeta(n=1,shape1=prior_a_s[i]+data_success_counts[i],shape2 = prior_b_s[i]+data_failure_counts[i]) #this is just the posterior  p_i|data_i ~ beta(a_i+#success_i)/(b_i+#failures_i)
    }
    machine_to_pull = which.max(p_stars) #the action we take is based on the "best" parameter sampled- in this case we want to win money, so we want to choose the machine with the maximum p. In another scenario we may choose the group with the lowest mu or sigma whatever, or, if our goal was to find the fairest machine we could actually choose the machine with the p closest to .5
    
    choices[iteration]=machine_to_pull #store the choice just for diagnostics
    
    #now actually pull that machine
    result = rbinom(1,1,true_p_s[machine_to_pull])
    
    #update the data:
    data_success_counts[machine_to_pull] = data_success_counts[machine_to_pull] + result
    data_failure_counts[machine_to_pull] = data_failure_counts[machine_to_pull] + (1-result)
  }
  
  data_success_counts
  data_failure_counts
  return(choices)
  
}

z_dist<-function(m, data_success_counts = c(1,1,1), data_failure_counts = c(1,1,1)){
  #it is really important to initialize with 1s. initializing with 0's is NOT good.
  #in fact, it turns out we don't even need agresti correction
  choices=rep(NA,m)
  for(iteration in 1:m){ #this is a pull of a slot machine
    p_stars = c(0,0,0) #this stores our parameter values sampled from the posterior for the round
    for(i in 1:3){ #sample p's from the posterior
      if(data_success_counts[i]+data_failure_counts[i]==0){
        p_stars[i] =.5+ runif(1)*.1
      }else{
        n=(data_success_counts[i]+data_failure_counts[i])
        phat = data_success_counts[i]/n
        p_stars[i] = rnorm(n=1, mean=phat, sd=sqrt(phat*(1-phat)/n))
      } 
    }
    machine_to_pull = which.max(p_stars) #the action we take is based on the "best" parameter sampled- in this case we want to win money, so we want to choose the machine with the maximum p. In another scenario we may choose the group with the lowest mu or sigma whatever, or, if our goal was to find the fairest machine we could actually choose the machine with the p closest to .5
    
    choices[iteration]=machine_to_pull #store the choice just for diagnostics
    
    #now actually pull that machine
    result = rbinom(1,1,true_p_s[machine_to_pull])
    
    #update the data:
    data_success_counts[machine_to_pull] = data_success_counts[machine_to_pull] + result
    data_failure_counts[machine_to_pull] = data_failure_counts[machine_to_pull] + (1-result)
  }
  
  data_success_counts
  data_failure_counts
  return(choices)
  
}

corrected_z_dist<-function(m, data_success_counts = c(1,1,1), data_failure_counts = c(1,1,1)){
  choices=rep(NA,m)
  for(iteration in 1:m){ #this is a pull of a slot machine
    p_stars = c(0,0,0) #this stores our parameter values sampled from the posterior for the round
    for(i in 1:3){ #sample p's from the posterior
      if(data_success_counts[i]+data_failure_counts[i]==0){
        p_stars[i] =.5+ runif(1)*.1
      }else{
        #agresti-coull correction:
        n=(data_success_counts[i]+data_failure_counts[i])
        phat = data_success_counts[i]/n
        z=(phat-.5)/sqrt(phat*(1-phat)/n)
        n=n+z^2
        phat=1/n * (data_success_counts[i] + (z^2)/2)
        p_stars[i] = rnorm(n=1, mean=phat, sd=sqrt(phat*(1-phat)/n))
      } 
    }
    machine_to_pull = which.max(p_stars) #the action we take is based on the "best" parameter sampled- in this case we want to win money, so we want to choose the machine with the maximum p. In another scenario we may choose the group with the lowest mu or sigma whatever, or, if our goal was to find the fairest machine we could actually choose the machine with the p closest to .5
    
    choices[iteration]=machine_to_pull #store the choice just for diagnostics
    
    #now actually pull that machine
    result = rbinom(1,1,true_p_s[machine_to_pull])
    
    #update the data:
    data_success_counts[machine_to_pull] = data_success_counts[machine_to_pull] + result
    data_failure_counts[machine_to_pull] = data_failure_counts[machine_to_pull] + (1-result)
  }
  
  data_success_counts
  data_failure_counts
  return(choices)
  
}

generate_results_df <- function(f, n=100, m=1000, data_success_counts = NA, data_failure_counts =NA, true_p_s = NA, prior_a_s = NA,prior_b_s =NA){
  
  args <- as.list(match.call())
  args$f <- NULL
  args$n <- NULL
  args[[1]] <- NULL # remove first list element, it's the function call
  if(!("m" %in% names(args))){
    args$m=m
  }
  
  results=matrix(NA,n,m)
  for(i in 1:n){
    results[i,]=do.call(f, args, envir = parent.frame())
  }
  results_df = data.frame("a"=rep(NA,m),"b"=rep(NA,m),"c"=rep(NA,m), "iteration"=1:m)
  for(i in 1:m){
    results_df[i,1]=sum(results[,i]==1)/n
    results_df[i,2]=sum(results[,i]==2)/n
    results_df[i,3]=sum(results[,i]==3)/n
  }
  return(results_df)
}

results_df_greedy <- generate_results_df(greedy, 200)
results_df_greedy %>% gather(.,machine,prop,c(a,b,c)) %>% 
  ggplot(., aes(x=iteration, y=prop, group=machine)) +
  geom_smooth(aes(color=machine))

results_df_bayes <- generate_results_df(bayes, 200)
results_df_bayes %>% gather(.,machine,prop,c(a,b,c)) %>% 
  ggplot(., aes(x=iteration, y=prop, group=machine)) +
  geom_smooth(aes(color=machine))

results_df_z_bad_prior <- generate_results_df(z_dist, 200, data_success_counts = c(0,0,0), data_failure_counts = c(0,0,0))
results_df_z_bad_prior %>% gather(.,machine,prop,c(a,b,c)) %>% 
  ggplot(., aes(x=iteration, y=prop, group=machine)) +
  geom_smooth(aes(color=machine))

results_df_z_good_prior <- generate_results_df(z_dist, 200, data_success_counts = c(1,1,1), data_failure_counts = c(1,1,1))
results_df_z_good_prior %>% gather(.,machine,prop,c(a,b,c)) %>% 
  ggplot(., aes(x=iteration, y=prop, group=machine)) +
  geom_smooth(aes(color=machine))

results_df_z_cor <- generate_results_df(corrected_z_dist, 200)
results_df_z_cor %>% gather(.,machine,prop,c(a,b,c)) %>% 
  ggplot(., aes(x=iteration, y=prop, group=machine)) +
  geom_smooth(aes(color=machine))

#cool! the z score approach is comparable to the bayesian apporach, although not quite as good, especially since you need to initialize it with a psuedo count of 1 and 1 to make it work properly. In fact, once you do this agresti correction doesn't even appear to matter much.

#the points to note are (1) any individual trial of the bayesian approach converges to exploiting the correct machine w.p. 1
#(2) collectively, the average of multiple bayesian approachs shows convergence.
#(3) single approach of greedy trial is not guaranteed to converge on correct machine, and in fact often converges on a bad machine. Dithering decreases probability of converging on a wrong machine, but also decreases the ability to ever "converge" on a correct machine.

#Also note that one nice thing about the above model is we can store the sum of the successes and failures up to a given point, so each update will take constant time.


#Now, for the final generalization, which is to better define the "best" parameter value. Clearly in the simple bernoulli example, where the rewards are constant, then the "best" parameter is the highest. However, suppose the rewards are fixed but differ between machines. Then the "best" choice is the one where reward_i * pi_i is highest (i.e. the one with the highest expected reward). More generally, if we define a reward distribution r_i(p_i), then given our sampled p_i we should choose i such that E(r_i|p_i) is maximized. 
#Example: Suppose there are different carriers for a lane, each with an ontime arrival probability p_i for that lane, and cost c_i, and there is a fixed penalty incurred by CHR, l, for the carrier being late.
#we want to minimize q_i = c_i+l*I_i, where I_i~Bern(p_i). This is equivalent to the previous problem, but now we will choose carrier i based on the minimum Expected cost q_i.
true_p_s = c(.1, .05, .3)
costs = c(1000,1090,990)
late_fee = 200
#We will use beta(1,1) ~ unif(0,1) priors to reflect our initial lack of knowledge. This is equivalent to 0 pseudo observations. However, I think it actually makes sense to use a Beta(1,2) prior, since truck companies GENERALLY are not late 90% of the time.
hist( rbeta(n=1000,shape1=1,shape2 = 2))

prior_a_s = c(1, 1, 1)
prior_b_s = c(1, 1, 1)

#now initialize data (with 0's obviously)
data_success_counts = c(0,0,0)
data_failure_counts = c(0,0,0)

choices=rep(NA,1000)
for(iteration in 1:1000){ #this is a pull of a slot machine
  p_stars = c(0,0,0) #this stores our parameter values sampled from the posterior for the round
  for(i in 1:3){ #sample p's from the posterior
    p_stars[i] = rbeta(n=1,shape1=prior_a_s[i]+data_success_counts[i],shape2 = prior_b_s[i]+data_failure_counts[i]) #this is just the posterior  p_i|data_i ~ beta(a_i+#success_i)/(b_i+#failures_i)
  }
  carrier_to_choose = which.min(costs + p_stars*late_fee) #the carrier we choose is the one with lowest expected cost, given the parameters we drew.
  
  choices[iteration]=carrier_to_choose #store the choice just for diagnostics
  
  #now actually pull that machine
  result = rbinom(1,1,true_p_s[carrier_to_choose])
  
  #update the data:
  data_success_counts[carrier_to_choose] = data_success_counts[carrier_to_choose] + result
  data_failure_counts[carrier_to_choose] = data_failure_counts[carrier_to_choose] + (1-result)
}

data_success_counts
data_failure_counts
plot(choices)
#Notice that we don't explore carrier 2, since the only scenerio where it is optimal is if carrier 3 is late ~100% of the time, and carrier 2 is late ~0%. Since carrier 1 and 3 are in fact late .1 and .3 of the time then this scenario is quickly (correctly) given almost no likelihood.
#This highlights the benefit of the Bayesian approach - INTELLIGENT exploration of the action space. Carrier 2 not the optimal action, and after collecting even just a little data on carriers 1 and 3 we are very certain that it is not possible for 2 to be optimal, therefore we do not waste any time exploring it.




#Let's do another example to illustrate how valuable an emprical rather than a flat prior can be.
# Specifically, a flat prior can result in a much longer "learning" period.
#we can illustrate this best with a real life example of web ads. The click through rate of a real web ad is 1% at best. Therefore our prior should reflect this:
plot(density( rbeta(n=1000,shape1=1,shape2 = 100)))

true_p_s = c(.02,.01,.005)
# true_p_s = c(.01,.001,.005)
# true_p_s = c(.02,.001,.005)
results_df_bayes_good_prior <- generate_results_df(bayes, 100, m=3000, true_p_s = true_p_s, prior_a_s = c(1,1,1),prior_b_s =c(100,100,100))
results_df_bayes_good_prior %>% gather(.,machine,prop,c(a,b,c)) %>% 
  ggplot(., aes(x=iteration, y=prop, group=machine)) +
  geom_smooth(aes(color=machine))

results_df_bayes_flat_prior <- generate_results_df(bayes, 100, m=3000, true_p_s = true_p_s, prior_a_s = c(1,1,1),prior_b_s =c(1,1,1))
results_df_bayes_flat_prior %>% gather(.,machine,prop,c(a,b,c)) %>% 
  ggplot(., aes(x=iteration, y=prop, group=machine)) +
  geom_smooth(aes(color=machine))


df_1 <- results_df_bayes_flat_prior %>% gather(.,machine,prop,c(a,b,c)) %>% mutate(machine = recode(machine, a="a_flat", b="b_flat", c="c_flat"))
df_2 <- results_df_bayes_good_prior %>% gather(.,machine,prop,c(a,b,c)) %>% mutate(machine = recode(machine, a="a_good", b="b_good", c="c_good"))
rbind(df_1,df_2) %>% ggplot(., aes(x=iteration, y=prop, group=machine)) +
  geom_smooth(aes(color=machine))

df = rbind(df_1,df_2)
df[df$iteration==1000,]

#notice that this because the click through rates are so low we need many iterations before we can segretate them- each play, no matter what we choose, gives very little info. However, a better prior absolutely helps us learn faster and get more clickthroughs, even if the prior is not quite right it is much more correct than the flat prior. Literally the only way a flat prior would be better is if all ads had click through rates >50%, in which case any extra time we would need to distinguish which one was "better" would not be a problem, because in this hypothetical scenario we have multiple AMAZINGLY good ads.


















#As a final example we consider a routing problem.
#Suppose that we have three possible truck routes, A, B, and C. The time it takes the truck to traverse each route is t_i ~ N(mu_i,sigma_i). Additionally, there are different fuel and tariff costs associated with each route, which are c_1,c_2,c_3. We want to maximize q_i = (-t_i+c_i).

#In this example Route C has the lowest expected cost, but of course we do not know this.
true_mus=c(200,50,150)
true_taus=c(1/30,1/10,1/40) #precision is 1/variance
costs = c(100,200,90)


prior_mu_s = c(100, 100, 100)
prior_tau_s=c(1/30,1/10,1/40) # i don't want to deal with a normal-gamma prior, so for simplicity we assume taus are known.
m=1000
results=matrix(NA, m, 3)
choices=rep(NA,m)
for(iteration in 1:m){ #this is a pull of a slot machine
  mu_stars = c(0,0,0) #this stores our parameter values sampled from the posterior for the round
  for(i in 1:3){ #sample mu's from the posterior
    numerator = prior_mu_s[i]*prior_tau_s[i] + ifelse(all(is.na(results[,i])), 0, true_taus[i]*(sum(results[,i], na.rm = TRUE)))
    denomenator = prior_tau_s[i] + ifelse(all(is.na(results[,i])), 0, sum(!is.na(results[,i])))             mu_stars[i] = rnorm(1, mean = numerator/denomenator, sd = sqrt(1/denomenator))
  }
  machine_to_pull = which.max(p_stars) #the action we take is based on the "best" parameter sampled- in this case we want to win money, so we want to choose the machine with the maximum p. In another scenario we may choose the group with the lowest mu or sigma whatever, or, if our goal was to find the fairest machine we could actually choose the machine with the p closest to .5
    
    choices[iteration]=machine_to_pull #store the choice just for diagnostics
    
    #now actually pull that machine
    result = rbinom(1,1,true_p_s[machine_to_pull])
    
    #update the data:
    data_success_counts[machine_to_pull] = data_success_counts[machine_to_pull] + result
    data_failure_counts[machine_to_pull] = data_failure_counts[machine_to_pull] + (1-result)
  }
  
  data_success_counts
  data_failure_counts
  choices
  



