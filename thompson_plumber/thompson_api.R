# hist( rbeta(n=1000,shape1=1,shape2 = 100))

prior_a_s = c(1, 1, 1)
prior_b_s = c(100, 100, 100)

#now initialize data (with 0's obviously)
data_success_counts = c(0,0,0) # should be stored in redis
data_failure_counts = c(0,0,0) # should be stored in redis

n = length(prior_a_s)

#* Get action choice, given current data
#* @get /action
function(){
  p_stars = rep(0, n) #this stores our parameter values sampled from the posterior for the round
  for(i in 1:n){ #sample p's from the posterior
    p_stars[i] = rbeta(n=1,shape1=prior_a_s[i]+data_success_counts[i],shape2 = prior_b_s[i]+data_failure_counts[i]) #this is just the posterior  p_i|data_i ~ beta(a_i+#success_i)/(b_i+#failures_i)
  }
  action = which.max(p_stars) #the action we take is based on the "best" parameter sampled- in this case we want to win money, so we want to choose the machine with the maximum p. In another scenario we may choose the group with the lowest mu or sigma whatever, or, if our goal was to find the fairest machine we could actually choose the machine with the p closest to .5

  return(action)  
}

#* Update data
#* @param action The action taken (index)
#* @param result The success or failure of the action (1 for success, 0 for failure)
#* @get /update-data/<action:int>/<result:int>
function(action, result){
  if(!(action >=1 && action<= n)){
    msg <- paste0("Invalid action value, was not between 1 and ", length(prior_a_s))
    result$status <- 400 # Bad request
    list(error=jsonlite::unbox(msg))
  }
  
  if(!(result %in% c(0,1))){
    msg <- "Invalid result value, was not 0 or 1."
    result$status <- 400 # Bad request
    list(error=jsonlite::unbox(msg))
  }
  
  if(result==1){
    data_success_counts[action] <<- data_success_counts[action]+1
  } else{
    data_failure_counts[action] <<- data_failure_counts[action]+1
  }
}


