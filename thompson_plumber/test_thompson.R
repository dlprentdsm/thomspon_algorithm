library(httr)
library(dplyr)
true_p_s=c(.01,.02,.005)

choice=rep(NA, 1000)
for(i in 1:1000){
  choice[i] = httr::GET(url = "http://0.0.0.0:41333/action") %>% content(.) %>% .[[1]]
  # now actually pull that machine
  result = rbinom(1,1,true_p_s[choice[i]])
  httr::GET(url = paste0("http://0.0.0.0:41333/update-data/", choice[i] , "/", result )) %>% content(.)
}

plot(choice)


