## Sample size calculator for partially clustered trials
## Kylie Lange (kylie.lange@adelaide.edu.au)

## Helper functions

# convert proportions of clusters to proportions of observations
delta_to_gamma = function(delta) {
  kdelta = delta %>% imap(~ .y*.x)  # k*delta_k
  gamma  = lapply(kdelta, function(k) k/sum(unlist(kdelta)))
  return(gamma)
}

# convert proportions of observations to proportions of clusters
gamma_to_delta = function(gamma) {
  kgamma = gamma %>% imap(~ .x/.y)  # gamma_k/k
  delta  = lapply(kgamma, function(k) k/sum(unlist(kgamma)))
  return(delta)
}

## DEFF functions

# Inputs: randomisation method, rho, list of proportions gamma_k, link function (binary), prevalence estimates (binary)
# Output: design effect

# Independence GEE for a continuous outcome
deff_cont_indgee = function(rand_method, rho, gamma) {
  if (rand_method == "Cluster") {
    sum1 = gamma %>% imap(~ .y*.x)  # k*gamma_k
    deff = 1 + rho*(sum(unlist(sum1)) - 1)
  }
  else if (rand_method == "Individual") {
    deff = 1
  }
  return(deff)
}

# Exchangeable GEE for a continuous outcome
deff_cont_exchgee = function(rand_method, rho, gamma) {
  if (rand_method == "Cluster") {
    sum1 = gamma %>% imap(~ (1/(1 + (.y-1)*rho))*.x)  # 1/(1+(k-1)rho)*gamma_k
    deff = 1 / sum(unlist(sum1))
  }
  else if (rand_method == "Individual") {
    sum1 = gamma %>% imap(~ (1 + (.y-2)*rho)/((1 - rho)*(1 + (.y-1)*rho))*.x)  # (1+(k-2)rho)/((1-rho)*(1+(k-1)rho))*gamma_k
    deff = 1 / sum(unlist(sum1))
  }
  return(deff)
}

# Independence GEE with a logit link for a binary outcome 
deff_logit_indgee = function(rand_method, rho, gamma, pi_C, pi_I) {
  if (rand_method == "Cluster") {
    sum1 = gamma %>% imap(~ (.y-1)*.x)   # (k-1)*gamma_k
    deff = 1 + rho*(sum(unlist(sum1)))
  }
  else if (rand_method == "Individual") {
    sum1 = gamma %>% imap(~ (.y-1)*.x)   # (k-1)*gamma_k
    deff = 1 + rho*((0.5 - sqrt(pi_I*pi_C*(1-pi_I)*(1-pi_C))/(pi_I*(1-pi_I)+pi_C*(1-pi_C)))*sum(unlist(sum1)))
  }
  return(deff)
}

# Exchangeable GEE with a logit link for a binary outcome 
deff_logit_exchgee = function(rand_method, rho, gamma, pi_C, pi_I) {
  if (rand_method == "Cluster") {
    sum1 = gamma %>% imap(~ 1/(1 + (.y-1)*rho)*.x)   # 1/(1+(k-1)rho)*gamma_k
    deff = 1 / sum(unlist(sum1))
  }
  else if (rand_method == "Individual") {
    sum1 = gamma %>% imap(~ 1/(1 + (.y-1)*rho)*.x)       # 1/(1+(k-1)rho)*gamma_k
    sum2 = gamma %>% imap(~ (.y-1)/(1 + (.y-1)*rho)*.x)  # (k-1)/(1+(k-1)rho)*gamma_k
    deff = (sum(unlist(sum1)) + (0.5 - sqrt(pi_I*pi_C*(1-pi_I)*(1-pi_C))/(pi_I*(1-pi_I)+pi_C*(1-pi_C)))*rho/(1-rho)*sum(unlist(sum2))) / 
      (sum(unlist(sum1))**2 + rho/(1-rho)*sum(unlist(sum1))*sum(unlist(sum2)))
  }
  return(deff)
}

# Independence GEE with a log link for a binary outcome 
deff_log_indgee = function(rand_method, rho, gamma, pi_C, pi_I) {
  if (rand_method == "Cluster") {
    sum1 = gamma %>% imap(~ (.y-1)*.x)   # (k-1)*gamma_k
    deff = 1 + rho*(sum(unlist(sum1)))
  }
  else if (rand_method == "Individual") {
    sum1 = gamma %>% imap(~ (.y-1)*.x)   # (k-1)*gamma_k
    deff = 1 + rho*((0.5 - sqrt(pi_I*pi_C*(1-pi_I)*(1-pi_C))/(pi_I*(1-pi_C)+pi_C*(1-pi_I)))*sum(unlist(sum1)))
  }
  return(deff)
}

# Exchangeable GEE with a log link for a binary outcome 
deff_log_exchgee = function(rand_method, rho, gamma, pi_C, pi_I) {
  if (rand_method == "Cluster") {
    sum1 = gamma %>% imap(~ 1/(1 + (.y-1)*rho)*.x)   # 1/(1+(k-1)rho)*gamma_k
    deff = 1 / sum(unlist(sum1))
  }
  else if (rand_method == "Individual") {
    sum1 = gamma %>% imap(~ 1/(1 + (.y-1)*rho)*.x)       # 1/(1+(k-1))*gamma_k
    sum2 = gamma %>% imap(~ (.y-1)/(1 + (.y-1)*rho)*.x)  # (k-1)/(1+(k-1)rho)*gamma_k
    deff = (sum(unlist(sum1)) + (0.5 - sqrt(pi_I*pi_C*(1-pi_I)*(1-pi_C))/(pi_I*(1-pi_C)+pi_C*(1-pi_I)))*rho/(1-rho)*sum(unlist(sum2))) / 
      (sum(unlist(sum1))**2 + rho/(1-rho)*sum(unlist(sum1))*sum(unlist(sum2)))
  }
  return(deff)
}

# Call the appropriate deff function depending on trial characteristics
calc_deff = function(outcome, corr, rand, ICC, link, gamma, pi_I, pi_C) {
  if (outcome == "Continuous") {
    if (corr == "Independence") {
      deff = deff_cont_indgee(rand_method = rand, rho = ICC, gamma = gamma)
    }
    else if (corr == "Exchangeable") {
      deff = deff_cont_exchgee(rand_method = rand, rho = ICC, gamma = gamma)
    }
  }
  else if ((outcome == "Binary") & (link == "Logistic")) {
    if (corr == "Independence") {
      deff = deff_logit_indgee(rand_method = rand, rho = ICC, gamma = gamma, pi_C = pi_C, pi_I = pi_I)
    }
    else if (corr == "Exchangeable") {
      deff = deff_logit_exchgee(rand_method = rand, rho = ICC, gamma = gamma, pi_C = pi_C, pi_I = pi_I)
    }
  }
  else if ((outcome == "Binary") & (link == "Log Binomial")) {
    if (corr == "Independence") {
      deff = deff_log_indgee(rand_method = rand, rho = ICC, gamma = gamma, pi_C = pi_C, pi_I = pi_I)
    }
    else if (corr == "Exchangeable") {
      deff = deff_log_exchgee(rand_method = rand, rho = ICC, gamma = gamma, pi_C = pi_C, pi_I = pi_I)
    }
  }
  return(deff)
}
