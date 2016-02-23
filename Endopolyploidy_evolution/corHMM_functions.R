# Functions to test the evolutionary rates of endopolyploidy in leaves as true or false states

state_threshold = function(state, threshold){
  state_temp = numeric(length(state))
  state_temp[state < threshold] = 1
  state_temp[state >= threshold] = 2
  if(length(state[state >= threshold])/length(state) >= threshold
     || 
     length(state[state >= threshold])/length(state) <= 1-threshold)
    print("The proportion of species in each state is appropriate.")
  else
    print("Warning: the proportion of species in each state is NOT appropriate.")
  state_temp = as.factor(state_temp)
  return(state_temp)
}

transition_model = function(comparative.data, model_type, root_nonendo = FALSE){
  if(root_nonendo == TRUE)
    root = c(1,0,0) # This means that the model assumes that the root had state 1.
  else
    root = NULL # This means that the model assume that either root state is equally probable.
  
  rayDISC(phy = comparative.data$phy,
          data = comparative.data$data,
          model = model_type,
          node.state = "marginal",
          root.p = root)
}

Aikake_weight = function(model1, model2){
  delta_AIC = sqrt( (model1$AIC - model2$AIC)^2 )
  numerator = exp(-delta_AIC)
  denominator = exp(-delta_AIC) + 1
  if(model1$AIC < model2$AIC)
    return( c(1/denominator, numerator/denominator) )
  else
    return( c(numerator/denominator, 1/denominator) )
}