# 正規化ジニ係数: Normalized Gini Coefficient 

normalizedGini <- function ( data, lev = NULL, model = NULL){
  Gini <- function(a, p) {
    if (length(a) !=  length(p)) stop("Actual and Predicted need to be equal lengths!")
    temp.df <- data.frame(actual = a, pred = p, range=c(1:length(a)))
    temp.df <- temp.df[order(-temp.df$pred, temp.df$range),]
    population.delta <- 1 / length(a)
    total.losses <- sum(a)
    null.losses <- rep(population.delta, length(a)) # Hopefully is similar to accumulatedPopulationPercentageSum
    accum.losses <- temp.df$actual / total.losses # Hopefully is similar to accumulatedLossPercentageSum
    gini.sum <- cumsum(accum.losses - null.losses) # Not sure if this is having the same effect or not
    sum(gini.sum) / length(a)
  }
  
  result <- Gini(as.numeric(data$obs), as.numeric(data$pred)) / Gini(as.numeric(data$obs), as.numeric(data$obs))
  
  return(c(`Gini` = result)) 
}
