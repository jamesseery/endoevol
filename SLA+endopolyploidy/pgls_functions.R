dist.check = function(variable){
    op = par(mfrow = c(2,2)) # Display 2x2 plots

    hist(variable, breaks=12)
    qqnorm(variable)
    qqline(variable)
    hist(log(variable), breaks=12)
    qqnorm(log(variable))
    qqline(log(variable))

    par(op) # Reset plot display
}

line.maker = function(pgls_model){
    abline(pgls_model$model$coef[1], pgls_model$model$coef[3], lty=2)
    abline(pgls_model$model$coef[1] + pgls_model$model$coef[2], pgls_model$model$coef[3])
}
