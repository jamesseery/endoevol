dist.check = function(variable){
    op = par(mfrow = c(2,2)) # Display 2x2 plots

    hist(variable, breaks=12)
    qqnorm(variable)
    qqline(variable)
    hist(log(variable), breaks=12)
    qqnorm(log(variable))
    qqline(log(variable))

    par(op) # Reset plot display}
