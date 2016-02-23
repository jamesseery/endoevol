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

EI.plots = function(dataset){
    mar.default <- c(5,4,4,2) + 0.1
    op=par(mar = mar.default + c(0, 1, 0, 0), mfrow=c(2,1))
    plot(log(SLA) ~ lnEI, data=dataset, xlab="", xaxt='n', 
         ylab=expression("Ln specific leaf area " ~ (m^{2} ~ g^{-1})), main = "")
    axis(side=1, labels=FALSE)
    plot(log(RWC) ~ lnEI, data=dataset, 
         ylab=expression("Ln relative water content " ~ (g ~ g^{-1})), xlab = "Ln endoreduplication index")
    par(op)
}