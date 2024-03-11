loadTraces <- function(run_path) {
  r <- lapply(dir(run_path, full.names = T), function(x){
    y <- readRDS(x)
    return(y$trace)
  })

  return(r)
}


plotTraces <- function(traces){
  
  r1 <- traces[[1]]
  r2 <- traces[[2]]
  r3 <- traces[[3]]
  
  theta_names <- names(r1)[-length(names(r1))]
  
  col_alpha <- 0.5
  par(mfrow = c(3, 2), cex=1.0, mar=c(3, 2, 3, 2))
  
  for(var in theta_names){
    ymin <- 0.95*min(r1[[var]],r2[[var]],r3[[var]])
    ymax <- 1.05*max(r1[[var]],r2[[var]],r3[[var]])
    
    plot(r3[[var]], type="l", col=alpha("chartreuse4", col_alpha), ylab="", xlab="", ylim=c(ymin,ymax))
    par(new=TRUE)
    plot(r1[[var]], type="l", col=alpha("red", col_alpha), ylab="", xlab="", ylim=c(ymin,ymax))
    par(new=TRUE)
    plot(r2[[var]], type="l", col=alpha("blue", col_alpha), ylab="", xlab="", ylim=c(ymin,ymax), main = var)
    
    d1 <- density(r1[[var]])
    d2 <- density(r2[[var]])
    d3 <- density(r3[[var]])
    
    xmin <- 0.95*min(d1$x, d2$x, d3$x)
    xmax <- 1.05*max(d1$x, d2$x, d3$x)
    ymin <- 0.95*min(d1$y, d2$y, d3$y)
    ymax <- 1.05*max(d1$y, d2$y, d3$y)
    
    plot(d3$x, d3$y, type="l", col=alpha("chartreuse4", col_alpha), yaxt="n", ylab="", xlab="", ylim=c(ymin,ymax), xlim=c(xmin,xmax), lwd=3)
    par(new=TRUE)
    plot(d1$x, d1$y, type="l", col=alpha("red", col_alpha), yaxt="n", ylab="", xlab="", ylim=c(ymin,ymax), xlim=c(xmin,xmax), lwd=3)
    par(new=TRUE)
    plot(d2$x, d2$y, type="l", col=alpha("blue", col_alpha), yaxt="n", ylab="", xlab="", ylim=c(ymin,ymax), xlim=c(xmin,xmax), lwd=3, main="")  
  }
}