# *******************************
# BROWNIAN MOTION SIMULATION
# December 2012 | Benjamin Tovar
# *******************************
#
#   REFERENCES
#   http://landshape.org/enm/r-code-for-brownian-motion/
#
#   According to Wikipedia the mathematical model for Brownian motion 
#   (also known as random walks) can also be used to describe many 
#   phenomena as well as the random movements of minute particles, 
#   such as stock market fluctuations and the evolution of physical 
#   characteristics in the fossil record. The simple form of the 
#   mathematical model for Brownian motion has the form:
#
#    S_t = eS_t-1
#
#    where e is drawn from a probability distribution.
#
#######################################################################

brownian <- function(n.times){
    x <- y <- x.new <- y.new <- x.new.p <- y.new.p <- vector()
    for(i in 1:n.times){
        # Initialize variables
        x <- rnorm(1)
        y <- rnorm(1)
        # concatenate variables 
        # to increase the vector size
        x.new <- c(x.new,x)
        y.new <- c(y.new,y)
        # sum the vector numbers
        x.new.p <- cumsum(x.new)
        y.new.p <- cumsum(y.new)  
        # plot the model
        plot(x.new.p,y.new.p,type="b",
             main=paste("Brownian motion simulation in R\nTime =",i,sep=" "),
             xlab="x coordinates",ylab="y coordinates",
             col=c(rep("gray",i-1),"red"),
             pch=c(rep(20,i-1),1))    
    }
}

# Test the function
# brownian(500)
