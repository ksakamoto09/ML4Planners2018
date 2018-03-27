# by petrkeil
# https://www.r-bloggers.com/fast-conways-game-of-life-in-r/
# library that allows the animated .gif export
library(caTools)

# The game.of.life() function ------------------
# Arguments:
# side - side of the game of life arena (matrix)
# steps - number of animation steps
# filename - name of the animated gif file

game.of.life <- function(side, steps, filename){
    
    # the sideXside matrix, filled up with binomially
    # distributed individuals
    X <- matrix(nrow=side, ncol=side)
    X[] <- rbinom(side^2,1,0.4)
    
    # array that stores all of the simulation steps
    # (so that it can be exported as a gif)
    storage <- array(0, c(side, side, steps))
    
    # the simulation                                             
    for (i in 1:steps)
    {
        # make the shifted copies of the original array
        allW = cbind( rep(0,side) , X[,-side] )
        allNW = rbind(rep(0,side),cbind(rep(0,side-1),X[-side,-side]))
        allN = rbind(rep(0,side),X[-side,])
        allNE = rbind(rep(0,side),cbind(X[-side,-1],rep(0,side-1)))
        allE = cbind(X[,-1],rep(0,side))
        allSE = rbind(cbind(X[-1,-1],rep(0,side-1)),rep(0,side))
        allS = rbind(X[-1,],rep(0,side))
        allSW = rbind(cbind(rep(0,side-1),X[-1,-side]),rep(0,side))
        
        # summation of the matrices
        X2 <- allW + allNW + allN + allNE + allE + allSE + allS + allSW
        
        # the rules of GoL are applied using logical subscripting
        X3 <- X
        X3[X==0 & X2==3] <- 1
        X3[X==1 & X2<2] <- 0
        X3[X==1 & X2>3] <- 0
        X <- X3
        
        # each simulation step is stored
        storage[,,i] <- X2
        # note that I am storing the array of Ni values -
        # - this is in order to make the animation prettier
    }
    
    storage <- storage/max(storage) # scaling the results
    # to a 0-1 scale
    
    # writing the results into an animated gif
    write.gif(storage, filename, col="gray", delay=5)
}

game.of.life(side=100, steps=300, file="conway.gif")
