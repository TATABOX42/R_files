#       parameter_optimization.R
#       
#       Copyright 2013 Benjamin Tovar Cisneros <benjamin@dreamSequencer>
#       
#       This program is free software; you can redistribute it and/or modify
#       it under the terms of the GNU General Public License as published by
#       the Free Software Foundation; either version 2 of the License, or
#       (at your option) any later version.
#       
#       This program is distributed in the hope that it will be useful,
#       but WITHOUT ANY WARRANTY; without even the implied warranty of
#       MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#       GNU General Public License for more details.
#       
#       You should have received a copy of the GNU General Public License
#       along with this program; if not, write to the Free Software
#       Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
#       MA 02110-1301, USA.
#
################################################################################

# Author: Benjamin Tovar 
# Date: October 19 2013

######################################
# RUNNING EXAMPLE
######################################

# # SET THE TRAINING SET PARAMETERS

# x <- c(2,4,6,8,10,12,14,16,18,20)
# y <- c(26,-1,4,20,0,-2,19,1,-4,19)

# # ## plot the training set
# plot(x,y,type="b",col=4,main="Training set",
# 	xlab="x value",ylab="y value")
# grid()

# # ***********************************
# # SOLVE THETA USING THE RANDOM METHOD
# # ***********************************

# random.method(x,y,10000,7)

# # ***********************************
# # SOLVE THETA USING THE GENETIC ALGORITHM
# # ***********************************

# genetic.algorithm(x,y,500,100,0.2,7,NA)

# # ***********************************
# # SOLVE THETA USING THE SIMULATED ANNEALING METHOD
# # ***********************************

# simulated.annealing(x,y,360,1,7,NA)

##########################################################################
##########################################################################
# 						FUNCTIONS
##########################################################################
##########################################################################


######################################
# FUNCTION TO ADJUST THE PARAMETERS THETA
######################################

f <- function(x,theta){
	# theta[1] = a0
	# theta[2] = a1
	# theta[3] = a2
	# theta[4] = a3
	predicted.y <- (theta[1]/x^2) + (theta[2]*exp(theta[3]/x)) + (theta[4]*sin(x))
	return(predicted.y)
}

######################################
# ERROR FUNCTION
######################################

error <- function(x,y,theta){
	f.temp <- error <- 0
	f.temp <- f(x,theta)
	error <- sum(abs(y-f.temp))
	return(error)
}

######################################
# FUNCTION TO PLOT THE SOLUTION
######################################

plot_solution <- function(x,y,predicted.y,e,n.iterations=1){
	plot(x,y,type="b",col=1,
		main=paste("Iteration #:",n.iterations,"| y and predicted y plot | error =",signif(e,4),sep=""),
		xlab="x value",ylab="y value",
		ylim=c(-30,80))
		lines(x,predicted.y,col=2,pch=2,type="b")
		legend("topright",legend=c("y","predicted y"),lty=1,col=1:2)
		grid()
}

######################################
# RANDOM METHOD ALGORITHM
######################################

random.method <- function(x,y,n.iterations,min.error){
	# THIS METHOD PRODUCES i COMBINATIONS OF THETA EACH ITERATION
	# AND TRIES BY TRIAL AND ERROR TO SATISFY THE GOAL
	# THIS METHOD DOES NOT HAVE "MEMORY", IT ONLY TEST
	# RANDOM THETA PARAMETERS EACH TIME
	for(i in 1:n.iterations){
		# Initiate the 4 parameters of theta
		theta <- sample(0:15,4,rep=TRUE)
		# obtain the values of predicted.y
		predicted.y <- f(x,theta)
		# compute the error
		e <- error(x,y,theta)
		cat("Iteration =",i,"| error =",e,"\n")
		#plot_solution(x,y,predicted.y,e,i)	
		if(e <= min.error){
			cat("GOOD! SOLUTION FOUND WITH ERROR =",e,"\n")
			plot_solution(x,y,predicted.y,e,i)	
			return(theta)
		}
	}
	cat("*** SOLUTION NOT FOUND WITH ERROR =",e,"\n")
	return(theta)
}

############################################
# GENETIC ALGORITHM
############################################

crossover <- function(parent.1,parent.2){
	# get the size of any of the parents
	l <- length(parent.1)
	# set the split index, any random int number
	# among 1 and l-1 positions of the parent chromosome
	split.index <- sample(1:(l-1),1)
	# produce the new chromosome
	new.chromosome <- c(parent.1[1:split.index],parent.2[(split.index+1):l])
	return(new.chromosome)
}

mutation <- function(parent,p.mutation){
	# set a random probability value among 0 and 1
	# the value r is <= than p.mutation, mutate that position
	# of the original parent
	new.chromosome <- numeric(4)
	for(i in 1:length(parent)){
		r <- 0
		r <- runif(1)
		if(r <= p.mutation){
			new.chromosome[i] <- sample(0:15,1)
		} else{
			new.chromosome[i] <- parent[i]
		}
	}
	return(new.chromosome)
}

genetic.algorithm.aux <- function(n.population,theta.population,rank.best.sols.index,p.mutation){
	# pick the top 25% of the population to create a new population using
	# mutation and crossover
	split.size <- ceiling(n.population*0.25)
	# extract the best.solutions
	best.solutions <- theta.population[rank.best.sols.index[1:split.size],]
	# delete the population to save memory
	rm(theta.population)
	# mix the best.solutions using mutation and crossover in order
	# to create the other 25% of the new population
	# create a random number among 0 and 1 to establish the genetic operator
	mix.population <- matrix(0,nr=split.size,nc=4)
	for(i in 1:split.size){
		r <- 0
		r <- runif(1)
		if(r <= 0.5){
			c.index <- sample(1:split.size,2,rep=FALSE)
			mix.population[i,] <- crossover(best.solutions[c.index[1],],best.solutions[c.index[2],])
		} else{
			c.index <- sample(1:split.size,1)
			mix.population[i,] <- mutation(best.solutions[c.index,],p.mutation)			
		}
	}
	# concatenate both populations
	new.best.population <- rbind(best.solutions,mix.population)
	return(new.best.population)
}

genetic.algorithm <- function(x,y,n.population=50,n.generations=50,p.mutation=0.2,min.error=8,init.pop=NA){
	# IN A GENETIC ALGORITHM, A POPULATION OF CANDIDATE SOLUTIONS 
	# (CALLED INDIVIDUALS, CREATURES, OR PHENOTYPES) TO AN OPTIMIZATION 
	# PROBLEM IS EVOLVED TOWARD BETTER SOLUTIONS. 
	# EACH CANDIDATE SOLUTION HAS A SET OF PROPERTIES 
	# (ITS CHROMOSOMES OR GENOTYPE) WHICH CAN BE MUTATED AND ALTERED.
	# more information here <- http://en.wikipedia.org/wiki/Genetic_algorithm
	#
	# BASAL CASE: when n.generations = 0, print the best solution a time moment
	if(n.generations == 0){
		theta <- init.pop[1,]
		predicted.y <- f(x,theta)
		e <- error(x,y,init.pop[1,])
		cat("*** SOLUTION NOT FOUND WITH ERROR =",e,"\n")
		plot_solution(x,y,predicted.y,e,n.generations)
		return(theta)	
	} else{
	# GENERAL CASE: when n.generations != 0, the recursion can continue 	
		# check if the population is empty, this means that the population will 
		# be created 
		if(is.na(init.pop[1])){
			theta.population <- matrix(nr=n.population,nc=4)
			theta.population <- t(apply(theta.population,1,function(z) sample(0:15,4,rep=TRUE)))
		} else{
			# this case is when you have population from previous iterations
			theta.population.temp <- matrix(nr=(n.population - (ceiling(n.population*0.25)*2) ) ,nc=4)
			theta.population.temp <- t(apply(theta.population.temp,1,function(z) sample(0:15,4,rep=TRUE)))
			theta.population <- rbind(init.pop,theta.population.temp)
		}
		#predicted.y <- t(apply(theta.population,1,function(z) f(x,z)))
		e <- apply(theta.population,1, function(z) error(x,y,z))
		# sort the solutions
		rank.best.sols.index <- order(e)
		# check if the solution is among the population
		if(e[rank.best.sols.index[1]]<=min.error){
			theta <- theta.population[rank.best.sols.index[1],]
			e <- e[rank.best.sols.index[1]]
			predicted.y <- f(x,theta)
			cat("GOOD! SOLUTION FOUND WITH ERROR =",e,"\n")
			plot_solution(x,y,predicted.y,e,n.generations)	
			return(theta)		
		} else {
			# if solution is not among the population, then proceed with the algorithm
			# pick the top 25% of the population to create a new population using
			# mutation and crossover
			new.best.population <- genetic.algorithm.aux(n.population,theta.population,rank.best.sols.index,p.mutation)
			# finally look for recursion
			cat("iteration",n.generations,"| best solution error",e[rank.best.sols.index[1]],"\n")
			genetic.algorithm(x,y,n.population=50,(n.generations-1),p.mutation=0.2,min.error,new.best.population)
		}
	}
}

############################################
# SIMULATED ANNEALING
############################################

create.close.neighbors <- function(theta){
	theta.neighbors <- matrix(nr=4,nc=4)
	for(i in 1:4){
		theta.neighbors[i,] <- theta
		theta.neighbors[i,i] <- sample(0:15,1)
	}
	return(theta.neighbors)
}

create.random.neighbors <- function(theta){
	theta.neighbors <- matrix(nr=4,nc=4)
	for(i in 1:4){
		theta.neighbors[i,] <- sample(0:15,4,rep=TRUE)
	}
	return(theta.neighbors)
}

create.neighbors <- function(theta){
	if(runif(1)<=0.5){
		neighbors <- create.close.neighbors(theta)
	} else{
		neighbors <- create.random.neighbors(theta)
	}
	return(neighbors)
}

simulated.annealing.aux <- function(x,y,e,energy,temperature,min.error,theta){
	# Create neighbors
	neighbors <- create.neighbors(theta)
	# pick a random neighbor 
	random.neighbor <- neighbors[sample(1:4,1),]
	# evaluate that random neighbor
	e.neighbor <- error(x,y,random.neighbor)
	# Check if the solution is better than a random neighbor
	if(e.neighbor <= e){
		theta <- random.neighbor
		# enter the recursion
		simulated.annealing(x,y,(energy-temperature),temperature,min.error,theta)
	} else {
		# if the new solution is not better than the original theta
		# replace theta with probability p
		p <- exp((e-e.neighbor)/(energy-temperature))
		if(runif(1) <= p){
			theta <- random.neighbor
			# enter the recursion
			simulated.annealing(x,y,(energy-temperature),temperature,min.error,theta)
		} else{
			# enter the recursion
			simulated.annealing(x,y,(energy-temperature),temperature,min.error,theta)
		}
	}
}

simulated.annealing <- function(x,y,energy=100,temperature=1,min.error=8,init.theta=NA){
	# SIMULATED ANNEALING (SA) IS A GENERIC PROBABILISTIC METAHEURISTIC 
	# FOR THE GLOBAL OPTIMIZATION PROBLEM OF LOCATING A GOOD 
	# APPROXIMATION TO THE GLOBAL OPTIMUM OF A GIVEN FUNCTION IN A LARGE SEARCH SPACE. 
	# IT IS OFTEN USED WHEN THE SEARCH SPACE IS DISCRETE 
	# (E.G., ALL TOURS THAT VISIT A GIVEN SET OF CITIES). 
	# FOR CERTAIN PROBLEMS, SIMULATED ANNEALING MAY BE MORE EFFICIENT 
	# THAN EXHAUSTIVE ENUMERATION — PROVIDED THAT THE GOAL IS MERELY TO FIND 
	# AN ACCEPTABLY GOOD SOLUTION IN A FIXED AMOUNT OF TIME, 
	# RATHER THAN THE BEST POSSIBLE SOLUTION.
	# more information here <- https://en.wikipedia.org/wiki/Simulated_annealing
	#
	# BASAL CASE: when energy = 0, print the best solution a time moment
	if(energy == 0){
		predicted.y <- f(x,init.theta)
		e <- error(x,y,init.theta)
		cat("*** SOLUTION NOT FOUND WITH ERROR =",e,"\n")
		plot_solution(x,y,predicted.y,e,energy)
		return(init.theta)	
	} else{
	# GENERAL CASE: when energy != 0, the recursion can continue 	
		# check if theta is empty, this means that theta will 
		# be initialized 
		if(is.na(init.theta[1])){
			# initialize a solution
			theta <- sample(0:15,4,rep=TRUE)
			# compute the error of that solution
			e <- error(x,y,theta)
			# check if the solution is less or equal than the min.error
			if(e<=min.error){
				predicted.y <- f(x,theta)
				cat("GOOD! SOLUTION FOUND WITH ERROR =",e,"\n")
				plot_solution(x,y,predicted.y,e,energy)	
				return(theta)		
			} else {			
				# recursive calls using the auxiliary function
				cat("iteration",energy,"| best solution error",e,"\n")
				simulated.annealing.aux(x,y,e,energy,temperature,min.error,theta)
			}
		} else{
			# this case is when you have a solution from the previous iteration
			# compute the error of that solution
			e <- error(x,y,init.theta)
			# check if the solution is less or equal than the min.error
			if(e<=min.error){
				predicted.y <- f(x,init.theta)
				cat("GOOD! SOLUTION FOUND WITH ERROR =",e,"\n")
				plot_solution(x,y,predicted.y,e,energy)	
				return(init.theta)		
			} else {			
				cat("iteration",energy,"| best solution error",e,"\n")
				# recursive calls using the auxiliary function
				simulated.annealing.aux(x,y,e,energy,temperature,min.error,init.theta)
			}
		}
	}
}



