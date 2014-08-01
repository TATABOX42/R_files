# ***********************************
# 100 Prisoners, 100 lines of code
# Author: Benjamin Tovar
# Date: August 1, 2014
#
# Based on the post: http://www.r-bloggers.com/100-prisoners-100-lines-of-code/ 
# ***********************************

# load the libraries
library(ggplot2)
library(gridExtra)

# set the variable values
n.prisoners=100
n.boxes=100
iters=1000

# ************************
# run the model
# this means, the simulation where
# the prisoners have a strategy
# (they will open their first box based on their 
# own number, and if the box does not contain their
# number, they will move to the box based on the number
# inside the previous opened one)
# ************************

results <- run.model(n.prisoners,n.boxes,iters)

# ************************
# run the random model
# this means, the simulation where
# the prisoners do not have a strategy
# (they will open every box they can by random)
# ************************

results.random <- run.random.model(n.prisoners,n.boxes,iters)

# **********************
# PLOT THE HISTOGRAMS
# OF THE RESULTS 
# **********************

# plot the distribution of the random model
p1 <- ggplot(data.frame(model=rep("random.model",iters),
					  values=results.random)) +
		aes(x=values) +
		geom_histogram(stat="bin",binwidth=1,
					  fill="red",colour="red",alpha=0.1) + 
		labs(title="RANDOM MODEL: Number of prisoners that succeeded the test",
			x="Number of prisoners",
			y="Frequency") 

# plot the distribution of the model
p2 <- ggplot(data.frame(model=rep("model",iters),
					  values=results)) +
		aes(x=values) +
		geom_histogram(stat="bin",binwidth=1,
					  fill="lightblue",colour="blue",alpha=0.3) + 
		labs(title="MODEL: Number of prisoners that succeeded the test",
			x="Number of prisoners",
			y="Frequency") 
# merge the plots
grid.arrange(p1, p2)

# **********************
# PLOT THE DENSITY OF THE RESULTS
# **********************

ggplot(data.frame(model=c(rep("model",iters),rep("random.model",iters)),
				  values=c(results,results.random))) +
	aes(x=values) +
	geom_density(aes(fill=as.factor(model),colour=as.factor(model)),alpha=0.3) + 
	labs(title="Density plot of number of prisoners that succeeded retrieving their number",
		x="Number of prisoners",
		y="Density") 

# **********************
# PLOT THE HISTOGRAMS 
# **********************

ggplot(data.frame(model=c(rep("model",iters),rep("random.model",iters)),
				  values=c(results,results.random))) +
	aes(x=values) +
	geom_histogram(position="dodge",stat="bin",binwidth=1,
					aes(fill=as.factor(model),
						colour=as.factor(model)),alpha=0.3) +
	geom_freqpoly(binwidth=1,aes(fill=as.factor(model),colour=as.factor(model))) + 
	labs(title="Histograms of number of prisoners that succeeded retrieving their number",
		x="Number of prisoners",
		y="Frequency") 

# **********************
# PLOT THE BOXPLOT
# **********************

dat <- data.frame(value=c(results,results.random),
				  strategy=c(rep("model",iters),rep("random.model",iters)))

ggplot(dat) +
	aes(x=strategy,y=value) +
	geom_boxplot(aes(fill=as.factor(strategy),
				 colour=as.factor(strategy)),alpha=0.5) + 
	geom_jitter(colour="darkgreen",alpha=0.15) +
		labs(title="Boxplot of the models",
			x="Model",
			y="Value") 


###################
# FUNCTIONS
###################

run.model <- function(n.prisoners=100,n.boxes=100,iters=1000) {
	results = rep(0,iters)
	# Labels for our prisoners
	prisoners = 1:n.prisoners
 	# run the model
	for(i in 1:iters) {
		# A random permutation:
		boxes = sample(1:n.boxes,n.boxes)
		# Track how many "winners" we have
		foundIt = 0
		# Main loop over the prisoners
		for(prisoner in prisoners) {
			# Track the prisoners path
			path = c(prisoner)
			tries = 1
			# Look first in the box that matches your own number
			inBox = boxes[prisoner]
			while(tries < 50) { 			
				path = c(path, inBox) 			 			
				if(inBox == prisoner) { 				
					foundIt = foundIt + 1 				
					break; 	
				} else { 				
					# Follow that number to the next box 				
					inBox = boxes[inBox] 			
				}	
					tries = tries+1 		
			}
		}
		# How many prisoners found their numbers?
		results[i] = foundIt
	}
	return(results)
}

run.random.model <- function(n.prisoners=100,n.boxes=100,iters=1000) {
	results = rep(0,iters)
	# Labels for our prisoners
	prisoners = 1:n.prisoners
 	# run the model
	for(i in 1:iters) {
		# A random permutation:
		boxes = sample(1:n.boxes,n.boxes)
		# Track how many "winners" we have
		foundIt = 0
		# Main loop over the prisoners
		for(prisoner in prisoners) {
			# create a copy of the boxes object to
			# keep an index of the opened boxes for 
			# each prisoner
			boxes.temp <- boxes
			# Track the prisoners path
			path = c(prisoner)
			tries = 1
			# Look first in the box that matches your own number
			inBox = boxes[prisoner]
			while(tries < 50) { 			
				path = c(path, inBox) 			 			
				if(inBox == prisoner) { 				
					foundIt = foundIt + 1 				
					break; 	
				} else { 				
					# choose randomly among any of the remaining boxes
					# delete that box from the box pool
					boxes.temp <- boxes.temp[boxes.temp!=inBox]	
					# choose among the remaining boxes
					inBox = sample(boxes.temp,1)			
				}	
					tries = tries+1 		
			}
		}
		# How many prisoners found their numbers?
		results[i] = foundIt
	}
	return(results)
}

