# ***********************************
# To cooperate of defect (besides of coding): Prisoners dilemma, a game theory example in R
# Author: Benjamin Tovar
# Date: August 10, 2014
#
# original post: http://tata-box-blog.blogspot.com/2014/08/update-100-prisoners-100-lines-of-code.html
#
# ***********************************

# *****************
# INTRODUCTION
# *****************

# I decided to program three basic strategy functions:

# tit.for.tat.bot: this simple strategy repeats opponent's last choice
# 	This bot/strategy is parameter-free

# greedy.bot: this strategy is affected by the parameter of greedy.level, 
# 	which increases the probability of the bot to 
# 	"defect" when this parameter is close to 1.0. 
# 	If this parameter is set to 0.5, then the bot will choose randomly. 
# 	This bot does not care about the previous action of the other 
# 	player, it only decides to "defect" or "cooperate" 
# 	based on its own greediness.

# vindictive.bot:  this strategy is affected by the parameter 
# 	of vindictive.level, which is only used when the previous 
# 	action of the other player is "defect" in the same sense as 
# 	the greedy.level affects the greedy.bot, otherwise 
# 	it will "cooperate". 
# 	This  means that if the other player plays "cooperate", 
# 	this bot will "cooperate" too, but at any moment when 
# 	the other player plays "defect", this bot will trigger its own 
#	vindictive.level to decide if it will play "defect" or to "cooperate".

# All bots have three parameters as arguments:
# 	1. action: last action of the other player
# 	2. greedy.level*
# 	3. vindictive.level

# *The greedy.level parameter in the tit.for.tat.bot and the 
# vindictive.bot is only used when this bots are playing as 
# player.1 and have to decide the first action/move against 
# player.2. After that event, this parameter is no longer used for these bots.

# Each point in the plots means 1000 (iter=1000) 
#	plays of player.1 vs player.2

# Parameters for the payoff matrix: T=20, R=5, P=1 and S=0

# *****************
# METHODS
# *****************

# Try different pairwise comparisons of strategies varying the 
# 	greediness and the vindictiveness of the bots
# Make our clandestine fight club with the bots 
# 	(1st RULE: You do not code about FIGHT CLUB) 

# *****************
# CODE
# *****************

# load the libraries
library(ggplot2)
library(gridExtra)

# run a single run of the function get.contest
get.contest(player.1.strategy="greedy.bot",
			player.1.greedy.level=0.9,
			player.1.vindictive.level=0,
			player.2.strategy="vindictive.bot",						
			player.2.greedy.level=0,
			player.2.vindictive.level=0.7,
			iter=1000)

# ***********************************
# try different threshold parameters
# and make pairwise comparisons
# ***********************************

# set the threshold index
index <- seq(from=0, to=1, by=0.05)

# *****************************
# GREEDY BOT VS TIT FOR TAT BOT
# *****************************

greedy.bot.vs.tit.for.that.bot <- sapply(index, function(x) get.contest(player.1="greedy.bot",
															player.1.greedy.level=x,
															player.1.vindictive.level=0,
															player.2="tit.for.that.bot",						
															player.2.greedy.level=0,
															player.2.vindictive.level=0,
															iter=1000))
# change the data structure to plot it using ggplot
greedy.bot.vs.tit.for.that.bot.dat <- data.frame(score=c(greedy.bot.vs.tit.for.that.bot[1,],
	                                                  	greedy.bot.vs.tit.for.that.bot[2,]),
												 strategy=c(rep("greedy.bot",length(index)),
												 		    rep("tit.for.that.bot",length(index))),
												 greedy.level=c(index,index))

# plot the result
png("greedy_vs_tit_for_tat.png",height=600,width=700)
	ggplot(greedy.bot.vs.tit.for.that.bot.dat) +
		aes(x=greedy.level,y=score) +
		geom_point(aes(shape=as.factor(strategy),
					   colour=as.factor(strategy)),
					size=4) + 
		geom_line(aes(colour=as.factor(strategy))) + 
		labs(title="greedy.bot vs tit.for.that | iter = 1000",
			x="greedy.level parameter for greedy.bot",
			y="score")
dev.off()

# *****************************
# VINDICTIVE BOT VS TIT FOR TAT BOT
# *****************************

# A)
# vindictive.bot with 0 of greedy.level and moving vindictive.level threshold
vindictive.bot.vs.tit.for.that.bot.1 <- sapply(index, function(x) get.contest(player.1="vindictive.bot",
															player.1.greedy.level=0,
															player.1.vindictive.level=x,
															player.2="tit.for.that.bot",						
															player.2.greedy.level=0,
															player.2.vindictive.level=0,
															iter=1000))

# change the data structure to plot it using ggplot
vindictive.bot.vs.tit.for.that.bot.dat.1 <- data.frame(score=c(vindictive.bot.vs.tit.for.that.bot.1[1,],
	                                                  	vindictive.bot.vs.tit.for.that.bot.1[2,]),
													 strategy=c(rep("vindictive.bot",length(index)),
													 		    rep("tit.for.that.bot",length(index))),
													 param.level=c(index,index))
# B)
# vindictive.bot with moving greedy.level and vindictive.level threshold
# both parameters are increased equally
vindictive.bot.vs.tit.for.that.bot.2 <- sapply(index, function(x) get.contest(player.1="vindictive.bot",
															player.1.greedy.level=x,
															player.1.vindictive.level=x,
															player.2="tit.for.that.bot",						
															player.2.greedy.level=0,
															player.2.vindictive.level=0,
															iter=1000))
									
# change the data structure to plot it using ggplot
vindictive.bot.vs.tit.for.that.bot.dat.2 <- data.frame(score=c(vindictive.bot.vs.tit.for.that.bot.2[1,],
	                                                  	vindictive.bot.vs.tit.for.that.bot.2[2,]),
													 strategy=c(rep("vindictive.bot",length(index)),
													 		    rep("tit.for.that.bot",length(index))),
													 param.level=c(index,index))


# plot the results
p1 <- ggplot(vindictive.bot.vs.tit.for.that.bot.dat.1) +
			aes(x=param.level,y=score) +
			geom_point(aes(shape=as.factor(strategy),
							colour=as.factor(strategy)),
							size=4) + 
			geom_line(aes(colour=as.factor(strategy))) + 
			labs(title="A) vindictive.bot vs tit.for.that | iter = 1000",
				x="vindictive.level parameter for vindictive.bot",
				y="score")

p2 <- ggplot(vindictive.bot.vs.tit.for.that.bot.dat.2) +
			aes(x=param.level,y=score) +
			geom_point(aes(shape=as.factor(strategy),
							colour=as.factor(strategy)),
							size=4) + 
			geom_line(aes(colour=as.factor(strategy))) + 
			labs(title="B) vindictive.bot vs tit.for.that | iter = 1000",
				x="greedy.level == vindictive.level parameter for vindictive.bot",
				y="score")

# merge the plots
png("vindictive_vs_tit_for_tat.png",height=800,width=700)
	grid.arrange(p1, p2)
dev.off()

# *****************************
# GREEDY BOT VS VINDICTIVE BOT 
# *****************************

# A)
# greedy level for greedy.bot is increased at the same proportion
# as vindictive.level for vindictive.bot 
# study the effect of direct proportional threshold values
greedy.bot.vs.vindictive.bot.1 <- sapply(index, function(x) get.contest(player.1="greedy.bot",
															player.1.greedy.level=x,
															player.1.vindictive.level=0,
															player.2="vindictive.bot",						
															player.2.greedy.level=0,
															player.2.vindictive.level=x,
															iter=1000))
# change the data structure to plot it using ggplot
greedy.bot.vs.vindictive.bot.dat.1 <- data.frame(score=c(greedy.bot.vs.vindictive.bot.1[1,],
	                                                  	greedy.bot.vs.vindictive.bot.1[2,]),
											 strategy=c(rep("greedy.bot",length(index)),
											 		    rep("vindictive.bot",length(index))),
											 param.level=c(index,index))


# B)
# greedy level for greedy.bot is increased at the same proportion
# as (1-vindictive.level) for vindictive.bot
# study the effect of inverse proportional threshold values
greedy.bot.vs.vindictive.bot.2 <- sapply(index, function(x) get.contest(player.1="greedy.bot",
															player.1.greedy.level=x,
															player.1.vindictive.level=0,
															player.2="vindictive.bot",						
															player.2.greedy.level=0,
															player.2.vindictive.level=(1-x),
															iter=1000))
								
# change the data structure to plot it using ggplot
greedy.bot.vs.vindictive.bot.dat.2 <- data.frame(score=c(greedy.bot.vs.vindictive.bot.2[1,],
	                                                  	greedy.bot.vs.vindictive.bot.2[2,]),
											 strategy=c(rep("greedy.bot",length(index)),
											 		    rep("vindictive.bot",length(index))),
											 param.level=c(index,index))


# plot the results
p1 <- ggplot(greedy.bot.vs.vindictive.bot.dat.1) +
			aes(x=param.level,y=score) +
			geom_point(aes(shape=as.factor(strategy),
							colour=as.factor(strategy)),
							size=4) + 
			geom_line(aes(colour=as.factor(strategy))) + 
			labs(title="A) greedy.bot vs vindictive.bot | iter = 1000",
				x="greedy.level == vindictive.level parameter for greedy.bot and vindictive.bot",
				y="score")

p2 <- ggplot(greedy.bot.vs.vindictive.bot.dat.2) +
			aes(x=param.level,y=score) +
			geom_point(aes(shape=as.factor(strategy),
							colour=as.factor(strategy)),
							size=4) + 
			geom_line(aes(colour=as.factor(strategy))) + 
			labs(title="B) greedy.bot vs vindictive.bot | iter = 1000",
				x="greedy.level threshold for greedy.bot | (1-vindictive.level) parameter for vindictive.bot",
				y="score")

# merge the plots
png("greedy_vs_vindictive.png",height=800,width=700)
	grid.arrange(p1,p2)
dev.off()

# ***********************************
FIGHT CLUB:
let them fight
try different parameter settings 
in an all vs all scheme
from 0.7 to 1.0 in greedy and vindictive.level
fierce adversaries!
# ***********************************

# set the threshold parameters
index <- seq(from=0.7, to=1, by=0.1)

# compute the data.frame of the contest
dat.1 <- get.contest.results(index)

# plot the results
p1 <-	ggplot(dat.1$data) +
			aes(x=x,y=y,label=strategy.index) +
			xlim(3500,5200) + ylim(3500,5200) + 
			aes(shape = factor(strategy)) +
			geom_point(aes(colour = factor(strategy)), size = 5) +
			geom_point(colour="grey90", size = 2) + 
			geom_text(aes(x=x,y=(y+100),colour=factor(strategy)),size = 4) + 
			geom_abline(intercept = 1,colour="grey",size=0.5,linetype="dashed")  + 
			labs(title="A) Strategy performance plot (params from 0.7 to 1.0) | iter = 1000",
				x="mean score (bot is player.1)",
				y="mean score (bot is player.2)")

p2 <-	ggplot(data.frame(strategy.index=dat.1$data$strategy.index,
						  strategy=dat.1$data$strategy,
						  values=rowMeans(dat.1$data[,c("x","y")]))) +
			aes(x=strategy.index,y=values) +
			geom_bar(stat="identity",
					aes(colour=factor(strategy),
						fill=factor(strategy)),alpha=0.3) + 
			labs(title="B) Barplot of mean scores per strategy (params from 0.7 to 1.0) | iter = 1000",
				x="Strategy",
				y="mean score")

png("fight_club_round1.png",height=800,width=850)
	grid.arrange(p1,p2)
dev.off()


# ***********************************
# FIGHT CLUB:
# round 2: let them fight
# try different parameter settings 
# in an all vs all scheme
# from 0.8 to 1.0 in greedy and vindictive.level
# much more fierce adversaries!
# ********************************

# set the threshold parameters
index <- seq(from=0.8, to=1, by=0.1)

# compute the data.frame of the contest
dat.2 <- get.contest.results(index)

# plot the results
p1 <- 	ggplot(dat.2$data) +
			aes(x=x,y=y,label=strategy.index) +
			xlim(2500,4200) + ylim(2500,4200) + 
			aes(shape = factor(strategy)) +
			geom_point(aes(colour = factor(strategy)), size = 5) +
			geom_point(colour="grey90", size = 2) + 
			geom_text(aes(x=x,y=(y+100),colour=factor(strategy)),size = 4) + 
			geom_abline(intercept = 1,colour="grey",size=0.5,linetype="dashed")  + 
			labs(title="A) Strategy performance plot (params from 0.8 to 1.0) | iter = 1000",
				x="mean score (bot is player.1)",
				y="mean score (bot is player.2)")

p2 <-	ggplot(data.frame(strategy.index=dat.2$data$strategy.index,
						  strategy=dat.2$data$strategy,
						  values=rowMeans(dat.2$data[,c("x","y")]))) +
			aes(x=strategy.index,y=values) +
			geom_bar(stat="identity",
					aes(colour=factor(strategy),
						fill=factor(strategy)),alpha=0.3) + 
			labs(title="B) Barplot of mean scores per strategy (params from 0.8 to 1.0) | iter = 1000",
				x="Strategy",
				y="mean score")

png("fight_club_round2.png",height=800,width=700)
	grid.arrange(p1,p2)
dev.off()

# **************************************
# FUNCTIONS
# **************************************

tit.for.that.bot <- function(action,greedy.level,vindictive.level){
	# repeat opponent's last choice
	r <- ifelse(action=="cooperate", "cooperate","defect")
	return(r)
} 

greedy.bot <- function(action,greedy.level,vindictive.level){
	# set the probabilities 
	p <- c((1-greedy.level),greedy.level)
	# retrieve the action
	r <- sample(c("cooperate","defect"),1,prob=p)
	return(r)
}

vindictive.bot <- function(action,greedy.level,vindictive.level){
	# if the previous action is cooperate, then I have noting to forgive
	if(action=="cooperate"){
		r <- "cooperate"
		return(r)
	} else {
		# set the probabilities 
		p <- c((1-vindictive.level),vindictive.level)
		# retrieve the action
		r <- sample(c("cooperate","defect"),1,prob=p)
		return(r)
	}
}

get.score.matrix <- function(T=20,R=5,P=1,S=0){
	# create score matrix
	score.matrix <- matrix(0,nr=2,nc=2)
	rownames(score.matrix) <- colnames(score.matrix) <- c("cooperate","defect")
	score.matrix[1,1] <- R
	score.matrix[2,2] <- P
	score.matrix[1,2] <- S
	score.matrix[2,1] <- T
	return(score.matrix)
}

compute.scores <- function(action.1,action.2,score.matrix){
	# if both bots decide to perform the same action
	score.1 <- score.matrix[action.1,action.2]
	score.2 <- score.matrix[action.2,action.1]
	return(c(score.1,score.2))
}

assign.player <- function(strategy) {
 switch(strategy,
 		greedy.bot = greedy.bot,
        tit.for.that.bot = tit.for.that.bot,
        vindictive.bot = vindictive.bot)
}

get.contest <- function(player.1.strategy="greedy.bot",
						player.1.greedy.level=0.9,
						player.1.vindictive.level=0.5,
						player.2.strategy="tit.for.that.bot",						
						player.2.greedy.level=1,
						player.2.vindictive.level=0.5,
						iter=100,
 						T=20,R=5,P=1,S=0){

	# set the strategies per bot
	player.1 <- assign.player(player.1.strategy)
	player.2 <- assign.player(player.2.strategy)

	# create objects to store results
	action.1 <- action.2 <- character(iter)
	# get the first action of player.1 based on his greediness
	action.1[1] <- sample(c("cooperate","defect"),1,prob=c((1-player.1.greedy.level),player.1.greedy.level))
	action.2[1] <- player.2(action.1[1],player.2.greedy.level,player.2.vindictive.level)
	# run the iterations
	for (i in 2:iter) {
		action.1[i] <- player.1(action.2[i-1],player.1.greedy.level,player.1.vindictive.level)
		action.2[i] <- player.2(action.1[i-1],player.2.greedy.level,player.2.vindictive.level)		
	}
	# merge the actions
	actions <- rbind(action.1,action.2)
	# compute the scores
	score.matrix <- get.score.matrix(T,R,P,S)
	scores <- apply(actions,2, function(x) compute.scores(x[1],x[2],score.matrix))
	rownames(scores) <- c("player.1","player.2")
	# compute the sum
	scores.sum <- rowSums(scores)
	names(scores.sum) <- c("player.1","player.2")
	# export the results
	# scores.list <- list(scores=scores,scores.sum=scores.sum)
	return(scores.sum)
}

get.contest.results <- function(index){

	# this function generates a list with
	# two matrices:
	# for example, A[[1]][i,j] indicates the score
	# of player.1 in row i vs player.2 in column j
	# while A[[2]][i,j] indicates the score of player.2 in
	# row i vs player.1 in column j.
	# therefore A[[1]][i,j] ~= A[[2]][j,i])
	# after computing that matrix, return
	# rowMeans of A[[1]] as the mean score of player.1 i in "x"
	# and rowMeans of A[[2]] as the mean score of player.1 i in "y"
	# in other words 
	# compute the mean of the values when bot is playing as player 1 and player 2
		
	# set the object to store results
	strategy.index <- c(rep("greedy.bot",length(index)),
						rep("vindictive.bot",length(index)),
						rep("tit.for.that.bot",1))

	param.index <- rep(index,length(strategy.index))
	# get matrix to store results
	results.matrix <- matrix(0,nr=length(strategy.index),nc=length(strategy.index))
	c.names <- paste(strategy.index,index,sep=".")
	c.names[length(c.names)] <- "tit.for.that.bot"
	rownames(results.matrix) <- colnames(results.matrix) <- c.names

	results <- list(player.1=results.matrix,
					player.2=results.matrix)
	names(results) <- c("player.1","player.2")
	# let 'em fight
	for (i in 1:length(strategy.index)) {
		for (j in 1:length(strategy.index)) {
			# get the parameters player 1
			player.1.greedy.level <- ifelse(strategy.index[i]=="greedy.bot", param.index[i], 0)
			player.1.vindictive.level <- ifelse(strategy.index[i]=="vindictive.bot", param.index[i], 0)
			# get the parameters player 1
			player.2.greedy.level <- ifelse(strategy.index[j]=="greedy.bot", param.index[j], 0)
			player.2.vindictive.level <- ifelse(strategy.index[j]=="vindictive.bot", param.index[j], 0)
			# populate the matrix
			r <- get.contest(player.1=strategy.index[i],
							player.1.greedy.level=player.1.greedy.level,
							player.1.vindictive.level=player.1.vindictive.level,
							player.2=strategy.index[j],						
							player.2.greedy.level=player.2.greedy.level,
							player.2.vindictive.level=player.2.vindictive.level,
							iter=1000)
			results$player.1[i,j] <- r[1]
			results$player.2[j,i] <- r[2]

		}
		cat("iteration",i,date(),"\n")
	}
	# export the results 
	dat <- data.frame(x=rowMeans(results$player.1),
					  y=rowMeans(results$player.2),
					  strategy=strategy.index,
					  strategy.index=rownames(results.matrix))
	# compute the mean of the values when bot is playing as player 1 and player 2
	results.matrix <- (results$player.1 + results$player.2)/2
	res <- list(data=dat,
				matrix=results.matrix)
	return(res)
}


