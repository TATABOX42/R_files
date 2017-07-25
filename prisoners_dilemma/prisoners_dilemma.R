# ***********************************
# To cooperate of defect (besides of coding): Prisoners dilemma, a game theory example in R
# Author: Benjamin Tovar
# Date: August 10, 2014
#
# original post: http://tata-box-blog.blogspot.com/2014/08/to-cooperate-of-defect-besides-of.html
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
# SETUP
# *****************

# load the libraries
library(ggplot2)
library(gridExtra)
source("prisoners_dilemma_lib.R")

# *****************
# CODE
# *****************

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

greedy.bot.vs.tit.for.that.bot <- sapply(index, 
																			function(x) 
																				get.contest(player.1="greedy.bot",
																										player.1.greedy.level=x,
																										player.1.vindictive.level=0,
																										player.2="tit.for.that.bot",						
																										player.2.greedy.level=0,
																										player.2.vindictive.level=0,
																										iter=1000)
																				)
# change the data structure to plot it using ggplot
greedy.bot.vs.tit.for.that.bot.dat <- 
		data.frame(score=c(greedy.bot.vs.tit.for.that.bot[1,],
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
vindictive.bot.vs.tit.for.that.bot.1 <- sapply(index, 
																					function(x) 
																						get.contest(player.1="vindictive.bot",
																							 player.1.greedy.level=0,
																							 player.1.vindictive.level=x,
																							 player.2="tit.for.that.bot",						
																							 player.2.greedy.level=0,
																							 player.2.vindictive.level=0,
																							 iter=1000))

# change the data structure to plot it using ggplot
vindictive.bot.vs.tit.for.that.bot.dat.1 <- 
		data.frame(score=c(vindictive.bot.vs.tit.for.that.bot.1[1,],
	                     vindictive.bot.vs.tit.for.that.bot.1[2,]),
							strategy=c(rep("vindictive.bot",length(index)),
													rep("tit.for.that.bot",length(index))),
							param.level=c(index,index))
# B)
# vindictive.bot with moving greedy.level and vindictive.level threshold
# both parameters are increased equally
vindictive.bot.vs.tit.for.that.bot.2 <- 
		sapply(index, 
			function(x) 
				get.contest(player.1="vindictive.bot",
										player.1.greedy.level=x,
										player.1.vindictive.level=x,
										player.2="tit.for.that.bot",						
										player.2.greedy.level=0,
										player.2.vindictive.level=0,
										iter=1000))
									
# change the data structure to plot it using ggplot
vindictive.bot.vs.tit.for.that.bot.dat.2 <- 
		data.frame(score=c(vindictive.bot.vs.tit.for.that.bot.2[1,],
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
greedy.bot.vs.vindictive.bot.1 <- 
		sapply(index, 
			function(x) 
					get.contest(player.1="greedy.bot",
											player.1.greedy.level=x,
											player.1.vindictive.level=0,
											player.2="vindictive.bot",						
											player.2.greedy.level=0,
											player.2.vindictive.level=x,
											iter=1000))

# change the data structure to plot it using ggplot
greedy.bot.vs.vindictive.bot.dat.1 <- 
	data.frame(score=c(greedy.bot.vs.vindictive.bot.1[1,],
	           				 greedy.bot.vs.vindictive.bot.1[2,]),
						 strategy=c(rep("greedy.bot",length(index)),
						 		    rep("vindictive.bot",length(index))),
						 param.level=c(index,index))


# B)
# greedy level for greedy.bot is increased at the same proportion
# as (1-vindictive.level) for vindictive.bot
# study the effect of inverse proportional threshold values
greedy.bot.vs.vindictive.bot.2 <- 
		sapply(index, 
			function(x) 
				get.contest(player.1="greedy.bot",
										player.1.greedy.level=x,
										player.1.vindictive.level=0,
										player.2="vindictive.bot",						
										player.2.greedy.level=0,
										player.2.vindictive.level=(1-x),
										iter=1000))
								
# change the data structure to plot it using ggplot
greedy.bot.vs.vindictive.bot.dat.2 <- 
	data.frame(score=c(greedy.bot.vs.vindictive.bot.2[1,],
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
# FIGHT CLUB:
# let them fight
# try different parameter settings 
# in an all vs all scheme
# from 0.7 to 1.0 in greedy and vindictive.level
# fierce adversaries!
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
p1 <- ggplot(dat.2$data) +
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

