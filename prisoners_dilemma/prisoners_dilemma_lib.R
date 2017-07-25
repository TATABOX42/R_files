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


