library(doParallel)
data <- read.csv("HT-Table11-P1-randomised.csv", header=TRUE)
f <- read.csv("num2.csv", header=FALSE)
grid <- matrix(1, 150, 21, byrow=TRUE)
grid_height <- nrow(grid)
grid_width <- ncol(grid)
bottom_left <- FALSE #True if bottom-left-algorithm

start <- Sys.time()
#Rprof(tmp <- tempfile())
#Put box into grid
setBox <- function(box){
  #Set box properties
  box_height <- ncol(box)
  box_width <- nrow(box)
  box_placed <- FALSE

  #Iterate from bottom of grid
  for(row in grid_height:1){
    #Iterate from first col
    for(col in 1:grid_width){

      sum <- sum(grid[row, ])
      if(sum < box_width){
        break
        if(sum < box_height ){
          break
        }
        else{
          box <- t(box)
          box_height <- ncol(box)
          box_width <- nrow(box)
        }
      }

      if( (grid_width+1) %in% c(col:(col+box_width-1)) ){
        break
      }

      #If current box fits in sub-matrix
      if(all(grid[row:(row-box_height+1), col:(col+box_width-1)] == 1)){
        #If bottom left activated
        if( bottom_left & !all(grid[row:1, col:(col+box_width-1)] == 1)){
          #Break if lowest 'available' position is not within reach
          break
        }
          #Place box
          grid[row:(row-box_height+1), col:(col+box_width-1)] <- box
          box_placed <- TRUE
          break
      }
    }

    #Break if box is placed
    if( box_placed ){
      break
    }
  }
  return(grid)
}

main <- function() {

  for(i in c(f[,1])){
  #for(i in 1:2){
    print(i)
    box <- matrix(0, data[i,1], data[i,2])
    grid <<- setBox(box)
    write.csv(grid, file = "grid.csv")
  }

  height <- nrow(grid) - min(which(rowSums(grid==1) != ncol(grid)))
  cat(sprintf("Height: %s\n", height))
  cat(sprintf("Current area of grid covered: %s\n", sum(colSums(grid == 0))))
  cat(sprintf("Total area of boxes from file: %s\n", sum(data[,1]*data[,2])))
}

#main()
#quit()

##################################################
##Start Genetic Algorithm
##################################################
#For repeatability
set.seed(100)
this_pop <- cbind(c(1:length(data[,1])), sample(c(1:length(data[,1]))), sample(c(1:length(data[,1]))), sample(c(1:length(data[,1]))))

#Evalutate population by height
evaluate_pop <- function(pop){
  popsize <- length(pop[,1])
  varsize <- length(pop[1,])
  min_height <- 800

  evals <- rep(0, varsize)

  for(i in 1:varsize){
    for( j in 1:popsize){
      box <- matrix(0, data[pop[,i][j],1], data[pop[,i][j],2])
      grid <<- setBox(box)
    }
    height <- nrow(grid) - min(which(rowSums(grid==1) != ncol(grid)))

    #Check if current generation is better
    if( height < min_height ){
      min_height <- height
      if( height <= 89 ){
        print("OPTIMAL SOLUTION")
        write.csv(pop[,i], file = "num2.csv", col.names=FALSE, row.names=FALSE)
        write.csv(grid, file = "opt.csv")
        #quit()
      }
      #write.csv(pop[,i], file = "num1.csv", col.names=FALSE, row.names=FALSE)
      #write.csv(grid, file = "grid.csv")
    }
    evals[i] <- height
    grid <<- matrix(1, 150, 21, byrow=TRUE)
  }
  #print(evals)
  evals <- penalize(evals)
  p <- (1 - (evals)/(sum(evals)))
  #print(p)
  return (sample(c(1:varsize), varsize, replace=TRUE, prob=c(p)))
}


#Penalize the two worst in population
penalize <- function(evals){
  max_index <- which.max(evals)
  evals[max_index] <- evals[max_index] + 200

  n <- length(unique(evals))
  second_max_index <- which(evals == sort(unique(evals),partial=n-1)[n-1])
  evals[second_max_index] <- evals[second_max_index] + 200
  return (evals)
}

#Create children from parent population
reproduce <- function(parents){
  for( i in 1:2 ){
    cycle_start_bool <- FALSE; cycle_start <- 1; index <- 1; remove <- c()
    while(TRUE){
      value <- this_pop[,parents[i+1]][index]
      if( index == cycle_start & cycle_start_bool){
        index <- which(this_pop[,parents[i]] == value)
        remove <- c(remove, index)
        break
      }
      cycle_start_bool <- TRUE

      index <- which(this_pop[,parents[i]] == value)
      remove <- c(remove, index)
    }

    edit <- this_pop[,parents[i]]
    edit <- edit [! edit %in% remove]

    temp <- this_pop[,parents[i]]

    for( s in edit ){
      this_pop[,parents[i]] <- replace(this_pop[,parents[i]], s, this_pop[,parents[i+1]][s])
      this_pop[,parents[i+1]] <- replace(this_pop[,parents[i+1]], s, temp[s])
    }

    if( (length(unique(this_pop[,parents[i]])) != 28) | (length(unique(this_pop[,parents[i+1]])) != 28)){
      print("Wooah")
      quit()
    }

  }
  return(this_pop)
}

#Mutate with a 5% chance
mutate <- function(pop){
  for( i in 1:4 ){
    if(runif(1) > 0.95){
      child_to_mutate <- sample(c(1:4), 1, 1/4)
      index_one <- sample(c(1:28), 1, 1/28)
      index_two <- sample(c(1:28), 1, 1/28)
      pop[,parents[child_to_mutate]] <- replace(pop[,parents[child_to_mutate]], c(index_one, index_two), this_pop[,parents[child_to_mutate]][c(index_two, index_one)])
      return(pop)
  }}
  return (pop)
}


#Parallelize
cl <- makeCluster(4, outfile="")
registerDoParallel(cl)


a <- foreach(i = 1:1000, .combine="c") %dopar% {
  parents <- evaluate_pop(this_pop)
  this_pop <- reproduce(parents)
  this_pop <- mutate(this_pop)
}
stopCluster(cl)

#for(i in 1:50){
#  this_pop <- reproduce(evaluate_pop(this_pop))
#}

#Rprof()
#summaryRprof(tmp)

end <- Sys.time()
print(end - start)

#write.csv(grid, file = "grid.csv")
#print(sprintf("Box height: %s, box width: %s", dim(box)[2], dim(box)[1]))
#a <- readLines("stdin",n=1);

#box_one_index_in_first_parent <- which(this_pop[,parents[1]] == box_one)
#box_two_index_in_first_parent <- which(this_pop[,parents[1]] == box_two)

#box_one_index_in_second_parent <- which(this_pop[,parents[1+1]] == box_one)
#box_two_index_in_second_parent <- which(this_pop[,parents[1+1]] == box_two)

#this_pop[,parents[i]] <- replace(this_pop[,parents[i]], c(box_one_index_in_second_parent, box_two_index_in_second_parent), this_pop[,parents[i]][c(box_one_index_in_first_parent, box_two_index_in_first_parent)])
#this_pop[,parents[i+1]] <- replace(this_pop[,parents[i+1]], c(box_one_index_in_first_parent, box_two_index_in_first_parent), this_pop[,parents[i+1]][c(box_one_index_in_second_parent, box_two_index_in_second_parent)])

#print(this_pop[,parents[1]])
#print(this_pop[,parents[2]])

#print(sprintf("Value: %s", value))
#print(sprintf("Index: %s", index))
#print("Parent 1: ")
#print(this_pop[,parents[1]])
#print(this_pop[,parents[2]])
#c <- readLines("stdin",n=1);


#print(this_pop[,parents[i]])
#print(this_pop[,parents[i+1]])

#print("------------------------")

#print(length(unique(this_pop[,parents[i]])))
#print(length(unique(this_pop[,parents[i+1]])))

#print("In cycle: ")
#print(sort(remove))

#print("edit")
#print(edit)

#Break if box exceeds grid size
#if( (grid_width+1) %in% c(col:(col+box_width-1)) ){
#  #Try to rotate box
#  box <- t(box)
#  box_height <- as.numeric(dim(box)[2])
#  box_width <- as.numeric(dim(box)[1])
#  if( (grid_width+1) %in% c(col:(col+box_width-1)) | box_height > 5 ){
#    box <- t(box)
#    break
#  }
#}
