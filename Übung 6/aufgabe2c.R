standardize <- function(x){(x-min(x))/(max(x)-min(x))}

random <- function(seed, n){
  if((seed %% 2) == 0){
    seed <- seed +1
  }
  start <- as.character((seed*n)^2)
  save <- c()
  for (i in 1:(n+2)){
    if (i == 1){
      x <- start
      k <- nchar(x)
      y <- as.numeric(substr(as.numeric(x), round(0.25*k), round(0.75*k)))
      save[i] <- y
    }
    else{
      x <- as.character((save[i-1]*n)^2)
      k <- nchar(x)
      y <- as.numeric(substr(as.numeric(x), round(0.25*k), round(0.75*k)))
      save[i] <- y
    }
  }
  return(standardize(save%%100))
}

test <- random(27, 10000)
hist(test)
