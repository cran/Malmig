Lasker <- function(x){
somme <- colSums(x)
M <- matrix(data=0,nrow=length(somme),ncol=length(somme))
for(i in 1:length(somme)){
  for(j in 1:length(somme)){
    M[i,j] <- (sum(x[,i]*x[,j]))/(2*(somme[i]*somme[j]))
  }
}
M
}
