
##### ALGORITMO DE LA BURBUJA

swap_pass <- function(v){
  for (i in seq(1,length(v)-1)){
    v[i:(i+1)] = swap(v[i:(i+1)])
  }
  return (v)
}


swap = function(pair){
  if (pair[1]>pair[2]){
    return (rev(pair));
  }
  else {
    return (pair);
  }
}

bubbleSort = function(v){
  new_v = swap_pass(v)
  if (isTRUE(all.equal(v,new_v))){
    return (new_v)
  }
  else{
    return (bubbleSort(new_v))
  }
}


test_vec = round(runif(100,0,100))
bubbleSort(test_vec)
system.time(bubbleSort(test_vec))


###### MERGESORT


mergeSort = function(a,b){
  res <- numeric(length(a)+length(b))
  ai <- 1
  bi <- 1
  for (j in 1:length(res)){
    if (((a[ai]<b[bi]) && ai<=length(a)) || bi > length(b)){
      res[j] = a[ai]
      ai = ai+1
    }
    else{
      res[j]=b[bi]
      bi = bi+1
    }
  }
  res
}

realMerge= function(v){
  if (length(v)>1){
    mid = ceiling(length(v)/2)
    v1 = realMerge(v[1:mid])
    v2 = realMerge(v[(mid+1):length(v)])
    mergeSort(v1,v2)
  }
  else{
    return(v)
  }
}