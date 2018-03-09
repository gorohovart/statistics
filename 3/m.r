
library(purrr)

Xa1 = 0
Xa2 = 0
Xa3 = 0

Xb1 = 100
Xb2 = 100
Xb3 = 100



gweights = c(0.2, 0.3, 0.5)

weights = function(x) {
  return(as.numeric(unlist(gweights[x])))
}

unif = function(n, Xa, Xb, i){
  if (i==1)
    return(runif(weights(1)*n, as.numeric(unlist(Xa[1])), as.numeric(unlist(Xb[1]))))
  else if (i==2)
    return(runif(weights(2)*n, as.numeric(unlist(Xa[2])), as.numeric(unlist(Xb[2]))))
  else if(i == 3)
    return((runif(weights(3)*n, as.numeric(unlist(Xa[3])), as.numeric(unlist(Xb[3])))))
  else
    return(list(1,2))
}

choose = function() {sample(1:3, 1, prob = gweights)}

getSample = function(i,n,Xa,Xb){
  if (i == 1){
    return (1:n %>% map(function(ewq) unif(n,Xa,Xb, as.numeric(unlist(choose())))))
  }
  
  else if(i == 2){
    return (c(runif(n*weights(1), as.numeric(unlist(Xa[1])), as.numeric(unlist(Xb[1]))),
              runif(n*weights(2), as.numeric(unlist(Xa[2])), as.numeric(unlist(Xb[2]))),
              runif(n*weights(3), as.numeric(unlist(Xa[3])), as.numeric(unlist(Xb[3])))))
  }
  else if(i == 3){
    s = weights(1)*(as.numeric(unlist(Xb[1])) - as.numeric(unlist(Xa[1])))*(as.numeric(unlist(Xb[1])) - as.numeric(unlist(Xa[1])))
    + weights(2)*(as.numeric(unlist(Xb[2])) - as.numeric(unlist(Xa[2])))*(as.numeric(unlist(Xb[2])) - as.numeric(unlist(Xa[2])))
    + weights(3)*(as.numeric(unlist(Xb[3])) - as.numeric(unlist(Xa[3])))*(as.numeric(unlist(Xb[3])) - as.numeric(unlist(Xa[3])))/12
    n1 = n / 12 * weights(1)*(as.numeric(unlist(Xb[1])) - as.numeric(unlist(Xa[1])))*(as.numeric(unlist(Xb[1])) - as.numeric(unlist(Xa[1]))) / s
    n2 = n / 12 * weights(2)*(as.numeric(unlist(Xb[2])) - as.numeric(unlist(Xa[2])))*(as.numeric(unlist(Xb[2])) - as.numeric(unlist(Xa[2]))) / s
    n3 = n / 12 * weights(3)*(as.numeric(unlist(Xb[3])) - as.numeric(unlist(Xa[3])))*(as.numeric(unlist(Xb[3])) - as.numeric(unlist(Xa[3]))) / s
    cat(n1)
    return (c(runif(n1, as.numeric(unlist(Xa[1])), as.numeric(unlist(Xb[1]))),
              runif(n2, as.numeric(unlist(Xa[2])), as.numeric(unlist(Xb[2]))),
              runif(n3, as.numeric(unlist(Xa[3])), as.numeric(unlist(Xb[3])))))
  }
  else return(list(1,2,3))
}

calcMeansAndVars = function (n, borders){
  Xa = borders[1]
  Xb = borders[2]
  
  data = 
    1:3 %>%
    map(function(i) {
      meansAndVars = 
        1:10 %>%
        map(function(x){
          samp = unlist(getSample(as.numeric(unlist(i)), n, Xa, Xb))
          return(c(mean(samp), (n-1)/n * var(samp)))
        })
      
      
      means = unlist(map(meansAndVars, function(l) l[1]))
      vars = unlist(map(meansAndVars, function(l) l[2]))
      return(list(means,vars))
    })
  return (data)
  #hist(unlist(data[1][1]))
  #,data[2][1],data[3][1])
  #hist(data[1][2],data[2][2],data[3][2])
}

borders = 
  list(list(0, 0,   0),   list(100, 100, 100),
       list(0, 100, 200), list(100, 200, 300),
       list(0, 120, 15),  list(100, 185, 150)
  )


calcMeansAndVars(40, borders[1])
#calcMeansAndVars(40, borders[2])
#calcMeansAndVars(40, borders[3])
```