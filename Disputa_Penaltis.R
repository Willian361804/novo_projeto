PENALTIS = function(){
  a = sample(0:1,3,replace=T,prob=c(0.2,0.8))
  b = sample(0:1,3,replace=T,prob=c(0.2,0.8))
  cont = sum(a);cont2 = sum(b)
  ls1 = list(a,b,paste(cont,"vs",cont2))
  if(cont - cont2 == 3 || cont - cont2 == -3){
    return(ls1)
  }
  a[4] = sample(0:1,1,replace=T,prob=c(0.2,0.8))
  cont = cont + a[4]
  ls2 = list(a,b,paste(cont,"vs",cont2))
  if(cont - cont2 == 3 || cont - cont2 == -2){
    return(ls2)
  }
  b[4] = sample(0:1,1,replace=T,prob=c(0.2,0.8))
  cont2 = cont2 + b[4]
  ls3 = list(a,b,paste(cont,"vs",cont2))
  if(cont - cont2 == 2 || cont - cont2 == -2){
    return(ls3)
  }
  a[5] = sample(0:1,1,replace=T,prob=c(0.2,0.8))
  cont = cont + a[5]
  ls4 = list(a,b,paste(cont,"vs",cont2))
  if(cont - cont2 == 2 || cont - cont2 == -1){
    return(ls4)
  }
  b[5] = sample(0:1,1,replace=T,prob=c(0.2,0.8))
  cont2 = cont2 + b[5]
  ls5 = list(a,b,paste(cont,"vs",cont2))
  if(cont - cont2 == 1 || cont - cont2 == -1){
    return(ls5)
  }
  if(length(a)==5 & length(b)==5 & cont==cont2){
    repeat{
      a = c(a,sample(0:1,1,replace=T,prob=c(0.2,0.8)))
      cont = sum(a)
      b = c(b,sample(0:1,1,replace=T,prob=c(0.2,0.8)))
      cont2 = sum(b)
      if(cont != cont2) break
    }
  }
  return(list(a,b,paste(cont,"vs",cont2)))
}
PENALTIS()

