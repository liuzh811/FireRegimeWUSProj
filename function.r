#functions used in the fire simulation model

#function 1: fire spread function
FireSp3 <- function(row.n, col.n, size, p1)  ##row.n, col.n in cells, size in ha, p1 is the burned probability matrix
{
  ##convert fire size into numbers of cells
  cell.n = floor(size*0.01)
  #1 spread begins
  #1.1 produce an accumulative travel cost surface
  #1.1.1 producing distance to fire source map
  p2 = p1
  for (i in 1:nrow(p1)){ 
    for (j in 1:ncol(p1)){ 
      p2[i,j] = sqrt((i-row.n)*(i-row.n)+(j-col.n)*(j-col.n))
    }
  }
  #1.1.2 producing accumulative travel cost surface
  BP.cost = p2/p1
  #1.2 produce an fire shape
  Breakpoint = 0
  while(cell.n - sum(rowSums(FireSP))>1) { #do until the difference between pre-defined fire size and simulated fire size < 2 cells
    Breakpoint = Breakpoint+1
    FireSP[BP.cost<Breakpoint] = 1
  }
  return(FireSP) ##return a matrix
}

#function 2: modified function 1 for egde effects
FireSp4 <- function(row.n, col.n, size, nc = 1683, nr = 2174)  ##row.n, col.n in cells, size in ha, p1 is the burned probability matrix
{
  ##convert fire size into numbers of cells
  cell.n = size*0.01
  R = floor(sqrt(cell.n)/2)
   if ((col.n-R)<0)
     col.n = col.n+R
	  else if ((col.n+R)>nc)
        col.n = col.n-R
        else if ((row.n-R)<0)
		 row.n = row.n+R
		  else if ((row.n+R)>nr)
		  row.n = row.n-R
  else
  FireSP[(row.n-R):(row.n+R),(col.n-R):(col.n+R)] = 1
  return(FireSP) ##return a matrix
}

#function 3: determine the fire occurrence date
SAMPLE = function(x){
if (length(which(x>=40)>0)) {
 P1 = which(x>=40)
 P = P1[sample(1:length(P1), 1)]
 } else 
 if (length(which(x>=30 & x<40)>0)) {
 P1 = which(x>=30 & x<40)
 P = P1[sample(1:length(P1), 1)]
  } else 
  if (length(which(x>=20 & x<30)>0)) {
  P1 = which(x>=20 & x<30)
  P = P1[sample(1:length(P1), 1)]
 } else 
	 P1 = which(x>=1 & x<20)
     P = P1[sample(1:length(P1), 1)]
return(P)
}

#function 4: record whether >45 days requirement is met or not
SAMPLE2 = function(x){
if (length(which(x>=45)>0)) {
 P1 = length(which(x>=45))
 } else 
  P1 = 0
return(P1)
}
