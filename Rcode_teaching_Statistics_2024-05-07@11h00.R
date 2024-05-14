

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~ Section 2 ~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

entry = c(500,1182,150,460,340,14)

# Table 1:

MakeTable <- function(entry){
  mat = matrix(entry,3,2)
  cs  = colSums(mat)
  rs  = rowSums(mat)
  tot = sum(mat)
  out = matrix(0,4,3)
  out[1:3,1:2] = mat
  out[1:3,3] = rs
  out[4,1:2] = cs
  out[4,3] = tot
  return(out)
}

MakeTable(entry)

# Table 2:

CalcProbs <- function(entry){
  mat = matrix(entry,3,2)
  cs  = colSums(mat)
  rs  = rowSums(mat)
  tot = sum(mat)
  out = matrix(rs)%*%t(matrix(cs))/tot^2
  return(out)
}

prbs = CalcProbs(entry)
MakeTable(prbs)

# Table 3

n = sum(entry)
MakeTable(CalcProbs(entry)*n)

# Tables 4 - 7

ent = rmultinom(1,n,prbs)
MakeMatrix(ent)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~ Section 3 ~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

cat("\014")
graphics.off()

# Figure 2 and Table 11:

simulator <- function(mu,            #Specify the 'population' mean
	sig,           #Specify the 'population' standard deviation
	N,             #Specify the 'population' size
	n,             #Specify the sample size
	MC,            #Specify the number of Monte Carlo simulations 
	seed=123,      #Specify the random seed
	xlim=c(52,80), #Specify the x-axis limits of the histogram
	breaks=6,      #Specify the number of breaks of the histogram 
	statistic      #Specify the statistic to calculate for each sample
){
	set.seed(seed)                              #Set the seed.	
	X         <- round(rnorm(N,mu,sig),0)       #Generate a 'population'
	mystat    <- numeric(MC)                    #Initalise the vector of MC sample statistics  
	stat.name <- deparse(substitute(statistic)) #Extract the name of the statistic calculated
	x.mat     <- NULL            #Initialise the matrix which will contain the sample data  
	for(i in 1:MC){                                   #Start the MC for loop
		x           <- sample(X,size=n,replace=FALSE)   #Generate the sample from the 'population' 
		mystat[i]   <- statistic(x)                     #Calculate the statistic from the sample 
		x.mat       <- rbind(x.mat,(matrix(x,ncol=n)))  #Store the generated samples in a matrix
	}
	#Construct the histogram:
	hist(mystat,   main=paste0("Sample size of ",n), xlab="Sample means",   xlim=xlim)
	cat(paste0("\n~~~~~~~~~~~~~~~~\nSample size=",n,"\nStatistic=",stat.name,"\n~~~~~~~~~~~~~~~~\n"))
	cat("Summary of the 'population':\n")
	print(summary(X))
	cat(paste0("\nmean of ",stat.name,"s =",mean(mystat),"\n"))
	cat(paste0("\nvariance of ",stat.name,"s =",var(mystat),"\n\n"))
	datatable        <- data.frame(cbind(x.mat,mystat))
	names(datatable) <- c(1:n,stat.name)
	print(head(datatable,3))
	cat(".\n.\n.\n")
	print(tail(datatable,1))
	
	return(datatable)
}	

mu  <- 65
N   <- 50
sig <- 13.5
MC  <- 1e4

ans1 <- simulator(mu,sig,N,n=10,MC,statistic=mean)
# ans2 <- simulator(mu,sig,N,n=30,MC,statistic=mean)
# ans3 <- simulator(mu,sig,N,n=10,MC,statistic=max)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
