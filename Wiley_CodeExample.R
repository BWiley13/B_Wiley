


t <- sort(round(runif(1683,.5,1825)))


#randomization rejected counter
rr <- c(0)
rr <- rr +1

#shipments counter
ship_f <- c()

#empty vectors for counting unused drug kits
waste <- c()
lot_waste <- c()


#expiration dates -- this can vary
exp <- 215


#empty vector

drug_age <- c(1)

#site list probabilites - this is based on past site enrollment data in other clinical trials
probs <- c(rep(.028,25),rep(.003846153846,65),rep(.002,25))

#dummy variable for calculating avg % deterministic
mean_det <- c()

#create empty variable for storing # deterministic allocations from each simulation
te <- 0





for (h in 1:100){

ship <- 0


#create site list
sites <-sample(1:115,2190, replace=TRUE,prob=probs)


#create time list - each obs represents a randomization that occurred on a specified day

r <- c(rbinom(n=1825, size=1, prob=.922191780821),rep(0,365))
t <- c(1:2190)

times <- cbind(t, r, sites)

#new_times <- subset(times, t > 0)
#new_times <- new_times[-c(1684:1689),]

#create treatment assignment vector

ta <- c(rep(0,2190))

#create matrix 1

#matrix11 <- cbind(sites,new_times)
#matrix12 <- subset(matrix11, select= -t)
matrix1 <- as.data.frame(cbind(times, ta)) 

#create matrix 2

site2 <- c(1:115)
dslr <- c(rep(0,115))
lr <- c(rep(0,115))
a <- c(rep(0,115))
b <- c(rep(0,115))
c <- c(rep(0,115))
ka1 <- c(rep(1,115))
kb1 <- c(rep(1,115))
kc1 <- c(rep(1,115))



matrix2 <- as.data.frame(cbind(site2, dslr, lr, a, b, c, ka1, kb1, kc1))

#create matrix 3
matrix3 <- subset(matrix1, r!=0)
matrix3 <- matrix3[,-2]
t4 <- matrix3$t+90
t8 <- matrix3$t+210
matrix3 <- cbind(matrix3,t4,t8)

#create overall treatment balance counter

oa <- c(0)
ob <- c(0)
oc <- c(0)
#overall <- as.data.frame(cbind(oa, ob, oc)) 

#create overall drug kit count by lot

tl1a <- 0
tl1b <- 0
tl1c <- 0


#tl <- cbind(tl1a, tl1b, tl1c, tl2a, tl2b, tl2c, tl3a, tl3b, tl3c, tl4a, tl4b, tl4c, tl5a, tl5b, tl5c, tl6a, tl6b, tl6c)


#go day by day through the trial

for(i in 1:2190){


  
######



if(i<1825){matrix2$dslr = matrix2$dslr + 1} #add 1 to days since last randomization for each iteration of the loop

drug_age <- drug_age+1

s <- which(matrix2$site2==sites[i]) #which site did randomization occur at

#############
#Logic#
#############

#number of randomizations on day 1 
if(drug_age==exp){waste <- rbind(waste, (sum(matrix2$ka1)+sum(matrix2$kb1)+sum(matrix2$kc1)))
                  lot_waste <- rbind(lot_waste,(tl1a+tl1b+tl1c))
                  drug_age <- c(1)
                  tl1a <- 0
                  tl1b <- 0
                  tl1c <- 0
                  
                                  
}

if(drug_age==exp){
                ka1 <- c(rep(1,115))
                kb1 <- c(rep(1,115))
                kc1 <- c(rep(1,115))
                matrix2 <- cbind(matrix2[,1:6],ka1,kb1,kc1)
                ship <- ship+115
}

matrix2$ind1 <- c("None","A","B","C","AB","AC","BC","ABC")[((matrix2$ka1>0)+((matrix2$kb1>0)*2)+((matrix2$kc1>0)*3)+(matrix2$ka1>0 & matrix2$kb1>0)+(matrix2$ka1>0 & matrix2$kc1>0)+(matrix2$kb1>0 & matrix2$kc1>0)+((matrix2$ka1>0 & matrix2$kb1>0 & matrix2$kc1>0)*-2))+1]


#RANDOMIZATION#


min <- min(oa,ob,oc)
max <- max(oa,ob,oc)

mins <- min(matrix2$a[s],matrix2$b[s],matrix2$c[s])
maxs <- max(matrix2$a[s],matrix2$b[s],matrix2$c[s])

oas <- matrix2$a[s]
obs <- matrix2$b[s]
ocs <- matrix2$c[s]

total_imbalance <- max-min
site_imbalance <- maxs-mins

CP1 <- 0
CP2 <- 0
CP3 <- 0

if(matrix1$r[i]==1){
  

if(matrix2$ind1[s]=="A"){matrix1$ta[i] <- 1; te <- te+1}
if(matrix2$ind1[s]=="B"){matrix1$ta[i] <- 2; te <- te+1}
if(matrix2$ind1[s]=="C"){matrix1$ta[i] <- 3; te <- te+1}


if(total_imbalance>=4 & (matrix2$ind1[s]=="AB" | matrix2$ind1[s]=="AC" | matrix2$ind1[s]=="ABC") & oa==min){CP1=1} 
if(total_imbalance>=4 & (matrix2$ind1[s]=="AB" | matrix2$ind1[s]=="BC" | matrix2$ind1[s]=="ABC") & ob==min){CP2=1} 
if(total_imbalance>=4 & (matrix2$ind1[s]=="BC" | matrix2$ind1[s]=="AC" | matrix2$ind1[s]=="ABC") & oc==min){CP3=1} 


if(total_imbalance<4 & site_imbalance>=3 & (matrix2$ind1[s]=="AB" | matrix2$ind1[s]=="AC" | matrix2$ind1[s]=="ABC") & oas==mins){CP1=1} 
if(total_imbalance<4 & site_imbalance>=3 & (matrix2$ind1[s]=="AB" | matrix2$ind1[s]=="BC" | matrix2$ind1[s]=="ABC") & obs==mins){CP2=1} 
if(total_imbalance<4 & site_imbalance>=3 & (matrix2$ind1[s]=="BC" | matrix2$ind1[s]=="AC" | matrix2$ind1[s]=="ABC") & ocs==mins){CP3=1} 

if(total_imbalance<4 & site_imbalance<3 & (matrix2$ind1[s]=="AB" | matrix2$ind1[s]=="AC" | matrix2$ind1[s]=="ABC")){CP1=0.3333} 
if(total_imbalance<4 & site_imbalance<3 & (matrix2$ind1[s]=="AB" | matrix2$ind1[s]=="BC" | matrix2$ind1[s]=="ABC")){CP2=0.3333} 
if(total_imbalance<4 & site_imbalance<3 & (matrix2$ind1[s]=="BC" | matrix2$ind1[s]=="AC" | matrix2$ind1[s]=="ABC")){CP3=0.3333} 



CPSum <- CP1+CP2+CP3

if(CPSum==0 & matrix2$ind1[s]=="AB"){
  matrix1$ta[i] <- sample(c(1,2),1)
  } else if(CPSum==0 & matrix2$ind1[s]=="BC"){
    matrix1$ta[i] <- sample(c(2,3),1)  
    } else if(CPSum==0 & matrix2$ind1[s]=="AC"){
    matrix1$ta[i] <- sample(c(1,3),1)  
      } else{
      CP1 <- CP1/CPSum
      CP2 <- CP2/CPSum
      CP3 <- CP3/CPSum 
      randnum <- runif(1,min=0,max=1)
  
  }



if(CPSum!=0){
  if(randnum<CP1){
    matrix1$ta[i] <- 1
    } else if(randnum<CP1+CP2){
      matrix1$ta[i] <- 2
      } else{
      matrix1$ta[i] <- 3
  }
}

if(CPSum==1 & (total_imbalance>4 | site_imbalance>3)){te <- te+1}

}





#UPDATE TRT TOTALS A 
if(matrix1$ta[i]==1){
    oa <- oa + 1
    matrix2$a[s] <- matrix2$a[s] + 1
    matrix2$lr[s] <- 1
    matrix2$ka1[s] <- matrix2$ka1[s] - 1
  }  



#UPDATE TRT TOTALS B
if(matrix1$ta[i]==2)
  {
    ob <- ob + 1
    matrix2$b[s] <- matrix2$b[s] + 1
    matrix2$lr[s] <- 2
    matrix2$kb1[s] <- matrix2$kb1[s] - 1
  }  



#UPDATE TRT TOTALS C
if(matrix1$ta[i]==3)
  {
    oc <- oc + 1
    matrix2$c[s] <- matrix2$c[s] + 1
    matrix2$lr[s] <- 3
    matrix2$kc1[s] <- matrix2$kc1[s] - 1
  }  



#if there is a randomization reset dslr to zero for that site
if(matrix1$r[i]==1){matrix2$dslr[s] <- 0}

#now that drug kit totals are updated, we update our indicator for drug availability
matrix2$ind1 <- c("None","A","B","C","AB","AC","BC","ABC")[((matrix2$ka1>0)+((matrix2$kb1>0)*2)+((matrix2$kc1>0)*3)+(matrix2$ka1>0 & matrix2$kb1>0)+(matrix2$ka1>0 & matrix2$kc1>0)+(matrix2$kb1>0 & matrix2$kc1>0)+((matrix2$ka1>0 & matrix2$kb1>0 & matrix2$kc1>0)*-2))+1]



 for(j in 1:115)
 {
 
    
   #LOT 1
    if(matrix2$dslr[j]==90 & matrix2$ind1[j]=="AB")
      {
        matrix2$kc1[j] <- matrix2$kc1[j] + 2
        tl1c <- tl1c - 2
        ship <- ship+1
      }
    
    if(matrix2$dslr[j]==90 & matrix2$ind1[j]=="AC")
      {
        matrix2$kb1[j] <- matrix2$kb1[j] + 2
        tl1b <- tl1b - 2
        ship <- ship+1
      }
    
    if(matrix2$dslr[j]==90 & matrix2$ind1[j]=="BC")
      {
        matrix2$ka1[j] <- matrix2$ka1[j] + 2
        tl1a <- tl1a - 2
        ship <- ship+1
      }
    
    if(matrix2$dslr[j]==0 & matrix2$ind1[j]=="A" & matrix2$lr[j]==2)
      {
        matrix2$kc1[j] <- matrix2$kc1[j] + 2
        matrix2$kb1[j] <- matrix2$kb1[j] + 1
        tl1c <- tl1c - 2
        tl1b <- tl1b - 1
        ship <- ship+1
      }
    
    if(matrix2$dslr[j]==0 & matrix2$ind1[j]=="A" & matrix2$lr[j]==3)
      {
        matrix2$kc1[j] <- matrix2$kc1[j] + 1
        matrix2$kb1[j] <- matrix2$kb1[j] + 2
        tl1c <- tl1c - 1
        tl1b <- tl1b - 2
        ship <- ship+1
      }
    
      
    if(matrix2$dslr[j]==0 & matrix2$ind1[j]=="B" & matrix2$lr[j]==1)
      {
        matrix2$kc1[j] <- matrix2$kc1[j] + 2
        matrix2$ka1[j] <- matrix2$ka1[j] + 1
        tl1c <- tl1c - 2
        tl1a <- tl1a - 1
        ship <- ship+1
      }
    
    if(matrix2$dslr[j]==0 & matrix2$ind1[j]=="B" & matrix2$lr[j]==3)
      {
        matrix2$kc1[j] <- matrix2$kc1[j] + 1
        matrix2$ka1[j] <- matrix2$ka1[j] + 2
        tl1c <- tl1c - 1
        tl1a <- tl1a - 2
        ship <- ship+1
      }
      
    if(matrix2$dslr[j]==0 & matrix2$ind1[j]=="C" & matrix2$lr[j]==1)
      {
        matrix2$kb1[j] <- matrix2$kb1[j] + 2
        matrix2$ka1[j] <- matrix2$ka1[j] + 1
        tl1b <- tl1b - 2
        tl1a <- tl1a - 1
        ship <- ship+1
      }
    
    if(matrix2$dslr[j]==0 & matrix2$ind1[j]=="C" & matrix2$lr[j]==2)
      {
        matrix2$kb1[j] <- matrix2$kb1[j] + 1
        matrix2$ka1[j] <- matrix2$ka1[j] + 2
        tl1b <- tl1b - 1
        tl1a <- tl1a - 2
        ship <- ship+1
      }
   
   
 }



for(k in 1:nrow(matrix3)){
  
  ss <- which(matrix2$site2==matrix3$sites[k])
  sss <- which(matrix1$t==matrix3$t[k])
  
#IF >1 KIT AT SITE TAKE FROM SITE  
  
  #TRT
  #A
 if((is.element(i,matrix3$t4[k]) | is.element(i,matrix3$t8[k])) & matrix1$ta[sss]==1 & matrix2$ka1[ss]>1)
  {
    matrix2$ka1[ss]-1
 } 
  
  
  #B
  if((is.element(i,matrix3$t4[k]) | is.element(i,matrix3$t8[k])) & matrix1$ta[sss]==2 & matrix2$kb1[ss]>1)
  {
    matrix2$kb1[ss]-1
  } 
  
  
  
  
  #C
  if((is.element(i,matrix3$t4[k]) | is.element(i,matrix3$t8[k])) & matrix1$ta[sss]==3 & matrix2$kc1[ss]>1)
  {
    matrix2$kc1[ss]-1
  } 
  
  
  #IF <=1 KIT AT SITE THEN SHIP NEW KIT
  
  #A
  if((is.element(i,matrix3$t4[k]) | is.element(i,matrix3$t8[k])) & matrix1$ta[sss]==1 & matrix2$ka1[ss]<=1)
  {
    ship <- ship+1
    tl1a <- tl1a-1
  } 
  
  
  
  #B
  if((is.element(i,matrix3$t4[k]) | is.element(i,matrix3$t8[k])) & matrix1$ta[sss]==2 & matrix2$kb1[ss]<=1)
  {
    ship <- ship+1
    tl1b <- tl1b-1
  } 
  
  
  #C
  if((is.element(i,matrix3$t4[k]) | is.element(i,matrix3$t8[k])) & matrix1$ta[sss]==3 & matrix2$kc1[ss]<=1)
  {
    ship <- ship+1
    tl1c <- tl1c-1
  } 
  
  
}


}


ship_f <- rbind(ship_f,ship)
    


te

}
                 
                                                                            
mean_det <- te/h






