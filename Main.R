library(tseries)
library(quantmod)

# Define variables 
n.periods <- nrow(mat.cbsa.returns)
n.stocks <- ncol(mat.cbsa.returns)

# Excess returns for every series
mat.cbsa.excess <- matrix(nrow = n.periods, ncol = n.stocks)
for (i in 1:n.stocks) {
  mat.cbsa.excess[,i] <- mat.cbsa.returns[,i]-as.numeric(dat.quarter.rf)
}

# Define which returns to take (normal or excess)
mat.cbsa.returns <- mat.cbsa.excess

# Step 1: Build matrix and define variables
threshold <- 36 # min observation

# 0 if preselect, 1 if firsttime
firsttime <- 1
n.preselect <- 0

# Create pre selected matrix
# number 11
if (firsttime == 0) {
  mat.preselect <- matrix(ncol = n.preselect, nrow = fac.periods)
  mat.preselect[,1] = mat.factors[,11]
  mat.preselect[,2] = mat.factors[,12]
  mat.can = mat.factors[,-11]
  mat.can = mat.can[,-11]
  n.can <- ncol(mat.can)
} else {
  mat.can = mat.factors
  n.can <- ncol(mat.can)
}

# Step 2: Original Regression
original.mean.stats <- numeric(n.can) # store mean-based stats
original.med.stats <- numeric(n.can) # store median-based stats


for (i in 1:n.can) {
  k = mat.can[,i] 
  icept0 <- numeric(n.stocks)
  icept1 <- numeric(n.stocks)
  
  for (j in 1:n.stocks) {
    y = mat.cbsa.returns[,j]
    ind <- 1-as.integer(is.na(mat.cbsa.returns[,1]))
    if (sum(ind) < threshold) {
      icept0[j] = NA
      icept1[j] = NA
    } else  {
      yy = y[ind == 1]
      kk = k[ind ==1]
      if (firsttime == 1) {
        xx0 = matrix(rep(1,sum(ind)))
      } else {
        ll = mat.preselect[ind == 1,1]
        ll2 = mat.preselect[ind == 1,2]
        xx0 = matrix(nrow=sum(ind),ncol = 1+n.preselect)
        xx0[,1] = rep(1,sum(ind))
        xx0[,2] = ll
        xx0[,3] = ll2# Need to add if more factors
      }
      xx1 <- matrix(nrow = sum(ind),ncol = n.preselect + 2)# Need to change if pre selected factors
      xx1[,1] = rep(1,sum(ind))
      xx1[,2] = kk
      beta0 = solve(t(xx0) %*% xx0) %*% t(xx0) %*% yy
      beta1 = solve(t(xx1) %*% xx1) %*% t(xx1) %*% yy
      
      res0 = yy - xx0 %*% beta0
      nvar = ncol(xx0)
      nobs = nrow(xx0)
      sig2e = (t(res0)%*%res0)/(nobs-nvar)
      
      inv.xx = diag(nvar)/(t(xx0)%*%xx0)
      olsse = sqrt(sig2e*diag(inv.xx))
      
      icept0[j] = abs(beta0[1]/olsse[1])
      icept1[j] = abs(beta1[1]/olsse[1])
    }
  }
  original.mean.stats[i] = (mean(icept1, na.rm = TRUE)-mean(icept0, na.rm = TRUE))/mean(icept0, na.rm = TRUE)
  original.med.stats[i] = (median(icept1, na.rm = TRUE)-median(icept0, na.rm = TRUE))/median(icept0, na.rm = TRUE)
}

# Step 3: Orthogonalization 

fac.can = matrix(nrow = nrow(mat.can), ncol = ncol(mat.can))
if (firsttime == 1) {
  xx = matrix(rep(1,n.periods))
} else {
  xx = matrix(nrow=n.periods,ncol = 1+n.preselect)
  xx[,1] = rep(1,n.periods)
  xx[,2] = mat.preselect[,1]
  xx[,3] = mat.preselect[,2]# Need to add if more factors
}

for (i in 1:n.can) {
  yy <- mat.can[,i]
  beta = solve(t(xx) %*% xx) %*% (t(xx) %*% yy)
  fac.can[,i] = yy-beta[1]
}

# Step 4: bootstrap
n.sim = 10000

bb.mean.stats = matrix(nrow = n.can,ncol=n.sim)
bb.med.stats= matrix(nrow = n.can,ncol=n.sim)

for (s in 1:n.sim) {
  sam = as.integer(runif(n.periods, min = 1, max = n.periods))
  k.bb = fac.can[sam,]
  # baseline factors to be added
  ret.bb = mat.cbsa.returns[sam,]
  
  temp.mean.stats = numeric(n.can)
  temp.med.stats= numeric(n.can)
  
  for (i in 1:n.can) {
    k = k.bb[,i] 
    icept0 <- numeric(n.stocks)
    icept1 <- numeric(n.stocks)
    
    for (j in 1:n.stocks) {
      y = ret.bb[,j]
      ind <- 1-as.integer(is.na(ret.bb[,1]))
      
      if (sum(ind) < threshold) {
        icept0[j] = NA
        icept1[j] = NA
      } else  {
        yy = y[ind == 1]
        kk = k[ind ==1]
        if (firsttime == 1) {
          xx0 = matrix(rep(1,sum(ind)))
        } else {
          ll = mat.preselect[ind == 1,1]
          ll2 = mat.preselect[ind == 1,2]
          xx0 = matrix(nrow=sum(ind),ncol = 1+n.preselect)
          xx0[,1] = rep(1,sum(ind))
          xx0[,2] = ll
          xx0[,3] = ll2
        }
        xx1 <- matrix(nrow = sum(ind),ncol = 2+n.preselect)# Need to change if pre selected factors
        xx1[,1] = rep(1,sum(ind))
        xx1[,2] = kk
        beta0 = solve(t(xx0) %*% xx0) %*% (t(xx0) %*% yy)
        beta1 = solve(t(xx1) %*% xx1) %*% (t(xx1) %*% yy)
        
        res0 = yy - xx0 %*% beta0
        nvar = ncol(xx0)
        nobs = nrow(xx0)
        sig2e = (t(res0)%*%res0)/(nobs-nvar)
        
        inv.xx = diag(nvar)/(t(xx0)%*%xx0)
        olsse = sqrt(sig2e*diag(inv.xx))
        
        icept0[j] = abs(beta0[1]/olsse[1])
        icept1[j] = abs(beta1[1]/olsse[1])
      }
    }
    temp.mean.stats[i] = (mean(icept1, na.rm = TRUE)-mean(icept0, na.rm = TRUE))/mean(icept0, na.rm = TRUE)
    temp.med.stats[i] = (median(icept1, na.rm = TRUE)-median(icept0, na.rm = TRUE))/median(icept0, na.rm = TRUE)
  }
  bb.mean.stats[,s] = temp.mean.stats
  bb.med.stats[,s] = temp.med.stats
}


# Step 5: Results

pval.mean = numeric(n.can)
pval.med = numeric(n.can)
quant10.mean = numeric(n.can)
quant10.med = numeric(n.can)
quant5.mean = numeric(n.can)
quant5.med = numeric(n.can)

for (i in 1:n.can) {
  pval.mean[i] = sum(bb.mean.stats[i,]<original.mean.stats[i], na.rm = TRUE)/n.sim
  pval.med[i] = sum(bb.med.stats[i,]<original.med.stats[i], na.rm = TRUE)/n.sim
  quant10.mean[i] = quantile(bb.mean.stats[i,],na.rm = TRUE, probs = 0.10)
  quant10.med[i] = quantile(bb.med.stats[i,],na.rm = TRUE, probs = 0.10)
  quant5.mean[i] = quantile(bb.mean.stats[i,],na.rm = TRUE, probs = 0.05)
  quant5.med[i] = quantile(bb.med.stats[i,],na.rm = TRUE, probs = 0.05)
}

min.sim.mean <- numeric(n.sim)
min.sim.med <- numeric(n.sim)
for(j in 1:n.sim) {
  if (is.nan(bb.mean.stats[1,j])) {
    min.sim.mean[j] = NaN
    min.sim.med[j] = NaN
  } else {
    min.sim.mean[j] = min(bb.mean.stats[,j],na.rm = TRUE)
    min.sim.med[j] = min(bb.med.stats[,j],na.rm = TRUE)
  }
}
muliple.pval.mean <- sum(min.sim.mean<min(original.mean.stats), na.rm = TRUE)/n.sim
muliple.pval.med <- sum(min.sim.med<min(original.med.stats), na.rm = TRUE)/n.sim

multiple.5perc.mean <- quantile(min.sim.mean,na.rm = TRUE, probs = 0.05)
multiple.5perc.med <- quantile(min.sim.med,na.rm = TRUE, probs = 0.05)
