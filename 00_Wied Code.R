library(AER)
data <- read.table("SOEP_2020.txt")
data <- data[which(is.na(data$GrossIncome)==0),]
data <- data[which(is.na(data$Educ)==0),]
data <- data[which(is.na(data$Motheduc)==0),]
data <- data[which(is.na(data$Exper)==0),]
data <- data[which(data$Exper > 0),]
y <- log(data$GrossIncome)
educ <- data$Educ
exper <- data$Exper
expersq <- exper^2
motheduc <- data$Motheduc
fatheduc <- data$Fatheduc
partt <- data$PartTime
summary(lm(y~educ+exper+expersq+partt))
summary(ivreg(y~educ+exper+expersq+partt|motheduc+exper+expersq+partt))

nn <- length(educ)

## define the quantiles of the regressors that we want to use 
q_edu <- c(quantile(educ,0.1),quantile(educ,0.5),quantile(educ,0.9))
q_exper <- c(quantile(exper,0.1),quantile(exper,0.5),quantile(exper,0.9))
q_partt <- rep(mean(partt),3)
q_t <- c("01","05","09") 
y_axis <- matrix(c(-0.2,0.5,-0.1,0.1,-0.3,0.5),ncol = 2,byrow = TRUE) ## set up range of y-axis for plotting purpose later

for (q in 1:3){
  meaneduc <- q_edu[q]
  meanexper <- q_exper[q]
  meanpartt <- q_partt[q]
  
  ############################################################
  ############################################################
  ############################################################
  ## OLS and IV regression#
  
  ## run the OLS and IV regression
  linmod <- lm(y~educ+exper+expersq+partt)
  ivmod <- ivreg(y~educ+exper+expersq+partt|motheduc+exper+expersq+partt)
  
  ## calculate the predicted values and standard error from the OLS and IV regression model above
  expectOLS <- c(1,meaneduc,meanexper,meanexper^2,meanpartt)%*%linmod$coeff
  sdOLS <- sqrt(t(c(1,meaneduc,meanexper,meanexper^2,meanpartt)) %*% vcov(linmod) %*% c(1,meaneduc,meanexper,meanexper^2,meanpartt))
  expectIV <- c(1,meaneduc,meanexper,meanexper^2,meanpartt)%*%ivmod$coeff
  sdIV <- sqrt(t(c(1,meaneduc,meanexper,meanexper^2,meanpartt)) %*% vcov(ivmod) %*% c(1,meaneduc,meanexper,meanexper^2,meanpartt))
  
  ##############################################################
  ##############################################################
  ##############################################################
  ## Probit model
  thresholds <- sort(unique(y))  ## identify the different income levels
  n <- length(thresholds)  ## identify the total number of the income levels
  u <- seq(0.01,0.99,by=0.01)
  lengththresholds <- length(thresholds)
  
  ## create variables to save the coefficients from the probit without IV and with IV
  resultsnonIV <- matrix(0, ncol = 5, nrow = length(thresholds))
  yvaluesIV <- rep(0,length(thresholds))
  
  ## run the probit models
  for(i in 1:lengththresholds){
    dependent <- ifelse(y > thresholds[i], 0, 1) 
    ## without IV
    mod <- glm(dependent ~ educ+exper+expersq+partt, family = binomial(link = "probit"))$coeff ## run the probit model and identify the coefficients
    resultsnonIV[i, ] <- mod ## save the probit coefficients without IV 
    ## with IV
    dependent <- y <= thresholds[i]  ##create dummy varaible for lwage that is smaller or equal to the threshold
    Y2hatmodel <- lm(educ~exper+expersq+motheduc+partt)  ## run the first stage of IV regression
    Y2hat <- Y2hatmodel$residuals ## prepare residuals for second stage
    betahat <- glm(dependent~exper+expersq+educ+Y2hat+partt,family=binomial(link="probit"))$coeff # These are the desired estimators divided by sqrt(1-rho^2).
    func <- mean(sapply(1:nn,function(a) pnorm(betahat[1]+meanexper*betahat[2]+meanexper^2*betahat[3]+meaneduc*betahat[4]+Y2hat[a]*betahat[5]+meanpartt*betahat[6])))
    yvaluesIV[i] <- func ## expected quantile of log wage from probit with IV given education and experience at 90th quantile
  }
  
  yvaluesnonIV <- sapply(1:n,function(x) pnorm(resultsnonIV[x,1] + resultsnonIV[x,2]*meaneduc + resultsnonIV[x,3]*meanexper + resultsnonIV[x,4]*meanexper^2+resultsnonIV[x,5]*meanpartt)) ## expected quantile of log income from probit without IV
  
  ## Isotonic regression
  regressionnonIV <- isoreg(thresholds,yvaluesnonIV)  ## isoregession without IV
  regressionIV <- isoreg(thresholds,yvaluesIV)    ## isoregression with IV
  
  # Monotonic Rearrangement
  u <- seq(0.01,0.99,by=0.01)
  Qhat <- sapply(u,function(x) thresholds[min(lengththresholds,min(which(yvaluesnonIV >= x)))])  ## match every quantile with the income thresholds, for probit without IV 
  rearrangement <- sapply(thresholds,function(x) mean(Qhat <= x,na.rm=T))  ## the percentage of quantile that is below the threshold x, given the corresponding lwage values  
  Qhat <- sapply(u,function(x) thresholds[min(lengththresholds,min(which(yvaluesIV >= x)))]) ## match every quantile with the income thresholds, for probit with IV
  rearrangementIV <- sapply(thresholds,function(x) mean(Qhat <= x,na.rm=T)) 
  
  ## save the figures
  ## probit with IV vs without IV, without monotonization
  pdf(file=paste("Plot",q_t[q],"WithoutMonotonization.pdf"),width=9,height=6)
  plot(regressionIV$x,yvaluesIV,lwd=2,type="l",xlab="log (income)",cex=1.5,cex.axis=1.5,cex.lab=1.5,cex.main=1.5,ylab="Estimated CDF", ylim=c(0,1),col = "darkred")
  points(regressionnonIV$x,yvaluesnonIV,lwd=2,type="l",col = "black")
  legend("bottomright", c("Without IV","With IV"),lwd=2, col=c("black","darkred"))
  dev.off()
  
  ## probit with IV vs without IV, with monotonization
  pdf(file=paste("Plot",q_t[q],".pdf"),width=9,height=6)
  plot(regressionnonIV$x,regressionnonIV$yf,lwd=2,type="l",xlab="log (income)",cex=1.5,cex.axis=1.5,cex.lab=1.5,cex.main=1.5,ylab="Estimated CDF", ylim=c(0,1))
  points(regressionIV$x,regressionIV$yf,lwd=2,type="l",col="darkred")
  legend("bottomright", c("Without IV","With IV"),lwd=2, col=c("black","darkred"))
  dev.off()
  
  ## probit with IV vs without IV, with rearrangement
  pdf(file=paste("PlotRearrangement",q_t[q],".pdf"),width=9,height=6)
  plot(regressionnonIV$x,rearrangement,lwd=2,type="l",xlab="log (income)",cex=1.5,cex.axis=1.5,cex.lab=1.5,cex.main=1.5,ylab="Estimated CDF", ylim=c(0,1))
  points(regressionIV$x,rearrangementIV,lwd=2,type="l",col="darkred")
  legend("bottomright", c("Without IV","With IV"),lwd=2, col=c("black","darkred"))
  dev.off()
  
  
  
  # Confidence interval, by bootstrapping
  B <- 2 ## bootstrap times
  
  thresholdsBS <- matrix(0,ncol=B,nrow=n)
  thresholdsIVBS <- matrix(0,ncol=B,nrow=n)
  differenceBS <- matrix(0,ncol=B,nrow=n)
  lengththresholds <- length(thresholds)
  
  ## run bootstrap for the standard error
  for(b in 1:B)
  {
    print(b)
    set.seed(b)
    ## save the variables
    samplevalues <- sample(1:nn,replace=T) ## indicators of new sample
    yBS <- y[samplevalues]  ## lwage in new sample
    educBS <- educ[samplevalues]  ## edcation in new sample
    experBS <- exper[samplevalues]  ##  experience in new sample
    expersqBS <- experBS^2
    motheducBS <- motheduc[samplevalues] ##  mother education in new sample
    parttBS <- partt[samplevalues]
    ## create variable to save the results
    results <- matrix(0, ncol = 5, nrow = length(thresholds)) ## save coefficients from probit model without IV
    yvaluesIV <- rep(0,length(thresholds))  ## save expected values from IV model
    rownames(results) <- thresholds
    for(i in seq_along(thresholds)){
      ziel <- ifelse(yBS > thresholds[i], 0, 1)
      ## without IV
      mod <- glm(ziel ~ educBS+experBS+expersqBS+parttBS, family = binomial(link = "probit"))$coeff
      results[i, ] <- mod
      Y2hatmodel <- lm(educBS~experBS+expersqBS+motheducBS+parttBS)
      Y2hat <- Y2hatmodel$residuals
      betahat <- glm(ziel~experBS+expersqBS+educBS+Y2hat+parttBS,family=binomial(link="probit"))$coeff # These are the desired estimators divided by sqrt(1-rho^2).
      func <- mean(sapply(1:nn,function(a) pnorm(betahat[1]+meanexper*betahat[2]+meanexper^2*betahat[3]+meaneduc*betahat[4]+Y2hat[a]*betahat[5]+meanpartt*betahat[6])))
      yvaluesIV[i] <- func
      
    }
    yvalues <- sapply(1:n,function(x) pnorm(results[x,1] + results[x,2]*meaneduc + results[x,3]*meanexper + results[x,4]*meanexper^2+results[x,5]*meanpartt))
    
    regr1 <- isoreg(thresholds,yvalues)
    regr2 <- isoreg(thresholds,yvaluesIV)
    yvalues
    
    thresholdsBS[,b] <- regr1$yf  ## fitted value of isoregression without IV
    thresholdsIVBS[,b] <- regr2$yf  ## fitted value of isoregression with IV
    differenceBS[,b] <- regr1$yf - regr2$yf
    
    
  }
  ## calculate the confidence interval
  thresholds095 <- sapply(1:n,function(x) quantile(thresholdsBS[x,],0.9))
  thresholds005 <- sapply(1:n,function(x) quantile(thresholdsBS[x,],0.1))
  thresholdsIV095 <- sapply(1:n,function(x) quantile(thresholdsIVBS[x,],0.9))
  thresholdsIV005 <- sapply(1:n,function(x) quantile(thresholdsIVBS[x,],0.1))
  differenceBS095 <- sapply(1:n,function(x) quantile(differenceBS[x,],0.95))
  differenceBS005 <- sapply(1:n,function(x) quantile(differenceBS[x,],0.05))
  
  
  ## difference between estimated conditional distributionfunctions and confidence bounds
  ## with isotonic regression
  pdf(file=paste("Plot",q_t[q],"differenceBS.pdf"),width=9,height=6)
  plot(regressionnonIV$x,regressionnonIV$yf-regressionIV$yf,lwd=2,type="l",xlab="log (income)",cex=1.5,cex.axis=1.5,cex.lab=1.5,cex.main=1.5,ylab="Difference in Estimated CDF", ylim=y_axis[q,])
  points(regressionnonIV$x,differenceBS095,lwd=2,type="l",lty=2)
  points(regressionnonIV$x,differenceBS005,lwd=2,type="l",lty=2)
  dev.off()
  
  ## with monotonic rearrangement 
  pdf(file=paste("Plot",q_t[q],"differenceBSRearrangement.pdf"),width=9,height=6)
  plot(regressionnonIV$x,rearrangement-rearrangementIV,lwd=2,type="l",xlab="log (income)",cex=1.5,cex.axis=1.5,cex.lab=1.5,cex.main=1.5,ylab="Difference in Estimated CDF", ylim=y_axis[q,])
  points(regressionnonIV$x,differenceBS095,lwd=2,type="l",lty=2)
  points(regressionnonIV$x,differenceBS005,lwd=2,type="l",lty=2)
  dev.off()
  
}