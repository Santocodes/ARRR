nummetrics <-
  function(a,m)
  {  
    metrics= c(MAD=0,MSE=0,MAPE=0,MPSE=0,R2=0,TMAD=0,P90=0)
    metrics["MAD"]=mean(abs(a-m))
    metrics["MSE"]=mean((a-m)^2)
    metrics["MAPE"]=mean((abs(a-m)/a))
    metrics["MPSE"]= mean(((a-m)/a)^2)
    SST= sum((a-mean(a))^2)
    SSE= sum((a-m)^2)
    metrics["R2"]=1-(SSE/SST)
    metrics["TMAD"]= mean(abs(a-m),trim=0.05)
    metrics["P90"]=quantile(abs(a-m),probs=0.9)
    return(metrics)
  }
binmetrics <-
  function(a,m,k=10)
  {
    metrics=c(LL=0,AIC=0,BIC=0,R2=0)
    metrics["LL"]=sum(ifelse(a==1,log(m),log(1-m)))
    metrics["AIC"]=-2*metrics["LL"]+2*k
    metrics["BIC"]=-2*metrics["LL"]+2*k*log(length(a))
    SST= sum((a-mean(a))^2)
    SSE= sum((a-m)^2)
    metrics["R2"]=1-(SSE/SST)
    return(metrics)
  }