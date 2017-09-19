funSenSpePPVN<-function(TP=0,TN=0,FP=0,FN=0,n,alpha=0.05){
  if(sum(TP,TN,FP,FN,na.rm=T)==n){
    
    Sen<-TP*100/(TP+FN)
    Spe<-TN*100/(TN+FP)
    
    PPV<-(TP)*100/(TP+FP)
    NPV<-(TN)*100/(TN+FN)
    
    val<-rbind(Sen,Spe,PPV,NPV)
    
    Calc<-data.frame(Name=row.names(val),Ind=val)
    z<-qnorm(p = alpha/2,lower.tail=F)
    DiagVal<-Calc%>%mutate(Ind=round(Ind,1),inf=round(Ind-(z*sqrt(Ind*(100-Ind)/n)),1),sup=round(Ind+(z*sqrt(Ind*(100-Ind)/n)),1),IC=paste(inf,"to",sup));DiagVal
    
    GV <-(TP+TN)/(TP+FN+FP+TN)
    a  <-(TP+FN)/n
    b  <-(TP+FP)/n
    Pe1<-(a*b)
    Pe2<-(1-a)*(1-b)
    Pe <-Pe1+Pe2
    kappa<-(GV-Pe)/(1-Pe)
    
    Lp <-Sen/(100-Spe)
    Ln <-(100-Sen)/Spe
    Lik<-rbind(Lp,Ln)
  
  }else{
    
    print("Wrong Values")
    
  }
  print(list(Indicators=DiagVal,GlobalVal=round(GV*100,1),kappa=round(kappa,2),Likelihood=round(Lik,2)))
}
