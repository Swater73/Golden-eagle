library(RMark)
library(tidyverse)
library(gdata)

#####Tirage constant


{T1<-Sys.time()
  annees<-c(5,10,15,20,30)
  couples<-c(5,10,15,20,30)
  survie<-c(0.85,0.875,0.9,0.925)
  puissance_0.85<-table(annees,couples)
  puissance_0.875<-table(annees,couples)
  puissance_0.9<-table(annees,couples)
  puissance_0.925<-table(annees,couples)
  s_estim_tot<-list()
  p_estim_tot<-list()
  
  for (h in 1:length(survie)) {
    for (z in 1:length(annees)) {
      for (v in 1:length(couples)) {
        
        N<-couples[v] # nombre de territoires suivis
        supp<-N*2 #pour avoir assez d'individus en plus
        s<-survie[h] # proba de survie
        K<-annees[z] # nombre d'ann?es de suivi
        bo<-1000 # Nombre de boucle
        
        s_estim<-NULL #pour stocker proba survie estim?e
        p_estim<-NULL # pour stocker proba capture estim?e
        
        for (boot in 1:bo) # boucle pour 1000 tirages
        {
          l<-0
          Etat<-matrix(0,nrow=(N+supp),ncol=K) # matrice d'?tat des individus vide
          Etat[1:N,1]<-1 # premi?re ann?e pour les suivis
          
          for (i in 1:N) # boucler sur les N individus suivis
          {
            for (j in 2:K) # boucler sur les ann?es
            {
              real<-rbinom(1,1,s) # tirage pour survie ou non
              Etat[i,j]<-Etat[i,(j-1)]*real   # multiplication par etat pr?c?dent
              if (real==0&Etat[i,(j-1)]==1)   # si mort alors que vivant avant, on cr?e un nouvel individu
              {
                l<-l+1
                Etat[N+l,j]<-1
              }
            }
          }
          
          # ci-dessous on g?re tous les individus nouvellement cr?es
          if(sum(Etat[N+1,])>0){
            for (i in (N+1):(N+l))
            {
              first<-which(Etat[i,]==1) # quand apparition de l'individu
              if (first<K)
                for (j in (first+1):K) # survie ou non apr?s apparition 
                {
                  real<-rbinom(1,1,s)
                  Etat[i,j]<-Etat[i,(j-1)]*real
                }
            }}
          Etat<-Etat[1:(N+l),] # ne garder que les individus avec des donn?es
          
          hist<-Etat
          for (i in 1:(N+l))
          {	
            hist[i,]<-Etat[i,]*rbinom(K,1,0.8) # tirer al?atoirement les taux de capture 
          }
          hist<-hist[rowSums(hist)>0,]
          
          histp <- data.frame(ch = unite(as.data.frame(hist),col = "ch",sep = ""))
          phi.p<-mark(histp) # mod?le CJS
          
          s_estim<-c(s_estim,phi.p$results$real[1,4]) # r?cup estimation survie
          p_estim<-c(p_estim,phi.p$results$real[2,1])
          
          w<-0
          for (i in 1:length(s_estim)) {
            if(s_estim[i]<0.95){w<-w+1}
          }
          w<-w/bo
        }
        s_estim_tot<-append(s_estim_tot,list(s_estim))
        p_estim_tot<-append(p_estim_tot,list(p_estim))
        
        if(h==1){puissance_0.85[z,v]<-w}
        else if(h==2){puissance_0.875[z,v]<-w}
        else if(h==3){puissance_0.9[z,v]<-w}
        else{puissance_0.925[z,v]<-w}
        
        cleanup(ask = FALSE)
      }
    }
  }
  rm(h,survie,s_estim,w,p_estim,bo,first,i,IC,Etat,hist,histp,phi.p,Biais,boot,first,i,IC,j,K,l,N,p,quant,real,s,supp,proba,v,z,annees,couples)
  save.image("C:/Users/33651/Desktop/Stage Aigle/Script R/resume_constant.RData")
  T2<-Sys.time()
  difftime(T2,T1)
}

