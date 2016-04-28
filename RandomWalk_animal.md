# Animal-moving
Algorithm for traking moving animal
#############################################################################################################################
HR<-function(XY, perm,ANG,dista, dismax, ncoord)
{

tablaG<-list()

for(j in 1:perm)
{

TablaXY=as.data.frame(matrix(nrow=1000, ncol=2)) 
colnames(TablaXY)<-c('CoordX', 'CoordY')
cont=sample(dista, 1)              
Xini<-XY[cont,1]                   
Yini<-XY[cont,2]


i=1                               
repeat
{
           
 A<-c()
 Dis<-c()
 X<-c()
 Y<-c()
 X1<-c()
 Y1<-c()
       X<-Xini
       Y<-Yini

       A=sample(ANG, 1)
       Dis=sample(dista,1)

       X1<-X+cos(A)*Dis
       Y1<-Y+sin(A)*Dis
       TablaXY[i,1]<-X1
       TablaXY[i,2]<-Y1

       Xini<-X1
       Yini<-Y1
         i=i+1
         B=4
         distM<-max(na.omit(matrix(dist(TablaXY))))
          if(is.na(distM))
          {
          esmenor<-(10>dismax)

          }
          else{
          esmenor<-(distM>dismax)
           }

          if(esmenor || i>ncoord) break

}
NI=NI+1
coordinates(TablaXY)<-~CoordX+CoordY
tablaG[[j]]<-TablaXY
}
return(tablaG)
}
