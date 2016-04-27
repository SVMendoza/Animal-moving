# Animal-moving
#Algorithm for dispersal seed by monkey
#trial and development.

#####################################################################################################################





Traj_monkeys<-function(XYini, Vtray,Atray, Tcam, Tcom, Tdesc,Tdig, XYseed, perm, Tsimul,radio, Tday, Tnuit, directorio, file) 
{
setwd(directorio)
TablaG<-list()
TablaG1<-list()
BW<-as.data.frame(matrix(nrow=perm, ncol=2))
Distmax<-as.data.frame(matrix(nrow=perm, ncol=2))
Tabla=list()
colnames(BW)<-c('ID','ECM')
colnames(Distmax)<-c('ID','DMax')

for(j in 1:perm) ##número de ciclos
  {
    TablaXY=as.data.frame(matrix(nrow=1000, ncol=5))
    colnames(TablaXY)<-c('ID','CoordX', 'CoordY','Iseed', 'Tdiges')

    TSeed<-as.data.frame(matrix(nrow=1000, ncol=5))
    colnames(TSeed)<-c('CoordX', 'CoordY','DistExc', 'Seed_Exc','Letrinas')

    Xini<-c()
    Yini<-c()
    Vt<-c()

    FILAS<-c(1:length(XYini[,1]))
    cont=sample(FILAS, 1,replace =T) ##vector con probabilidades para iniciar las trayectorias o seleccionar las coordenadas de inicio
    Xini<-XYini[cont,1] #Coordenada X de inicio de la trayectoria
    Yini<-XYini[cont,2] #Coordenada Y de inicio de la trayectoria
    Dis<-c() #Distancia de desplazamiento
    X1<-c() #posición X de la trayectoria
    Y1<-c() #posición Y de la trayectoria
    A<-c()  #Angulo
    Per<-c() #periodo logico
    TDig<-c() ##estimando el tiempo de digestión
    l<-c() # distancia de semilla a la posición del mono
    l1<-c() # número de semillas ingeridas por el mono
    Td<-c() #tiempo del día
    Tc<-c() #tiempo de comer
    Tdesc1<-c()# tiempo de descanso extraído  por el sample
    Tca<-c() #Tiempo de caminata
    Tpar<-c()
    Tres<-c() #tiempo restante del recorrido en el día
              Vt<-sample(as.vector(Vtray), 1,replace =T) ##  extrayendo la velocidad de trayectoria
              A<-sample(as.vector(Atray), 1,replace =T)   #extrayendo el ángulo de la trayectoria
              Tca<-sample(as.vector(Tcam), 1,replace =T)   # Extrayendo el tiempo de caminata
              Dis<-Vt*Tca        #calculando la distancia en función del tiempo y la velocidad
              X1<-Xini+cos(A)*Dis  #generando las coordenadas en posición final del desplazamiento
              Y1<-Yini+sin(A)*Dis
              Td<-Tca
   Per1<-Tca
    Tpar<-1#Tiempo de parada
    TDig<-0
    index<-c()
    i<-1
   Per<-Per1<Tsimul  # valor lógico de parada para el bucle que se genera por día
    while(Per) # ciclo del día
        {
                 TablaXY[i,2]<-X1
                  TablaXY[i,3]<-Y1
                  Tc<-sample(as.vector(Tcom), 1, replace =T)##extrayendo el tiempo de comer
                  Td<-Td+Tc  #estimando el tiempo del día que se ha realizado
                  Per1<-Per1+Tc
                                           Dseed<-sqrt((TablaXY[i,2]-XYseed[,1])^2+(TablaXY[i,3]-XYseed[,2])^2) 
                                            if(min(Dseed)>100){
                                             #SI HAY SALIDA DEL AREA DE ESTUDIO
                                                X1<-TablaXY[i,2]+cos(A)*(1.5*-Dis)                                                 Y1<-TablaXY[i,3]+sin(A)*(1.5*-Dis)
                                                TablaXY[i,2]<-X1
                                                TablaXY[i,3]<-Y1
                                                Dseed<-sqrt((TablaXY[i,2]-XYseed[,1])^2+(TablaXY[i,3]-XYseed[,2])^2) 
                                                
                                              l<-XYseed[Dseed<=radio,3]
                                                   if(min(Dseed)>100){
                                             #SI HAY SALIDA DEL AREA DE ESTUDIO
                                                X1<-TablaXY[i,2]+cos(A)*(1.5*-Dis)  
                                                Y1<-TablaXY[i,3]+sin(A)*(1.5*-Dis)
                                                TablaXY[i,2]<-X1
                                                TablaXY[i,3]<-Y1
                                                Dseed<-sqrt((TablaXY[i,2]-XYseed[,1])^2+(TablaXY[i,3]-XYseed[,2])^2)            l<-XYseed[Dseed<=radio,3]
                                                                     }
                                                      else {

                                                l<-XYseed[Dseed<=radio,3]
                                                            }
                                                                }

                                            l<-XYseed[Dseed<=radio,3]      

                                            if(is.null(l1))
                                               {

                                                          l1<- sum(na.omit(l)/length(l)) 
                                               TDig<-sample(as.vector(Tdig),1,replace =T)                                                Tpar<-Td+TDig
                                               index<-i
                                               TablaXY[i,1]<-index
                                               TablaXY[i,2]<-X1
                                               TablaXY[i,3]<-Y1
                                               TablaXY[i,4]<-l1
                                               TablaXY[i,5]<-TDig
                                               l<-c()
                                                }
                                     if(l1==0){l1<-c()}



                           if(!is.null(l1))
                           {
                            if(!is.na(l1))# pregunta si el tiempo de digestión es menor que el periodo del día
                                             {
                                    if((Tpar<Tday)){
 
                                        Tres<-TDig>1
                                        p<-i
                                   while(Tres){
                                              Tdesc1<-sample(as.vector(Tdesc),1,replace =T)
                                              Par<-c(); Td<-Td+Tdesc1; Per1<-Per1+Tdesc1
                                              Par<-Tpar>Td
                                              if(Par){
                                                    TSeed[p,1]<-X1
                                                    TSeed[p,2]<-Y1
                                                    TSeed[p,3]<-sqrt((TablaXY[index,2]-(X1+cos(A)*(Dis)))^2+(TablaXY[index,3]-(Y1+sin(A)*(Dis)))^2)
                                                    TSeed[p,4]<-TablaXY[index,4]
                                                    TSeed[p,5]<-c('N')
                                                                  l1<-c()
                                                                  index=c()
                                                                  Tpar<-c()
                                                                  Tres<-FALSE
                                                     }
                                              else{Tca<-sample(as.vector(Tcam), 1,replace =T)
                                                    Td<-Td+Tca; Par<-Tpar>Td  ; Per1<-Per1+Tca
                                                    Vt<-sample(as.vector(Vtray), 1,replace =T)
                                                    A<-sample(as.vector(Atray), 1,replace =T)
                                                    Dis<-Vt*Tca
                                                    X1<-X1+cos(A)*Dis
                                                    Y1<-Y1+sin(A)*Dis
                                                    TablaXY[p,2]<-X1
                                                    TablaXY[p,3]<-Y1
                                                if(Par){
                                                   TSeed[p,1]<-X1
                                                   TSeed[p,2]<-Y1
                                                   TSeed[p,3]<-sqrt((X1-TablaXY[index,2])^2+(Y1-TablaXY[index,3])^2)
                                                   TSeed[p,4]<-TablaXY[index,4]
                                                   TSeed[p,5]<-c('N')
                                                                  l1<-c()
                                                                  index=c()
                                                                  Tpar<-c()
                                                                  Tres<-FALSE

                                                       }else {Tc<-sample(as.vector(Tcom), 1, replace =T)
                                                     Td<-Td+Tc
                                                     Per1<-Per1+Td
                                                     Par<-c()
                                                     Par<-Tpar>Td

                                                     if(Par){
                                                     TSeed[p,1]<-X1
                                                     TSeed[p,2]<-Y1
                                                     TSeed[p,3]<-sqrt((TSeed[p,1]-TablaXY[index,2])^2+(TSeed[p,2]-TablaXY[index,3])^2)
                                                     TSeed[p,4]<-TablaXY[index,4]
                                                     TSeed[p,5]<-c('N')
                                                                     l1<-c()
                                                                     index=c()
                                                                     Tpar<-c()
                                                                     Tres<-FALSE



                                                    }

                                              }
                                               }
                                     p<-p+1
                                   }
                              }
                              }
                              }
     if(Td>Tday)
                {
                Per1<-Per1+Tnuit
             if(!is.null(l1))
             {Tpar<-abs(Tpar-Tnuit)
            if(Tpar<Tnuit){
                if(!is.na(l1)) {
                              TSeed[i,1]<-X1
                              TSeed[i,2]<-Y1
                              TSeed[i,3]<-sqrt((TSeed[i,1]-TablaXY[index,2])^2+(TSeed[i,2]-TablaXY[index,3])^2)
                              TSeed[i,4]<-l1
                              TSeed[i,5]<-c('S')
                              Td<-1
                              index<-c()
                              Tpar<-c()
                              l1<-c()
                                }
             }
            else {Tpar<-Tpar
                 Td<-1
                        }
               }
                 Td<-1
                }



Tdesc1<-sample(as.vector(Tdesc),1,replace =T)#descansa
Td<-Td+Tdesc1 ; Per1<-Per1+Tdesc1

      if(Td>Tday)
                {
                Per1<-Per1+Tnuit
             if(!is.null(l1))
             {Tpar<-abs(Tpar-Tnuit)
            if(Tpar<Tnuit){
                if(!is.na(l1)) {
                              TSeed[i,1]<-X1
                              TSeed[i,2]<-Y1
                              TSeed[i,3]<-sqrt((TSeed[i,1]-TablaXY[index,2])^2+(TSeed[i,2]-TablaXY[index,3])^2)
                              TSeed[i,4]<-TablaXY[index,4]
                              TSeed[i,5]<-c('S')
                              Td<-1
                              index<-c()
                              Tpar<-c()
                              l1<-c()
                                }
             }
               else {Tpar<-Tpar; Td<-1}
               }
                Td<-1
               }



       Tca<-sample(as.vector(Tcam), 1,replace =T) #Camina
       Td<-Td+Tca; Per1<-Per1+Tca
       Vt<-sample(as.vector(Vtray), 1,replace =T)
       A<-sample(as.vector(Atray), 1,replace =T)
       Dis<-Vt*Tca
       X1<-TablaXY[i,2]+cos(A)*Dis
       Y1<-TablaXY[i,3]+sin(A)*Dis

      if(Td>Tday)
                {
                Per1<-Per1+Tnuit
             if(!is.null(l1))
             {Tpar<-abs(Tpar-Tnuit)
            if(Tpar<Tnuit){
                if(!is.na(l1)) {
                              TSeed[i,1]<-X1
                              TSeed[i,2]<-Y1
                              TSeed[i,3]<-sqrt((TSeed[i,1]-TablaXY[index,2])^2+(TSeed[i,2]-TablaXY[index,3])^2)
                              TSeed[i,4]<-l1
                              TSeed[i,5]<-c('S')
                              Td<-1
                              index<-c()
                              Tpar<-c()
                              l1<-c()
                                }

            }
            else {Tpar<-Tpar; Td}
            }
                Td<-1
                }

i<-i+1
Per<-Tsimul>Per1
         }
Distmax[j,1]<-j
Distmax[j,2]<-max(na.omit(TSeed)$DistExc)

BW[j,1]<-j

if(length(na.omit(TSeed)$DistExc)>10){
BW[j,2]<-density((na.omit(TSeed)$DistExc), bw="ucv", width = length((na.omit(TSeed)$DistExc)), window = "gaussian")$bw
                                      }

#######################################################################
TablaG[[j]]<-list(XY=na.omit(TablaXY), SEED=na.omit(TSeed))
TablaG1[[j]]<-na.omit(TSeed)
     }

DM50=subset(Distmax,DMax>=quantile(na.omit(Distmax$DMax), 0.50))
DM50<-subset(DM50,DMax==min(DM50[,2]))
DM50<-DM50[,1]

DM75=subset(Distmax,DMax>=quantile(na.omit(Distmax$DMax), 0.75))
DM75<-subset(DM75,DMax==min(DM75[,2]))
DM75<-DM75[,1]

DM95=subset(Distmax,DMax>=quantile(na.omit(Distmax$DMax), 0.95))
DM95<-subset(DM95,DMax==min(DM95[,2]))
DM95<-DM95[,1]


V50=subset(BW,ECM>=quantile(na.omit(BW$ECM), 0.50))
V50<-subset(V50,ECM==min(V50[,2]))
V50<-V50[,1]

V75=subset(BW,ECM>=quantile(na.omit(BW$ECM), 0.75))
V75<-subset(V75,ECM==min(V75[,2]))
V75<-V75[,1]

V95=subset(BW,ECM>=quantile(na.omit(BW$ECM), 0.95))
V95<-subset(V95,ECM==min(V95[,2]))
V95<-V95[,1]

T_ecm<-rbind(TablaG1[[V50]], TablaG1[[V75]], TablaG1[[V95]])
write.table(T_ecm, "T_ecm.csv", sep=',')

T_dmax<-rbind(TablaG1[[DM50]], TablaG1[[DM75]], TablaG1[[DM95]])
write.table(T_dmax, "Tdmax.csv", sep=',')

Tabla<-list(TablaG1[[V50]], TablaG1[[V75]], TablaG1[[V95]],TablaG1[[DM50]], TablaG1[[DM75]], TablaG1[[DM95]])
names(Tabla)=c('ECMQ50','ECMQ75', 'ECMQ95', 'DmaxQ50','DmaxQ75','DmaxQ95')

pdf(file=paste(directorio,'Dist_frecuencias.pdf'),onefile=T)
par(mfrow=c(2,1))
hist(na.omit(BW$ECM), xlab = 'Bandwidth', ylab = "Frequency",probability = TRUE, main = "Gaussian kernel", border = "gray")
lines(density(na.omit(BW)$ ECM,bw="ucv", width = length(na.omit(BW)$ ECM), window = "gaussian"), lwd = 2)
hist(na.omit(Distmax$DMax), xlab = 'Bandwidth', ylab = "Frequency",probability = TRUE, main = "Gaussian kernel", border = "gray")
lines(density(na.omit(Distmax)$DMax,bw="ucv", width = length(na.omit(Distmax)$DMax), window = "gaussian"), lwd = 2)
dev.off()

Resultados<-list(Tabla, BW, Distmax,TablaG1)
names(Resultados)<-c('Dexc_Quantil', 'ECM', 'Dmax', 'dist_exc')
save(Resultados, file = file)
}
