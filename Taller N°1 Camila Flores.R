
#Nombre: Camila Flores Morales 
#RUT: 19.805.999-4


 #===================================================


  #Ayuda en linea 

  
   ?lm
   help.start()

 #introduccion a R

 #2.1 Empezar y cerrar una sesion de R 

 getwd()

 #2.2 Aritmetica Basica

  14+3+2  #suma
  2-4-3   #resta
  2*3*5   #multiplicacion
  2/3     #division 
  ((2+7)-(2*3))/3
  
#2.3 asignacion de valores 
  
  x<-2 
  y<-3
  z= x + y
  print(z)
  
#2.4 valores logicos
  
  x>3
  x<=5
  
#2.5 Gestion de vectores
  
  #2.5.1 creacion de vectores 
  
   x<-c(2,3,5,6,7,11)

  #2.5.2 secuencias numericas 
  
   xx<- 1:4
   xx
   xx<- 50:1
   xx
   xx<- seq(from=60, to = 1)
   xx
   x<- 1:10
   x*3
   x
  
  #2.5.3 Extraccion de elements de un vector 
  
   xx[7]
   length(xx)
   max(xx)
   min(xx)
   sum(xx)
   prod(xx)
   sort(xx)
   sort.list(xx)

 #2.6 Elementos  de programacion en R
  
    doble<-function(x){
    y<-x^2
    return(y)
   }
   doble(x)
 
  #media aritmetica
   #forma 1
   x<- c(2,3,4,5) #creamos un vector de 4 elementos 
   sum(x)  #suma los elementos del vector
   mean(x) #media aritmetica del vector creado 
   median(x)
   #forma 2
   media<-function(x){
     sum(x)/length(x)
   }
   mean(x)
   
# Estadisticca 
   
 datos<- c(1,3,4,5,6,5)

   # mediana
 
   ordenDatos=sort(datos)
   largo=length(ordenDatos)
  if (largo%%2==0){
    mediana=(ordenDatos[((largo/2)+1)] + ordenDatos[((largo/2))])/ 2
  
  }else {
   mediana= ordenDatos[((largo/2)+1)]
  
  }

  print (mediana)

 #media aritmetica 
  
  suma=0
  for (i in datos) {
  suma= suma +i 
  }
  n= length(datos)
  promedio=suma/n
  print(promedio)

 #Modaa
  
  moda <-function(datos){
    ux<- unique(datos) #unique extrae los elementos 
    ux[which.max(tabulate(match(datos,ux)))] #indice del valor max 
  }
  print(moda(datos))


  #varianza
  obtenerVarianza= function(datos){
   n= length(datos)
   varianza= 0
   for (dato in 1:length(datos)){
     varianza=(varianza+(datos[dato] - mean(datos))^2)
   }
   return(varianza/(n-1))
  }

  print (obtenerVarianza(datos))

  # desviacion estandar 
   
   desviacion = function(datos){
     varianza= obtenerVarianza(datos)
     return((varianza)^(1/2))
  
   }

   print (desviacion(datos))

 # Maximo 
  
   maximo= 0 
   for (i in datos) {
     if (i > maximo)
       maximo = i
  
   }
   print (maximo)

 #Minimo

  orden=sort(datos)
  minimo=orden[c(1)]
  print (minimo)

 #rango
  
  rango= maximo - minimo 
  print (rango)

 #cuartiles 

  #cuartil 1 = P25 
    cuartil1 = function(datos){
      i= 0.25 * (n+1)
      b = floor(i)
      c = i-b 
      resultado= orden[b]* (1-c)+ (c*orden[b+1])
      return(resultado)
    }
    print (cuartil1(datos))

  #cuartil 2 = P50 = mediana
    cuartil2 = function(datos){
      i= 0.50 * (n+1)
      b= floor(i)
      c= i-b
      resultado= orden[b] * (1-c) + (c* orden[b+1])
      return(resultado)
    }
  print(cuartil2(datos))

   #cuartil 3 = P75
     cuartil3 = function(datos){
      i= 0.75 * (n+1)
      b= floor(i)
      c= i-b
      resultado= orden[b] * (1-c) + (c* orden[b+1])
      return(resultado)
     } 
    print (cuartil3(datos))


 # Rango intercuartil 
   Intercuartil = function(datos){
     a= cuartil3(datos)
     b= cuartil1(datos)
     rango= a - b
     return(rango)
   }
   
   print (Intercuartil(datos))
   
 #Metodo de las barreras de Tukey
   #Barrera interior inferior 
   #Barrera interior superior 
   
   tukey = function(datos){
     BII = cuartil1(orden) - (1.5 * Intercuartil(orden))
     BIS = cuartil3(orden) +(1.5 * Intercuartil(orden))
     return(c(BII,BIS))
     
   }

  tukey(datos)
   
   ## Barrera exterior inferior
   ## Barrera exterior superior 
   
   BTukey = function(datos){
     BEI = cuartil1(orden) - (3 * Intercuartil(orden))
     BES = cuartil3(orden) +(3 * Intercuartil(orden))
     return(c(BEI,BES))
   }
   
   print (BTukey(datos))
   
   #Coeficiente de Asimetria o sesgo 
   
   CS= function(datos){
     n= length(datos)
     asimetria= 0
     for (dato in 1:length(datos)){
       asimetria=(asimetria+((datos[dato] - mean(datos))^3)/((desviacion(datos))^3))
     }
     return(asimetria)
   }
   
   print(CS(datos))
   
   
   #Coeficiente de curtosis 
   
   CK= function(datos){
     n= length(datos)
     curtosis= 0
     for (dato in 1:length(datos)){
       curtosis=(curtosis+((datos[dato] - mean(datos))^4)/((desviacion(datos))^4))
     }
     return(curtosis)
   }
   print (CK(datos)) 
   
FuncionExplorar = function (datos){
     print (data.frame(mediana))
     print(data.frame(promedio))
     print(data.frame(moda(datos)))
     print (data.frame(obtenerVarianza(datos)))
     print (data.frame(desviacion(datos)))
     print (data.frame(maximo))
     print (data.frame(minimo))
     print (data.frame(rango))
     print (data.frame(cuartil1(datos)))
     print (data.frame(cuartil2(datos)))
     print (data.frame(cuartil3(datos)))
     print (data.frame(Intercuartil(datos)))
     print (data.frame(tukey(datos)))
     print (data.frame(BTukey(datos)))
     print(data.frame(CS(datos)))
     print (data.frame(CK(datos))) 
   }
  
print(FuncionExplorar(datos))  
   
 #2.7 {Matrices}
  #suma de matrices 
    
    A<- matrix(1:6,2,3)
    B<- matrix(2:7,2,3)
    A+B
  
  #multiplicacion de matrices
   
     A<-matrix(1:3,3,1)
    B<-matrix(3:1,3,1)
    A*B

  #determinante de una matriz 
    
    A <- A<- matrix(1:4,2,2)
    det(A)

  #matriz transpuesta 
    
    A<- matrix(1:8,4,2)
    t(A)

 #diagonal de una matriz
    
    A <- A<- matrix(1:6,3,3)
    diag(A)

    

   
    
      

