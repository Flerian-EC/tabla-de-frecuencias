#Esta funcion retorna una tabla de distribucion de frecuencias en un data.frame
#puede retornar una tabla con intervalos, o tambien puede retornar una tabla sin intervalos
#los parametros de esta funcion son las siguientes:

#[vector]; Como su nombre indica, recive un vector de datos (de este vector se generara la tabla). el vector debe ser estrictamente numerico para tablas con intervalos. en otro caso puede ser una tabla de cadenas.
#[intervalos]; Recibe un numero entero. Este parametro indica la cantidad de intervalos que tendra la distribucion en la tabla (lo puede calcular con la regla de Sturges).
#[amplitud]; Recibe un numero entero. Este valor indica la amplitud de los intervalos en la tabla (lo puede calcular de la siguiente forma: redondeo superior de [  rango/sturges ])
#Si el parametro [amplitud] recibe el numero "0" la tabla de frecuencias no tendra intervalos (mostrara la frecuencia de cada variable independiente). y la funcion en general ignorara el parametro [intervalos]
#[acumulado]; Recibe un valor booleano (TRUE o FALSE) por defecto este parametro es "TRUE". añade a la tabla las frecuencias acumuladas (Fi , Hi). si el valos es "FALSE" no se añadiran frecuencias acumuladas. 

tablaDeFrecuencias = function (vector, intervalos, amplitud, acumulado = TRUE) {
  if(amplitud != 0) {
    x = sort(vector);
    r = x[length(x)] - x[1];
    k = intervalos;
    c = amplitud;
    
    condicionalUltimoElemento = (c * k) == r;
    diferencia = (c * k) - r;
    
    
    left = x[1]
    if(diferencia > 1 & diferencia %% 2 == 0) {
      left = left - (diferencia / 2)  
    }else if(diferencia > 1 & diferencia %% 2 != 0) {
      left = left - (ceiling(diferencia / 2) - 1)
    }
    
    right = left + c;
    
    
    intervalos = c();
    xi = c()
    fi = c();
    
    cadena = ""
    contador = 0;
    contadorVec = 1;
    condicion = TRUE;
    
    for (i in 1:k) {
      if(condicionalUltimoElemento & i == k) {
        cadena = paste("[", left, " , ", right, "]", sep = "");
      }else {
        cadena = paste("[", left, " , ", right, ")", sep = "");
      }
      intervalos = append(intervalos, cadena);
      
      xi = append(xi, (left + right) / 2);
      
      while (condicion) {
        
        if(x[contadorVec] >= left & x[contadorVec] < right & contadorVec < length(x) + 1) {
          contador = contador + 1;
          contadorVec = contadorVec + 1;
        }else {
          if(condicionalUltimoElemento & x[contadorVec] == x[length(x)] & contadorVec < length(x) + 1) {
            contador = contador + 1;
            contadorVec = contadorVec + 1;
          }else {
            condicion = FALSE;
            left = right;
            right = right + c;
          }
        }
      }
      
      fi = append(fi, contador);
      contador = 0;
      condicion = TRUE;
      
    }
    
    hi = fi / length(x);
    
    Fi = c();
    Hi = c();
    
    absolutaAcumulada = 0;
    relativaAcumulada = 0;
    
    for (i in 1:length(fi)) {
      absolutaAcumulada = absolutaAcumulada + fi[i];
      Fi = append(Fi, absolutaAcumulada);
      
      relativaAcumulada = relativaAcumulada + hi[i];
      Hi = append(Hi, relativaAcumulada);
    }
    
    frecuencias = 0;
    if(acumulado) {
      frecuencias = data.frame(intervalos, xi, fi, hi, Fi, Hi);
    }else {
      frecuencias = data.frame(intervalos, xi, fi, hi);
    }
    return(frecuencias);
  }else {
    
    x = sort(vector);
    
    Xi = c(x[1]);
    fi = c()
    
    comparar = x[1];
    
    for (i in 1:length(x)) {
      if(x[i] != comparar) {
        Xi = append(Xi, x[i])
        comparar = x[i]
      }
    }
    
    contador = 0;
    for (i in 1:length(Xi)) {
      
      for(j in 1:length(x)) {
        if(Xi[i] == x[j]) {
          contador = contador + 1
        }
      }
      
     fi = append(fi, contador);
     contador = 0;
    }
    
    hi = fi / length(x);
    
    Fi = c();
    Hi = c();
    
    absolutaAcumulada = 0;
    relativaAcumulada = 0;
    
    for (i in 1:length(fi)) {
      absolutaAcumulada = absolutaAcumulada + fi[i];
      Fi = append(Fi, absolutaAcumulada);
      
      relativaAcumulada = relativaAcumulada + hi[i];
      Hi = append(Hi, relativaAcumulada);
    }
    
    frecuencias = 0;
    if(acumulado) {
      frecuencias = data.frame(Xi, fi, hi, Fi, Hi);
    }else {
      frecuencias = data.frame(Xi, fi, hi);
    }
    return(frecuencias);
  }
}
