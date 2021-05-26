
error_func <- function(yorig, ypred){
  ## Funzione che calcola la bontà del modello in termini di concordanza nelle derivate
  
  # yorig vettore di valori della funzione f0
  # ypred vettore di valori previsti dal modello nello stesso ordine di yorig
  # require(features)
  require(pracma)
  
  yorig <- matrix(yorig, ncol = 20, byrow = T)
  ypred <- matrix(ypred, ncol = 20, byrow = T)
  
 # calcolo le derivate sui valori originali  
 deriv_orig <- t(apply(yorig,
                    1,
                    function(row) pracma::gradient(row)))
 # calcolo le derivete sui valori predetti
 deriv_pred <- t(apply(ypred,
                    1,
                    function(row) pracma::gradient(row)))
 
 # calcolo dell'errore
 MSE <- mean((deriv_orig - deriv_pred)^2)
 print(MSE)
 denum <- mean(sign(deriv_orig) == sign(deriv_pred))
 print(denum)
 return(MSE/denum)
}


