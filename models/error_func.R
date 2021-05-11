
error_func <- function(yorig, ypred){
  ## Funzione che calcola la bontà del modello in termini di concordanza nelle derivate
  
  # yorig vettore di valori della funzione f0
  # ypred vettore di valori previsti dal modello nello stesso ordine di yorig
  require(features)
  
  yorig <- matrix(yorig, ncol = 20, byrow = T)
  ypred <- matrix(ypred, ncol = 20, byrow = T)
  
 # calcolo le derivate sui valori originali  
 deriv_orig <- apply(yorig,
                    1,
                    function(z) attr(features(x = 1:20, y = z ), "fits")$d1 )
 # calcolo le derivete sui valori predetti
 deriv_pred <- apply(ypred,
                    1,
                    function(z) attr(features(x = 1:20, y = z ), "fits")$d1 )
 
 # calcolo dell'errore
 MSE <- apply((deriv_orig - derivate_pred)^2, 2, sum)
 denum <- apply(sign(deriv_orig) == sign(deriv_pred), 2, mean)
 return(mean(MSE/denum))
}


