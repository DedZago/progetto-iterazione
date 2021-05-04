load("../data/mandarino.RData")
mandarino$current = ifelse(mandarino$time <= 10, mandarino$syllable1, mandarino$syllable2)
mandarino$current = levels(mandarino$syllable1)[mandarino$current]
mandarino %>%
  group_by_at(setdiff(colnames(mandarino), c("repetition", "f0"))) %>%
  summarize(f0 = mean(f0)) %>%
  ungroup() -> mandarino

### FACCIO ALCUNE PROVE SU LMER E GAMM4 (AGGIUNGENDO VARIABILI.. E BOH COSE COSI') ###

## (PDOR, FIGLIO DI K)LMER ##

#questo è quello di dede:
fitLmer = lmer(f0 ~ cog_load + time + current + time:current + syllable1 + syllable2 +
                 syllable1:syllable2 + time:syllable1 + time:syllable2 +
                 (1 + time + time:syllable1 + time:syllable2 + time:current|subject),
               data = mandarino,
               verbose = 2)
#oppure load("fitLmer")...
summary(fitLmer)
save(fitLmer, file="/Users/brosolo/Documents/GitHub/progetto-iterazione/models/brofacose_lmer_gamm4/fitLmer.Rdata")
#var.casuali/(var.casuali+var.res):
var.c <- 3.210e+03 + 1.135e+00 + 3.521e-02 + 1.149e-01 + 1.841e-01 + 7.631e-01 + 2.704e+00 + 1.329e+00 + 3.490e+00 + 1.524e+01 + 4.757e+00
var.c/(var.c + 3.550e+02) #0.9012

ypred = predict(fitLmer)
MSE <- mean((ypred - mandarino$f0)^2)
MSE

#qua provo a togliere effetti casuali ma non credo abbia senso essere tirchi:
fitLmer2 = lmer(f0 ~ cog_load + time + current + time:current + syllable1 + syllable2 +
                 syllable1:syllable2 + time:syllable1 + time:syllable2 +
                 (1|subject),
               data = mandarino,
               verbose = 2)
summary(fitLmer2)
save(fitLmer2, file="/Users/brosolo/Documents/GitHub/progetto-iterazione/models/brofacose_lmer_gamm4/fitLmer2.Rdata")
2787.2/(2787.2+570.5) #0.83
ypred2 = predict(fitLmer2)
MSE2 <- mean((ypred2 - mandarino$f0)^2)
MSE2
MSE
#molto peggiorato

#qua ne tolgo meno perchè boh oggi così:
fitLmer3 = lmer(f0 ~ cog_load + time + current + time:current + syllable1 + syllable2 +
                 syllable1:syllable2 + time:syllable1 + time:syllable2 +
                 (1 + time + time:current|subject),
               data = mandarino,
               verbose = 2)
summary(fitLmer3)
save(fitLmer3, file="/Users/brosolo/Documents/GitHub/progetto-iterazione/models/brofacose_lmer_gamm4/fitLmer3.Rdata")
(3322.050 + 1.184 + 1.630 + 8.692 + 1.697) / (3322.050 + 1.184 + 1.630 + 8.692 + 1.697 + 379.081) #0.8979
ypred3 = predict(fitLmer3)
MSE3 <- mean((ypred3 - mandarino$f0)^2)
MSE3 #poco più alto del primo modello
MSE


#provo aggiungendo time^2:
fitLmer4 = lmer(f0 ~ cog_load + time + I(time^2) + current + time:current + syllable1 + syllable2 +
                 syllable1:syllable2 + time:syllable1 + time:syllable2 +
                 (1 + time + time:syllable1 + time:syllable2 + time:current|subject),
               data = mandarino,
               verbose = 2)

summary(fitLmer4)
save(fitLmer4, file="//Users/brosolo/Documents/GitHub/progetto-iterazione/models/brofacose_lmer_gamm4/fitLmer4.Rdata")
ypred4 <- predict(fitLmer4)
MSE4 <- mean((ypred4 - mandarino$f0)^2)
MSE4 #meglio



## GAMM4 ##

#questo è quello di dede:
library(gamm4)
fitGamm4 = gamm4(f0 ~ cog_load + s(time) + current + time:current + syllable1 + syllable2 +
                   syllable1:syllable2 + time:syllable1 + time:syllable2,
                 random = ~ (1 + time + time:syllable1 + time:syllable2 + time:current|subject),
                 data = mandarino,
                 control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
                                       optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)),
                 verbose = 2)
summary(fitGamm4)
save(fitGamm4, file="/Users/brosolo/Documents/GitHub/progetto-iterazione/models/brofacose_lmer_gamm4/fitGamm4.Rdata")
ypredGamm4 <- predict(fitGamm4$mer)
MSE_Gamm4 <- mean((ypredGamm4 - mandarino$f0)^2)
MSE_Gamm4
MSE
#molto meglio, ma la previsione è giusta? cioè è giusto usare predict(fitGamm4$mer)?

#provo ad aggiungere time^2:
datasq = cbind(mandarino, time2 = mandarino$time^2)
fitGamm42 = gamm4(f0 ~ cog_load + s(time) + s(time2) + current + time:current + syllable1 + syllable2 +
                   syllable1:syllable2 + time:syllable1 + time:syllable2,
                 random = ~ (1 + time + time:syllable1 + time:syllable2 + time:current|subject),
                 data = datasq,
                 control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
                                       optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)),
                 verbose = 2)
summary(fitGamm42)
save(fitGamm42, file="/Users/brosolo/Documents/GitHub/progetto-iterazione/models/brofacose_lmer_gamm4/fitGamm42.Rdata")

ypredGamm42 <- predict(fitGamm42$mer)
MSE_Gamm42 <- mean((ypredGamm42 - mandarino$f0)^2)
MSE_Gamm42 #piccolo miglioramento
MSE_Gamm4

#aggiungo interazione di time^2 con current:
fitGamm43 = gamm4(f0 ~ cog_load + s(time) + s(time2) + current + time:current + time2:current + syllable1 + syllable2 +
                    syllable1:syllable2 + time:syllable1 + time:syllable2,
                  random = ~ (1 + time + time:syllable1 + time:syllable2 + time:current|subject),
                  data = datasq,
                  control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
                                        optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)),
                  verbose = 2)
summary(fitGamm43)
save(fitGamm43, file="/Users/brosolo/Documents/GitHub/progetto-iterazione/models/brofacose_lmer_gamm4/fitGamm43.Rdata")

ypredGamm43 <- predict(fitGamm43$mer)
MSE_Gamm43 <- mean((ypredGamm43 - mandarino$f0)^2)
MSE_Gamm43 #:)


#time^3: mooooolto lungo
datacube <- cbind(datasq, time3 = mandarino$time^3)
fitGamm44 = gamm4(f0 ~ cog_load + s(time) + s(time2) + s(time3) + current + time:current + time2:current + syllable1 + syllable2 +
                    syllable1:syllable2 + time:syllable1 + time:syllable2,
                  random = ~ (1 + time + time:syllable1 + time:syllable2 + time:current|subject),
                  data = datacube,
                  control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
                                        optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)),
                  verbose = 2)
summary(fitGamm44)
ypredGamm44 <- predict(fitGamm44$mer)
MSE_Gamm44 <- mean((ypredGamm44 - mandarino$f0)^2)
MSE_Gamm44
save(fitGamm44, file="/Users/brosolo/Documents/GitHub/progetto-iterazione/models/brofacose_lmer_gamm4/fitGamm44.Rdata")



# NUOVA VARIABILE:
#una variabile dicotomica che vale 1 quando le due sillabe sono uguali e 0 quando non lo sono
#magari ripetere una cosa due volte ha un effetto maggiore di dire due sillabe diverse
#vedo se migliora MSE

mandarino2 <- cbind(mandarino, doppia = rep(0, nrow(mandarino)))
mandarino2$doppia[mandarino2$syllable1 == mandarino2$syllable2] <- 1
mandarino2$doppia <- as.factor(mandarino2$doppia)

fitGamm45 = gamm4(f0 ~ cog_load + s(time) + current + time:current + syllable1 + syllable2 +
                   syllable1:syllable2 + time:syllable1 + time:syllable2 + doppia,
                 random = ~ (1 + time + time:syllable1 + time:syllable2 + time:current|subject),
                 data = mandarino2,
                 control = lmerControl(optimizer = "optimx", calc.derivs = FALSE,
                                       optCtrl = list(method = "nlminb", starttests = FALSE, kkt = FALSE)),
                 verbose = 2)
summary(fitGamm45)
save(fitGamm45, file="/Users/brosolo/Documents/GitHub/progetto-iterazione/models/brofacose_lmer_gamm4/fitGamm45.Rdata")

ypredGamm45 <- predict(fitGamm45$mer)
MSE_Gamm45 <- mean((ypredGamm45 - mandarino$f0)^2)
MSE_Gamm45 #meh mi sa che non serve, vabbè ci ho provato



cbind(MSE, MSE2, MSE3, MSE4, MSE_Gamm4, MSE_Gamm42, MSE_Gamm43, MSE_Gamm44, MSE_Gamm45)
#in teoria il migliore è quello con time al cubo


#un po' di plottini

ppc_subject("S1", datasq, ypredGamm42) #vabbè non fa sboccare ecco
ppc_subject("S1", mandarino2, ypredGamm45)
ppc_subject("S1", datasq, ypredGamm43)
ppc_subject("S1", datacube, ypredGamm44)

#plotta anche altri soggetti...

ppc_subject("S2", datasq, ypredGamm42) #e invece
ppc_subject("S2", mandarino2, ypredGamm45)
ppc_subject("S2", datasq, ypredGamm43)
ppc_subject("S2", datacube, ypredGamm44)

