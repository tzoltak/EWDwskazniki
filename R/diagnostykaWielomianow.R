#' @title Diagnostyka wielomianow
#' @description
#' Funkcja oblicza link test, BIC oraz współczynnik R2 (w przypadku modeli 'lm') lub dekompozycje wariancji (dla modeli 'lmerMod'),
#' sprawdza czy składowa wielomianowa modelu jest rosnąca oraz rysuje wykresy reszt modelu z wykresem przewidywań wielomianem
#' i regesją nieparametryczną.
#' @param model obiekt klasy 'lm' lub 'lmerMod'
#' @param zmienna ciąg znaków określający zmienną wielomianu
#' @param zmiennaGr wektor ciągów znakowych określających zmienne modelu, na podstawie których określone są grupy, w których wykonywane są obliczenia oraz dla których są sporządzane wykresy
#' @param folderWykresy ścieżka do folderu, gdzie powinny być zapisane wykresy. Jeżeli jest równa NULL to wykresy nie są zapisywane.
#' @param wykresyFun funkcja rysująca wykresy. Domyślnie jest to \code{png}.
#' @param wykresyParam lista parametrów funkcji rysującej wykresy.
#' @param przew_nparPar lista z parametrami funkcji \code{\link{przew_npar}}.
#' @param smooothScatterPar lista z parametrami funkcji \code{\link[graphics]{smoothScatter}}.
#' @return funkcja zwraca listę z obliczeniami dla modelu.
#' @import EWDogolny
#' @import lme4
#' @import plyr
#' @export
diagnostyka_wielomianow <- function(model, zmienna, zmiennaGr = NULL, 
                                    folderWykresy = NULL, wykresyFun = png, wykresyParam = NULL,
                                    przew_nparPar = NULL, smooothScatterPar = NULL){
  
  if( class(model) != "lmerMod" & class(model) != "lm" ) {
    stop("Obiekt model nie jest klasy 'lm' ani 'lmerMod'.")
    return(invisible(FALSE))
  }
  
  frameFull = modelFrame(model)
  zmiennaOrg = wyciagnij_nazwe_zmiennej(zmienna)
  zmienna = colnames(model.matrix(model))[ grepl(paste0("^", zmiennaOrg, "$|^poly[(]", zmiennaOrg,", [[:digit:]]+(|, raw = TRUE)[)]1$") , colnames(model.matrix(model)))]
  
  zmiennaGr = wyciagnij_nazwe_zmiennej(zmiennaGr, czyJednaZmienna = FALSE)
  zmiennaZalezna = all.vars(attributes(frameFull)$terms)[1]
  
  if( (!any( ktoraZmienna <- c(zmienna, zmiennaGr) %in% colnames(model.matrix(model)))) 
  ){
    stop("Model został zbudowany na danych, które nie zawierają kolumn: ", c(zmienna, zmiennaGr)[!ktoraZmienna])
  }
  
  if(is.null(zmiennaGr)){
    grupowanie = NULL
  } else{
    grupowanie = expand.grid(sapply(frameFull[, zmiennaGr], unique))
  }
  
  if( !is.null(grupowanie) ){
    grupowanieTab = cbind(grupowanie, grupa = 1:nrow(grupowanie))
    tabGr = data.frame( frameFull[, c(zmiennaGr) ])
    tabGr = suppressMessages(join(tabGr, grupowanieTab, type="inner"))
  }
  
  linkTest = numeric(max(1, nrow(grupowanie)))
  monotonicznosc = logical(max(1, nrow(grupowanie)))
  for(indeksGrupowania in 1:max(1, nrow(grupowanie))){
    
    if( is.null(grupowanie) ){
      grupa = rep(TRUE, nrow(frameFull))
    }else{
      grupa = tabGr$grupa == indeksGrupowania
    }
    
    poziomyGrup = grupowanie[indeksGrupowania, ]
    fixefModel = fixef(model)
    ranefModel = ranef(model)
    
    matrix = model.matrix(model)[grupa, ]
    frame = modelFrame(model)[grupa, ]
    
    wielomian = wielomian_grupa(matrix, fixefModel, zmiennaOrg, poziomyGrup)
    temp = reszta_grupa(wielomian, matrix, frame, ranefModel, fixefModel)
    
    wartosciZmiennejZaleznej = frame[, zmiennaZalezna]
    if( length(unique(wartosciZmiennejZaleznej[grupa])) > 100){
      przewNPar = przew_npar_rpr("reszty", zmiennaOrg, dane = temp, przew_nparPar)
    } else {
      przewNPar = regr_pierw_rodz("reszty", zmiennaOrg, dane = temp)
    }
    
    ## Rysowanie wykresów
    if( !is.null(folderWykresy) ){
      czescPliku = paste0(apply(grupowanie[indeksGrupowania, ], 2, as.character), collapse="_")
      plik = paste0(folderWykresy, "wykres_", czescPliku, ".png")
      
      if(is.null(wykresyParam)){
        wykresyParam = list(height=1004, width=1004, pointsize=12, res=150)
      }
      wykresyParam$filename = plik
      do.call(wykresyFun, wykresyParam)
    }
    
    if(is.null(smooothScatterPar)){
      smooothScatterPar = list(nbin=256, colramp=colorRampPalette(c("white", blues9)))
      smooothScatterPar$bandwidth = c((max(temp[, zmiennaOrg]) - min(temp[, zmiennaOrg])) / 256, (max(temp$reszty) - min(temp$reszty)) / 256)
      smooothScatterPar$xlim = range(model.matrix(model)[, zmienna], na.rm = TRUE)
      smooothScatterPar$ylim = range(frameFull[, zmiennaZalezna], na.rm = TRUE)
    }
    
    smooothScatterPar$x = data.frame(x = temp[, zmiennaOrg] , y = temp$reszty)
    do.call(smoothScatter, smooothScatterPar)
    grid(col=grey(0.5))
    lines(przewNPar, col=2, lwd=2, lty=1)
    lines(wielomian)
    
    if( !is.null(folderWykresy) ){
      dev.off()
    }
    
    linkTest[indeksGrupowania] = summary(lm(reszty ~ wielomian + I(wielomian^2), data=temp))$coefficients[3, 4]
    monotonicznosc[indeksGrupowania] = all(attributes(wielomian)$czyRosnaca)
  }
  
  if(is.null(grupowanie)){
    tab = data.frame(linkTest, monotonicznosc)
  } else {
    tab = data.frame(grupowanie, linkTest, monotonicznosc)
  }
  return(list(grupy = tab, 
              bic = AIC(model, k=log(nrow(frameFull))), 
              R2 = pseudoR2(model)))
}