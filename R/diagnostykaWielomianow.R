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
#' @param smooothScatterPar lista z parametrami funkcji \code{\link[utils]{smoothScatter}}.
#' @return funkcja zwraca listę z obliczeniami dla modelu.
#' @export
diagnostyka_wielomianow <- function(model, zmienna, zmiennaGr = NULL,
                                    folderWykresy = NULL, wykresyFun = png, wykresyParam = NULL,
                                    przew_nparPar = NULL, smooothScatterPar = NULL){
  if( class(model) != "lmerMod" & class(model) != "lm" ) {
    stop("Obiekt model nie jest klasy 'lm' ani 'lmerMod'.")
    return(invisible(FALSE))
  }

  frame = modelFrame(model)
  zmienna = wyciagnij_nazwe_zmiennej(zmienna)
  zmiennaGr = wyciagnij_nazwe_zmiennej(zmiennaGr, czyJednaZmienna = FALSE)
  zmiennaZalezna = all.vars(attributes(modelFrame(model))$terms)[1]

  isPoly = any(indPoly <- grepl("^poly[(]", colnames(frame)))
  if(isPoly){
    zmiennaX = colnames(model.matrix(model))[ grepl(paste0("^poly[(]", zmienna,", [[:digit:]]+(|, raw = TRUE)[)]1$"), colnames(model.matrix(model)))]
  }

  if(   (!isPoly & !any(ktoraZmienna <- c(zmienna                                                 , zmiennaGr) %in% colnames(frame)))
      | ( isPoly & !any(ktoraZmienna <- c(any(grepl(paste0("^poly[(]", zmienna), colnames(frame))), zmiennaGr) %in% colnames(frame)))
  ){
    stop("Model został zbudowany na danych, które nie zawierają kolumn: ", c(zmienna, zmiennaGr)[!ktoraZmienna])
  }

  grupowanie = daj_grupowanie(frame, zmiennaGr)

  if( is.null(grupowanie) | nrow(grupowanie) != 0 ){
    grupowanieTab = cbind(grupowanie, grupa = 1:nrow(grupowanie))
    tabGr = data.frame(frame[, c(zmiennaGr)])
    tabGr = suppressMessages(inner_join(tabGr, grupowanieTab))
  }

  linkTest = numeric(max(1, nrow(grupowanie)))
  monotonicznosc = logical(max(1, nrow(grupowanie)))
  for(indeksGrupowania in 1:max(1, nrow(grupowanie))){
    if( is.null(grupowanie) | nrow(grupowanie) == 0 ){
      grupa = rep(TRUE, nrow(frame))
    } else {
      tabGr$grupa == indeksGrupowania
    }

    message(paste0( apply(grupowanie[indeksGrupowania, ], 2, as.character), collapse=" " ))
    wielomian = wielomian_lmer(model, zmienna, zmiennaGr, grupa, grupowanie, indeksGrupowania, nsiatka=0, czyTylkoGrupa = TRUE)
    temp      = przewidywanie_lmer(model, wielomian, grupa, zmienna)

    wartosciZmiennejZaleznej = frame[, zmiennaZalezna]
    if( length(unique(wartosciZmiennejZaleznej[grupa])) > 100){
      #       if(is.null(przew_nparPar)){
      #         przew_nparPar = list()
      #       }
      #       przew_nparPar$formula = as.formula(paste0("reszty~", zmienna))
      #       przew_nparPar$data = temp
      przewNPar = przew_npar_rpr("reszty", zmienna, dane = temp, przew_nparPar)
      #       przewNPar = do.call(przew_npar, przew_nparPar)
    } else {
      przewNPar = regr_pierw_rodz("reszty", zmienna, dane = temp)
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
      smooothScatterPar$bandwidth = c((max(temp[, zmienna]) - min(temp[, zmienna])) / 256, (max(temp$reszty) - min(temp$reszty)) / 256)

      xx = if(isPoly){
        model.matrix(model)[, zmiennaX]
      } else{
        frame[, zmienna]
      }
      smooothScatterPar$xlim = range(xx, na.rm = TRUE)
      smooothScatterPar$ylim = range(frame[, zmiennaZalezna], na.rm = TRUE)
    }

    smooothScatterPar$x = data.frame(x = temp[, zmienna] , y = temp$reszty)
    do.call(smoothScatter, smooothScatterPar)
    grid(col=grey(0.5))
    lines(przewNPar, col=2, lwd=2, lty=1)
    lines(wielomian)

    if( !is.null(folderWykresy) ){
      dev.off()
    }

    linkTest[indeksGrupowania] = summary(lm(reszty ~ wielomian + I(wielomian^2), data=temp))$coefficients[3, 4]
    monotonicznosc[indeksGrupowania] = attributes(wielomian)$czyRosnaca
  }

  if(is.null(grupowanie) | nrow(grupowanie) == 0){
    tab = data.frame(            linkTest, monotonicznosc)
  } else {
    tab = data.frame(grupowanie, linkTest, monotonicznosc)
  }
  return(list(grupy = tab,
              BIC = AIC(model, k=log(nrow(frame))),
              R2 = pseudoR2(model)))
}
