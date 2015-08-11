#' @title Diagnostyka wielomianow
#' @description
#' Funkcja oblicza link test, BIC oraz współczynnik R2 (w przypadku modeli 'lm')
#' lub dekompozycje wariancji (dla modeli 'lmerMod'), sprawdza czy składowa
#' wielomianowa modelu jest rosnąca oraz rysuje wykresy reszt modelu z wykresem
#' przewidywań wielomianem i regesją nieparametryczną.
#' @param model obiekt klasy 'lm' lub 'lmerMod'
#' @param zmWielomian ciąg znaków określający zmienną wielomianu
#' @param zmGrupujace wektor ciągów znakowych określających zmienne modelu, na
#' podstawie których określone są grupy, w których wykonywane są obliczenia oraz
#' dla których są sporządzane wykresy
#' @param folderWykresy ścieżka do folderu, gdzie powinny być zapisane wykresy.
#' Jeżeli jest równa NULL to wykresy nie są zapisywane.
#' @param wykresyFun funkcja rysująca wykresy. Domyślnie jest to \code{png}.
#' @param wykresyParam lista parametrów funkcji rysującej wykresy.
#' @param przew_nparPar lista z parametrami funkcji \code{\link{przew_npar}}.
#' @param smooothScatterPar lista z parametrami funkcji \code{\link[graphics]{smoothScatter}}.
#' @return funkcja zwraca listę z obliczeniami dla modelu.
#' @import EWDogolny
#' @import lme4
#' @import plyr
#' @export
diagnostyka_wielomianow <- function(model, zmWielomian, zmGrupujace = NULL,
                                   folderWykresy = NULL, wykresyFun = png, wykresyParam = NULL,
                                   przew_nparPar = NULL, smooothScatterPar = NULL){
  zmWielomian = wyciagnij_nazwe_zmiennej(zmWielomian)
  zmGrupujace = wyciagnij_nazwe_zmiennej(zmGrupujace, czyJednaZmienna = FALSE)
  # trzeba tu dopisać jakieś sprawdzanie warunków, czy zmWielomian i zmGrupujace są w modelu (czy tam w danych)

  zmWielomianMacierz = colnames(model.matrix(model))[ grepl(paste0("^", zmWielomian, "$|^poly[(]", zmWielomian,", [[:digit:]]+(|, raw = TRUE)[)][1]{0,1}$") , colnames(model.matrix(model)))]

  mapowanie = model_map(model, zmWielomian)
  # Teraz wykorzystując mapowanie możemy przygotować maski:
  maskaWielomian = mapowanie[rownames(mapowanie) == zmWielomian, ]
  maskaGrupowanie = apply( mapowanie[ rownames(mapowanie) %in% zmGrupujace , , drop = FALSE], 2, any) &
    apply(!mapowanie[!(rownames(mapowanie) %in% zmGrupujace), , drop = FALSE], 2, all)
  maskaGrupowanie = c(TRUE, maskaGrupowanie[-1])  # i jeszcze stała
  maskaPozostale = !maskaWielomian & !maskaGrupowanie

  maskaTylkoWielomian = mapowanie[rownames(mapowanie) == zmWielomian, ] &
    apply( !mapowanie[ !rownames(mapowanie) %in% zmWielomian , , drop = FALSE], 2, all)
  stopien = sum(maskaTylkoWielomian)

  if (class(model) == "lm") {
    modelMatrix = t(model.matrix(model)) * coef(model)
    # nie transponuję z powrotem po mnożeniu, więc poniżej są colSums, a nie rowSums
    # a mapowania są po wierszach
    przewWielomian = colSums(modelMatrix[maskaWielomian | maskaGrupowanie, ])
    resztyCzesciowe = model.frame(model)[, 1] - colSums(modelMatrix[maskaPozostale, ])
    rm(modelMatrix)
  } else if (class(model) == "lmerMod") {
    # W wersji dla lmer() jest bardziej skomplikowanie, ale za to za jednym
    # zamachem uwzględnimy sobie efekty losowe, w tym ew. efekty losowe dla nachyleń(!),
    # których w naszych modelach co prawda póki co nie ma, ale w ogólności mogłyby być.
    # Przewidywanie z wielomianu idzie po prostu z efektów stałych, bez wielkiego cudowania:
    przewWielomian = colSums(t(model.matrix(model)[, maskaWielomian | maskaGrupowanie]) *
                               fixef(model)[maskaWielomian | maskaGrupowanie])
    # nie transponuję z powrotem po mnożeniu, więc poniżej są colSums, a nie rowSums
    # Z resztami częściowymi sprawa jest troszkę bardziej złożona, bo one muszą
    # przechwycić efekty stałe i losowe pozostałych zmiennych, ale też efekty
    # losowe wielomianu (i grupowania):
    # na początek przygotujmy sobie obiekt, który po naszej działalności będzie
    # przechowywał wartości współczynników "spersonalizowane" dla każdej obserwacji
    efStObs = matrix(0, ncol = ncol(model.matrix(model)),
                     nrow = nrow(model.matrix(model)),
                     dimnames = dimnames(model.matrix(model)))
    efSt = coef(model) # to jest lista data.frame'ów!
    # elementy listy są związane z pogupowaniem (dla odmiany tym związanym
    # z efektem losowym), i każdy z nich jest data.framem opisującym wartości
    # parametrów w ramach każdej grupy danego pogrupowania (tj. efekt stały plus
    # ew. przewidywanie realizacji efektu losowego w ramach danej grupy)
    for (i in 1:length(efSt)) {
      if (i > 1) {
        # jeśli zmiennych grupujących jest kilka, to nie chcemy wielokrotnie
        # sumować efektów stałych
        efSt[[i]] = t(t(efSt[[i]]) - fixef(model))
      } else {
        # a za pierwszym razem pozbywamy się tylko efektów stałych związanych
        # z wielomianem (i grupowaniem)
        efSt[[i]][, maskaWielomian | maskaGrupowanie] =
          t(t(efSt[[i]][, maskaWielomian | maskaGrupowanie]) -
              fixef(model)[maskaWielomian | maskaGrupowanie])
      }
      # dopiszmy sobie coś, po czym będzie można łączyć
      efSt[[i]] = cbind(rownames(efSt[[i]]), efSt[[i]], stringsAsFactors = FALSE)
      colnames(efSt[[i]])[1] = names(efSt)[i]
      # zawikłane: chcemy wydobyć z model.frame wartości zmiennej grupującej,
      # skonwertować je na tekst (co by było kompatybilnie z tym, co wyciągnęliśmy
      # przed chwilą z rownames() - to przecież też tekst), ale żeby na końcu
      # cały czas mieć data.frame'a z zachowaną nazwą (jedynej) kolumny
      temp = as.data.frame(lapply(model.frame(model)[, names(efSt)[i], drop = FALSE],
                                  as.character), stringsAsFactors = FALSE)
      # ważne: korzystamy z tego, że join() zachowuje uporządkowanie wierszy
      # pierwszego argumentu! (uwaga, merge() by tego nie zrobiła)
      efSt[[i]] = suppressMessages(join(temp, efSt[[i]]))
      efStObs = efStObs + efSt[[i]][, -1] # dodajemy
    }
    # tu mnożenie przez macierz o takim samym wymiarze, więc bez transpozycji
    resztyCzesciowe = model.frame(model)[, 1] - (rowSums(model.matrix(model) * efStObs) )
    rm(efSt) # to może być duże, niech nie zajmuje miejsca niepotrzebnie
  }

  # I już bardzo prosto przygotować sobie obiekt, którego będzie bardzo wygodnie
  # używać do rysowania i testowania:
  danePoobliczane = data.frame(
    model.frame(model)[, colnames(model.frame(model)) %in% zmGrupujace, drop = FALSE],  # to są zmienne grupujące
    zmWielomianowa = model.matrix(model)[, zmWielomianMacierz],
    przewWielomian = przewWielomian,
    resztyCzesciowe = resztyCzesciowe
  )

  if (is.null(zmGrupujace)) {
    danePoobliczane = cbind(artefaktualnaZmGrupujaca = 1, danePoobliczane)
    zmGrupujace = "artefaktualnaZmGrupujaca"
  }

  # obsługa parametrów sterujących rysowaniem - wartości domyślne i ew. ich
  # nadpisywanie/dopisywanie
  # funkcja zapisująca wykresy
  wykresyParamDomyslne = list(height = 1004, width = 1004, pointsize = 12, res = 150)
  if (!is.null(wykresyParam)) {
    for (i in names(wykresyParam)) wykresyParamDomyslne[[i]] = wykresyParam[[i]]
  }
  wykresyParam = wykresyParamDomyslne
  # smoothscatter
  xlimSmooth = range(danePoobliczane$zmWielomianowa, na.rm = TRUE)
  ylimSmooth = range(danePoobliczane$resztyCzesciowe, na.rm = TRUE)
  smooothScatterParDomyslne = list (
    nbin = 256,
    colramp = colorRampPalette(c("white", blues9)),
    bandwidth = with(danePoobliczane, c((max(zmWielomianowa ) - min(zmWielomianowa )) / 256,
                                        (max(resztyCzesciowe) - min(resztyCzesciowe)) / 256)),
    xlim = xlimSmooth,
    ylim = ylimSmooth,
    ylab = "przewidywanie (uśrednione)",
    xlab = zmWielomian
  )
  if (!is.null(smooothScatterPar)) {
    for (i in names(smooothScatterPar)) smooothScatterParDomyslne[[i]] = smooothScatterPar[[i]]
  }
  smooothScatterPar = smooothScatterParDomyslne

  # rysujemy i liczymy w podziale na grupy
  wyniki =
    ddply(danePoobliczane, zmGrupujace,
          function(x, zmGrupujace, zmZal, stopien, wykresyParam, smooothScatterPar) {
            wielomianXY = unique(data.frame(x = x$zmWielomianowa, y = x$przewWielomian ))
            wielomianXY = wielomianXY[order(wielomianXY$x), ]

            if (length(unique(x$resztyCzesciowe)) > 100) {
              przewNPar = przew_npar_rpr("resztyCzesciowe", "zmWielomianowa", dane = x, przew_nparPar)
            } else {
              przewNPar = regr_pierw_rodz("resztyCzesciowe", "zmWielomianowa", dane = x)
            }

            smooothScatterPar$main = paste(paste0("wielomian ", stopien, ". stopnia"),
                                           "\n",
                                           paste0(paste(zmGrupujace,
                                                        apply(x[1, zmGrupujace, drop = FALSE],
                                                              2, as.character),
                                                        sep = ": "),
                                                  collapse = ", "))
            smooothScatterPar$x = data.frame(x = x$zmWielomianowa , y = x$resztyCzesciowe)
            do.call(smoothScatter, smooothScatterPar)
            grid(col = grey(0.5))
            lines(przewNPar, col = 1, lwd = 2, lty = 2)
            # rysowanie wielomianu
            monot = c(wielomianXY$y[2] > wielomianXY$y[1],
                      wielomianXY$y[-1] > wielomianXY$y[-length(wielomianXY$y)])
            lines(wielomianXY$x, wielomianXY$y, col = ifelse(monot[1], 3, 2),
                  lwd = 2, lty = 1)
            # wydzielamy kawałki monotoniczne
            inds = c(which(monot[-1] != monot[-length(monot)]), length(monot))
            for (i in seq_along(inds[-1])) {
              if (monot[inds[i] + 1]) {  # następny kawałek rosnący
                lines(wielomianXY$x[inds[i]:(inds[i + 1])],
                      wielomianXY$y[inds[i]:(inds[i + 1])],
                      col = 3, lwd = 2, lty = 1)
              } else {# następny kawałek malejący
                lines(wielomianXY$x[inds[i]:(inds[i + 1])],
                      wielomianXY$y[inds[i]:(inds[i + 1])],
                      col = 2, lwd = 2, lty = 1)
              }
            }

            if ( !is.null(folderWykresy) ) {
              folderDoPowrotu = getwd()
              if (dir.exists(folderWykresy)) {
                setwd(folderWykresy)
              } else {
                dir.create(folderWykresy)
              }
              plik = paste0(zmZal, "_stopien", stopien, "_",
                            paste0(zmGrupujace, x[1, zmGrupujace],
                                   collapse = "_"), ".png")
              wykresyParam$filename = plik
              wykresyParam$device = wykresyFun
              do.call(dev.print, wykresyParam)
              setwd(folderDoPowrotu)
            }

            linkTest = summary(lm(resztyCzesciowe ~ przewWielomian +
                                    I(przewWielomian^2), x))$coefficients[3, 4]

            return(data.frame(czyRosnaca = all(monot), linkTest = linkTest))
          },
          zmGrupujace = zmGrupujace,
          zmZal = colnames(model.frame(model))[1],
          stopien = stopien,
          wykresyParam = wykresyParam,
          smooothScatterPar = smooothScatterPar
    )
  # koniec
  return(list(grupy = wyniki[, colnames(wyniki) != "artefaktualnaZmGrupujaca"],
              bic = AIC(model, k = log(nrow(model.matrix(model)))),
              R2 = pseudoR2(model)))
}
