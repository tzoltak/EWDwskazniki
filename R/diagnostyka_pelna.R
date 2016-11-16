#' @title Pełna diagnostyka wielomianow
#' @description Funkcja wykonuje pełną diagnostykę modeli EWD.
#' @param dane ramka danych z wynikami egzaminu
#' @param stopnie wektor liczb całkowitych określających stopnie wielomianów,
#' dla których należy zbudować modele.
#' @param typModelu zmienna określająca, jaki model ma być użyty. Dopuszczalne
#' wartości to "lm" oraz "lmer".
#' @param efektLosowy nazwa kolumny ramki 'dane', która określa efekt losowy.
#' Parametr ignorowany dla modelu 'lm'.
#' @param zmEgzWej nazwa kolumny ramki 'dane', która zawiera wyniki
#' (oszacowania umiejętności) egzaminu wejściowego.
#' @param zmEgzWyj nazwa kolumny ramki 'dane', która zawiera wyniki
#' (oszacowania umiejętności) egzaminu wyjściowego.
#' @param zmGrupujace wektor ciągów znakowych określających zmienne modelu, na
#' podstawie których określone są grupy, w których wykonywane są obliczenia oraz
#' dla których są sporządzane wykresy
#' @param zmDyslWej nazwa kolumny ramki 'dane', która zawiera informacje
#' o dysleksji zgłoszonej dla egzaminu wejściowego.
#' @param zmDyslWyj nazwa kolumny ramki 'dane', która zawiera informacje
#' o dysleksji zgłoszonej dla egzaminu wyjściowego.
#' @param katalogRysunki ścieżka do katalogu do zapisu wykresów diagnostycznych.
#' @param porownModeliPlik ścieżka do pliku do zapisu porównania modeli.
#' Parametr użyty tylko, gdy rozważamy wiele modeli.
#' @param tytulDiagnostyka tytuł wykresu diagnostycznego rysowanego, gdy
#' rozważany jest tylko jeden model.
#' @param maska wektor logiczny określający, które obserwacje z ramki dane
#' powinny zostać użyte do wyestymowania modeli.
#' @return funkcja zwraca listę modeli lub model (w przypadku podania parametru
#' 'stopnie' o długości 1)
#' @importFrom stats as.formula lm na.omit
#' @import lme4
#' @export
diagnostyka_pelna <- function(dane, stopnie, typModelu, zmEgzWej, zmEgzWyj,
                              zmGrupujace = NULL, efektLosowy = NULL,
                              zmDyslWej = NULL, zmDyslWyj = NULL,
                              katalogRysunki = NULL, porownModeliPlik = NULL,
                              tytulDiagnostyka = "", maska = NULL){
  stopifnot(is.data.frame(dane),
            is.numeric(stopnie), length(numeric) > 0,
            is.character(typModelu), length(typModelu) == 1,
            is.character(zmEgzWej), length(zmEgzWej) == 1,
            is.character(zmEgzWyj), length(zmEgzWyj) == 1,
            is.null(zmGrupujace) | is.character(zmGrupujace),
            is.null(efektLosowy) | is.character(efektLosowy),
            is.null(zmDyslWej) | is.character(zmDyslWej),
            is.null(zmDyslWyj) | is.character(zmDyslWyj),
            is.null(katalogRysunki) | is.character(katalogRysunki),
            is.null(porownModeliPlik) | is.character(porownModeliPlik),
            is.character(tytulDiagnostyka) & length(tytulDiagnostyka) == 1,
            is.null(maska) | is.logical(maska))
  stopifnot(typModelu %in% c("lm", "lmer"),
            all(c(zmEgzWej, zmEgzWyj, zmGrupujace, efektLosowy, zmDyslWej,
                zmDyslWyj, "plec") %in% names(dane)))
  if (any((stopnie %% 1) != 0)) {
    stop("Stopnie nie są liczbami całkowitymi:", stopnie[(stopnie %% 1)])
  }
  if (typModelu == "lmer" & is.null(efektLosowy)) {
    stop("Nieokreślony efekt losowy dla modelu lmer.")
  } else if (typModelu == "lm" & !is.null(efektLosowy)) {
    warning("Argument 'efektLosowy' zostanie pominięty, gdyż wybrano estymację modelu MNK.")
  }
  if (!is.null(zmGrupujace)) {
    stopifnot(length(zmGrupujace) > 0)
  }
  if (!is.null(efektLosowy)) {
    stopifnot(length(efektLosowy) == 1)
  }
  if (is.null(zmDyslWej) & is.null(zmDyslWyj)) {
    zmDysl = as.list(names(dane)[grep("^dysl(|eksja)_", names(dane))])
    names(zmDysl) = sub("^.*_(.*)$", "\\1", unlist(zmDysl))
    if ((!all(names(zmDysl) %in% c("g", "s")) &
         !all(names(zmDysl) %in% c("m", "g"))) | length(zmDysl) != 2) {
      stop("Nie udało się zgadnąć nazw zmiennych opisujących dysleksję. ",
           "Musisz podać je przy pomocy argumentóW 'zmDyslWej' i 'zmDyslWyj'.")
    }
    zmDysl = zmDysl[order(unlist(list("s" = 1, "g" = 2, "m" = 3)[names(zmDysl)]))]
    zmDyslWej = zmDysl[[1]]
    zmDyslWyj = zmDysl[[2]]
  }
  if (!is.null(zmDyslWej)) {
    stopifnot(length(zmDyslWej) == 1)
    if (is.null(zmDyslWyj)) {
      stop("Argumenty 'zmDyslWej' i 'zmDyslWyj' muszą albo oba zostać podane, albo oba być NULLami.")
    }
  }
  if (!is.null(zmDyslWyj)) {
    stopifnot(length(zmDyslWyj) == 1)
    if (is.null(zmDyslWej)) {
      stop("Argumenty 'zmDyslWej' i 'zmDyslWyj' muszą albo oba zostać podane, albo oba być NULLami.")
    }
  }
  if (!is.null(katalogRysunki)) {
    stopifnot(length(katalogRysunki) == 1)
    if (!dir.exists(katalogRysunki)) {
      tryCatch(dir.create(katalogRysunki),
               error = function(e) {
                 stop("Katalog '", katalogRysunki, "', podany w argumencie ",
                      "'katalogRysunki' nie istnieje i nie udało się go utworzyć.")
               })
      message("Utworzono katalog '", katalogRysunki, "'.")
    }
  }
  if (!is.null(porownModeliPlik)) {
    stopifnot(length(porownModeliPlik) == 1)
  }
  if (!is.null(maska)) {
    stopifnot(length(maska) == nrow(dane), all(maska) %in% c(TRUE, FALSE))
    dane = subset(dane, maska)
  }
  dane = na.omit(dane[, c(zmEgzWej, zmEgzWyj, zmGrupujace, efektLosowy,
                          zmDyslWej, zmDyslWyj, "plec")])

  modele = list()
  for (ind in seq_along(stopnie)) {
    if (typModelu == "lm") {
      formulaTekst = paste0(zmEgzWyj, "~", "poly(", zmEgzWej, ", ",
                            stopnie[ind], ", raw=TRUE)*",
                            paste(zmGrupujace, collapse = "*"), "+plec+",
                            zmDyslWyj, "*", zmDyslWej )
      modele[[ind]] = lm(as.formula(formulaTekst), dane)
    } else if (typModelu == "lmer") {
      formulaTekst = paste0(zmEgzWyj, "~", "poly(", zmEgzWej, ", ",
                            stopnie[ind], ", raw=TRUE)*",
                            paste(zmGrupujace, collapse = "*"), "+plec+",
                            zmDyslWyj, "*",zmDyslWej , "+ (1|", efektLosowy, ")")
      modele[[ind]] = lmer(as.formula(formulaTekst), data = dane, REML = TRUE)
    }
    names(modele)[ind] = paste0("stopień ", stopnie[ind])
  }

  if (length(stopnie) > 1) {
    wlasnosciModeli = lapply(modele, diagnostyka_wielomianow,
                             zmWielomian = zmEgzWej, zmGrupujace = zmGrupujace,
                             folderWykresy = katalogRysunki)
    porownaj_wielomiany(wlasnosciModeli, porownModeliPlik)
  } else if (length(stopnie) == 1) {
    diagnostyka(modele[[1]], katalogRysunki, tytulDiagnostyka)
    wlasnosciModeli = NULL
    modele = modele[[1]]
  }

  return(modele)
}
