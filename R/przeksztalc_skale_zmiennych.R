#' @title Wyliczanie EWD
#' @description
#' Funkcja na podstawie listy modeli, data frame'a z danymi, na podstawie
#' których zostały one wyliczone oraz ew. data frame'a o takiej samej
#' strukturze zawierającego dane nie użyte wcześniej do estymacji modeli,
#' przygotowuje zestawienia z wartościami wskaźników EWD.
#' @param dane data frame z danymi, typowo wczytany do R wynik działania
#' funkcji \code{\link[EWDdane]{przygotuj_dane_do_ewd}}
#' @param czesciEgzWy wektor ciągów znaków - kody konstruktów, które będą
#' w modelach EWD zmiennymi zależnymi
#' @param sufiksWyniki ciąg znaków: "irt", "norm" lub "suma"; określa, które
#' wyniki będą używane do wyliczania wskaźników i mają zostać przekształcone
#' @return data frame
#' @export
przeksztalc_skale_zmiennych = function(dane, czesciEgzWy, sufiksWyniki) {
  stopifnot(is.data.frame(dane),
            is.character(czesciEgzWy), length(czesciEgzWy) > 0,
            is.character(sufiksWyniki), length(sufiksWyniki) == 1)
  stopifnot(sufiksWyniki %in% c("irt", "norm", "suma"))

  if (sufiksWyniki == "irt") {
    # jeśli operujemy na wynikach wyskalowanych IRT, to zm. zależne trzeba przerzucić na 100;15
    for (i in intersect(names(dane), paste0(czesciEgzWy, "_irt"))){
      dane[, i] = 100 + 15 * dane[, i]
    }
  } else if (sufiksWyniki == "norm") {
    # jeśli operujemy na wynikach znormalizowanych, to zm. nzal. warto przerzucić na 0;1
    for (i in setdiff(names(dane)[grepl("_norm$", names(dane)) &
                                  !grepl("^bs_", names(dane))],
                      intersect(names(dane), paste0(czesciEgzWy, "_norm")))){
      dane[, i] = (dane[, i] - 100) / 15
    }
  }
  return(dane)
}
