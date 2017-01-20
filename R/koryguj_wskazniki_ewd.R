#' @title Obliczanie EWD
#' @description
#' Funkcja na podstawie listy modeli, data frame'a z danymi, na podstawie
#' których zostały one obliczone oraz data frame'a z danymi skorygowanymi,
#' dla których wartości wskaźników EWD mają zostać przeliczone, przygotowuje
#' zestawienia z wartościami wskaźników EWD (używając jako konia roboczego
#' \link{przygotuj_wsk_ewd}).
#' @param modele lista modeli klasy \code{lm} lub \code{lmerMod}
#' @param dane data frame z danymi, na podstawie
#' których estymowane były modele podane parametrem \code{modele}
#' @param daneKorekty data frame z danymi szkół, dla których mają zostać
#' przeliczone wskaźniki
#' @param skale data frame, która zostanie przekazana
#' jako argument o tej samej nazwie do \link{przygotuj_wsk_ewd}
#' @param powiazaniaPrzedmiotow opcjonalnie lista, która zostanie przekazana
#' jako argument o tej samej nazwie do \link{przygotuj_wsk_ewd};
#' @return lista data frame'ów
#' @export
koryguj_wsk_ewd = function(modele, dane, daneKorekty, skale = NULL,
                             powiazaniaPrzedmiotow = NULL) {
  stopifnot(is.list(modele), length(modele) > 0,
            is.data.frame(daneKorekty),
            is.null(skale) | is.data.frame(skale),
            is.null(powiazaniaPrzedmiotow) | is.list(powiazaniaPrzedmiotow))
  zmIdSzk = names(dane)[grep("^id_szkoly_", names(dane))]
  if (length(zmIdSzk) < 1) {
    stop("W danych brak kolumny z id szkoły.")
  }
  names(zmIdSzk) = sub("^id_szkoly_", "", zmIdSzk)
  zmIdSzk = zmIdSzk[order(unlist(list("s" = 1, "g" = 2, "m" = 3)[names(zmIdSzk)]))]
  zmIdSzkWy = zmIdSzk[length(zmIdSzk)]

  dane = subset(dane, !(get(zmIdSzkWy) %in% daneKorekty[[zmIdSzkWy]]))
  ewd = przygotuj_wsk_ewd(modele, dane, daneKorekty, skale = skale,
                          powiazaniaPrzedmiotow = powiazaniaPrzedmiotow)
  attributes(ewd) =
    attributes(ewd)[names(attributes(ewd)) %in%
                      c("names", "class", "liczba_zdajacych", "dataUtworzenia")]
  maska = unique(daneKorekty[[zmIdSzkWy]])
  for (i in 1:length(ewd)) {
    # w wynikach działania funkcji ewd() wartości zmiennej "pomin" równe TRUE
    # wskazują po prostu na wyniki obliczone na podstawie argumentu "danePominiete"
    ewd[[i]] = subset(ewd[[i]], get(zmIdSzkWy) %in% maska & get("pomin"))
    ewd[[i]]$pomin = FALSE
  }
  attributes(ewd)$liczba_zdajacych =
    subset(attributes(ewd)$liczba_zdajacych, get(zmIdSzkWy) %in% maska)
  return(ewd)
}
