#' @title Przeksztalcanie normalizacji ekwikwantylowej
#' @description
#' Funkcja służy do sklejania ze sobą najniższych wartości skali znormalizowanej
#' ekwikwantylowo, przeliczając jednocześnie wartości skali.
#' @param wynikiZnorm wektor wartości na skali znormalizowanej
#' @param wynikiSurowe wektor wartości surowych, odpowiadających wartościom
#' znormalizowanym podanym w \code{wynikiZnorm}
#' @param do wartość liczbowa - wartość surowa, do jakiej należy kontynuować
#' skracanie (ale nie zostanie przeprowadzonych więcej złączeń, niż wartość
#' argumentu \code{ile})
#' @param grupy opcjonalnie zmienna definiująca podział na grupy, w ramach
#' których oddzielnie ma być przeprowadzone łączenie
#' @param sr liczba - średnia znormalizowanej skali
#' @param os liczba - odchylenie standardowe znormalizowanej skali
#' @param ile wartość liczbowa - maksymalna liczba złączeń, które mogą zostać
#' wykonane
#' @details
#' Tu w przyszłości uzupełnić. Trzeba dodać do funkcji jakąś weryfikację
#' poprawności argumentów.
#' @return Wektor liczbowy z przeliczonymi wartościami argumentu \code{wynikiZnorm}.
#' @seealso \code{\link[ZPD]{normalizuj_ekwikwantylowo}}
#' @export
sklej_normy = function(wynikiZnorm, wynikiSurowe, do, grupy=NULL, sr=100, os=15, ile=do) {
  if (is.null(grupy)) grupy = rep(1, length(wynikiZnorm))
  grupy = as.character(grupy)

  normyMin = by(wynikiZnorm, grupy, function(x) {return(sort(unique(x))[1:2])} )
  normyMinNowe = lapply(normyMin,
                        function(x, popr) {
                          return(qnorm(pnorm(x[2], sr, os) - pnorm(x[1], sr, os), sr, os))
                        }
  )
  for (i in 1:length(normyMin)) {
    if (wynikiSurowe[grupy == names(normyMin)[i] &
                     wynikiZnorm == normyMin[[i]][2] &
                     !is.na(wynikiZnorm)][1] <= do) {
      # jeśli w danych jakiś wynik spr. nie wystąpił, to nie sklejajmy ze sobą
      # bez potrzeby wyższych wyników
      wynikiZnorm[grupy == names(normyMin)[i] &
                    wynikiZnorm %in% normyMin[[i]] &
                    !is.na(wynikiZnorm)] = normyMinNowe[[i]]
    }
  }

  if (ile > 1) wynikiZnorm = sklej_normy(wynikiZnorm, wynikiSurowe, do, grupy,
                                         sr, os, ile - 1)
  return(wynikiZnorm)
}
