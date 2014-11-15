#' @title Diagnostyka wielomianow
#' @description
#' Funkcja przygotowuje zestawienie porównujące modele z wielomianami różnych stopni.
#' @param x lista obiektów zwracanych przez funkcję \code{\link{diagnostyka_wielomianow}}
#' @param zapis opcjonalnie nazwa pliku, do którego ma zostać zapisane zestawienie
#' @param cyfryLinkTest liczba cyfr po przecinku, do której zostaną zaokrąglone
#' istotności "linktestu"
#' @param cyfryR2 liczba cyfr po przecinku, do której zostaną zaokrąglone wartości R2
#' (i BIC)
#' @return Funkcja zwraca listę z zestawieniami: 1) wyników "linktestu", 2) sprawdzenia
#' monotoniczności, 3) parametrami BIC i R2 modeli.
#' @import plyr
#' @import EWDogolny
#' @export
porownaj_wielomiany = function(x, zapis=NULL, cyfryLinkTest=5, cyfryR2=4) {
  linkTest = mapply(
    function(x, y, cyfryLinkTest) {
      x$grupy$linkTest = round(x$grupy$linkTest, cyfryLinkTest)
      names(x$grupy) = sub("linkTest", y, names(x$grupy))
      return(x$grupy[, names(x$grupy) != "czyRosnaca"])
    },
    x, as.list(names(x)), cyfryLinkTest=cyfryLinkTest, SIMPLIFY=FALSE
  )
  linkTest = suppressMessages(join_all(linkTest))
  message("Linktest")
  print(linkTest, row.names=FALSE)

  czyRosnaca = mapply(
    function(x, y, cyfryLinkTest) {
      names(x$grupy) = sub("czyRosnaca", y, names(x$grupy))
      return(x$grupy[, names(x$grupy) != "linkTest"])
    },
    x, as.list(names(x)), SIMPLIFY=FALSE
  )
  czyRosnaca = suppressMessages(join_all(czyRosnaca))
  message("Monotoniczność")
  print(czyRosnaca, row.names=FALSE)

  parametry = lapply(x, function(x) {
    if (is.matrix(x$R2)) {
      return(data.frame(bic=round(x$bic, cyfryR2),
                        "R2 ind."=round(x$R2[2, 2] / x$R2[2, 3], cyfryR2),
                        "R2 szk."=round(x$R2[1, 2] / x$R2[1, 3], cyfryR2)))
    } else {
      return(data.frame(bic=round(x$bic, cyfryR2), "R2"=round(x$R2, cyfryR2)))
    }
  })
  parametry = t(rbind.fill(parametry))
  colnames(parametry) = names(x)
  message("Parametry")
  print(parametry)

  wynik = list(linkTest=linkTest, czyRosnaca=czyRosnaca, parametry=parametry)
  if (!is.null(zapis)) {
    tryCatch({do_schowka(wynik, plik=zapis)}, error=stop)
  }
  invisible(wynik)
}
