#' @title Liczba zdających egzaminy.
#' @description
#' Funkcja zwraca tabelę z zestawieniem liczby podchodzących do dwóch egzaminów (na wejściu i na wyjściu), z wyszczególnieniem liczby laureatów i dyslektyków.
#' @param x data frame z danymi potrzebnymi do przygotowania zestawienia
#' @param nazwyZmWynikiEgzWy wektor tekstowy zawierający nazwy zmiennych zwynikami poszczególnych części egzaminu na wyjściu, dla których ma być przygotowane zestawienie
#' @param sufiksEgzWe ciąg znaków (zwykle pojedynczy znak) - sufiks związany z egzaminem na wejściu (zmienne opisujące bycie laureatem czy dyslektykiem dotyczącego tego egzaminu mają nazwy kończące się na '_sufiksEgzWe')
#' @param wydl liczba całkowita - jakie maksymalne wydłużenie toku kształcenia [lat] uwzględnić w zestawieniu
#' @param nazwaEgzWy ciąg znaków - nazwa egzaminu na wyjściu (do nagłówka tabeli)
#' @param nazwaEgzWe ciąg znaków - nazwa egzaminu na wejściu (do nagłówka tabeli)
#' @details
#' W \code{x} muszą znajdować się zmienne wymienione w \code{nazwyZmWynikiEgzWy}.
#' Nazwy wymienione w \code{nazwyZmWynikiEgzWy} muszą pasować do maski '^(sum|norm)_sufiks$', gdzie 'sufiks' jest skrótem powiązanym z daną częścią egzaminu na wyjściu (w przyszłości zestaw prefiksów zostanie zapewne poszerzony).
#' W \code{x} muszą się też znajdować zmienne o nazwach postaci 'laureat_sufiks' (dla każdego sufiksu jw.) oraz 'dysleksja_substr(sufiks, 1, 1)'.
#' Odnośnie egzaminu na wejściu, w \code{x} muszą znajdować się zmienne o nazwach postaci: 'rok_\code{sufiksEgzWe}' oraz 'laureat_\code{sufiksEgzWe}', 'dysleksja_substr(\code{sufiksEgzWe}, 1, 1)'.
#' @return data frame
#' @export
tabelka_ld = function(x, nazwyZmWynikiEgzWy, sufiksEgzWe, wydl=1, nazwaEgzWy="egz. wy.", nazwaEgzWe="egz. we.") {
  stopifnot(is.data.frame(x), is.character(nazwyZmWynikiEgzWy),
            is.character(sufiksEgzWe), length(sufiksEgzWe) == 1,
            is.numeric(wydl), length(wydl) == 1,
            is.character(nazwaEgzWy), length(nazwaEgzWy) == 1,
            is.character(nazwaEgzWe), length(nazwaEgzWe) == 1
  )
  stopifnot(all(nazwyZmWynikiEgzWy %in% names(x)),
            paste0("rok_", sufiksEgzWe) %in% names(x),
            all(grepl("^(sum|norm)_", nazwyZmWynikiEgzWy))
  )

  zmRokWe = paste0("rok_", sufiksEgzWe)
  # przygotowujemy macierz z nazwami wierszy i kolumn
  tabelkaLD = matrix(NA, nrow=length(nazwyZmWynikiEgzWy), ncol=3 + 3 * (wydl + 1),
                     dimnames=list(
                       nazwyZmWynikiEgzWy,
                       c("wszyscy", paste0(c("laureaci ", "dysleksja "), nazwaEgzWy),
                         paste0(
                           rep(c("laureaci ", "dysleksja ", "dysleksja "), wydl + 1),
                           nazwaEgzWe, " ",
                           rep(max(x[, zmRokWe]) - (0:wydl), each=3),
                           rep(c("", "", paste0(" i ", nazwaEgzWy)), wydl + 1)
                         )
                       )
                     )
  )
  # żeby ją wypełnić
  for (i in nazwyZmWynikiEgzWy) {
    skrot =sub("^(sum|norm)_", "", i)
    temp = c(
      sum(!is.na(x[, i])),
      sum(x[, paste0("laureat_"  , skrot              )] %in% "tak" & !is.na(x[, i])),
      sum(x[, paste0("dysleksja_", substr(skrot, 1, 1))] %in% "tak" & !is.na(x[, i]))
    )
    for (j in (max(x[, zmRokWe]) - (0:wydl))) {
      xTemp = x[x[, zmRokWe] == j, ]
      temp = c(temp,
               sum(xTemp[, paste0("laureat_"  , sufiksEgzWe              )] %in% "tak" & !is.na(xTemp[, i])),
               sum(xTemp[, paste0("dysleksja_", substr(sufiksEgzWe, 1, 1))] %in% "tak" & !is.na(xTemp[, i])),
               sum(xTemp[, paste0("dysleksja_", substr(sufiksEgzWe, 1, 1))] %in% "tak" & xTemp[, paste0("dysleksja_", substr(skrot, 1, 1))] %in% "tak" & !is.na(xTemp[, i]))
      )
    }
    tabelkaLD[rownames(tabelkaLD) == i, ] = temp
  }
  return(tabelkaLD)
}
#' @title Parametry rozkładów zmiennych.
#' @description
#' Funkcja zwraca wartości zestawu statystyk opisowych: kwartyle, średnia, odch. stand.
#' @param x wektor liczbowy
#' @param na.rm wartość logiczna - przekazywana do funkcji wyliczających statystyki
#' @param digits liczba całkowita - do ilu miejsc po przecinku zaokrąglać zwracane wyniki
#' @return wektor liczb
mojeParametry = function(x, na.rm=TRUE, digits=2) {
  stopifnot(is.numeric(x), is.logical(na.rm), is.numeric(digits),
            length(na.rm) == 1, length(digits) == 1)
  stopifnot(na.rm %in% c(TRUE, FALSE))
  return(round(
    c(
      min          = min(x, na.rm=na.rm),
      "1. kw."     = as.numeric(quantile(x, prob=0.25, na.rm=na.rm)),  #as.numeric, żeby pozbyć się nazwy
      mediana      = median(x, na.rm=na.rm),
      "3. kw."     = as.numeric(quantile(x, prob=0.75, na.rm=na.rm)),
      max          = max(x, na.rm=na.rm),
      "średnia"    = mean(x, na.rm=na.rm),
      "odch. std." = sd(x, na.rm=na.rm)
    ), digits))
}
#' @title Parametry rozkładów zmiennych.
#' @description
#' Funkcja zwraca wartości zestawu statystyk opisowych: kwartyle, średnia, odch. stand. dla zestawu zmiennych, w podziale na grupy.
#' @param x data frame lub lista ze zmiennymi, dla których mają zostać wyliczone statystyki
#' @param grBezLacznie ciąg znaków lub NULL, gdy nie dotyczy - nazwa zmiennej grupującej, dla której statystyki mają być zwrócone tylko w podziale na grupy, ale bez podawania statystyk dla całej zbiorowości
#' @param grZLacznie ciąg znaków lub NULL, gdy nie dotyczy - nazwa zmiennej grupującej, dla której statystyki mają być zwrócone zarówno w podziale na grupy, jak i dla całej zbiorowości
#' @param nazwaPierwKol ciąg znaków - nazwa pierwszej kolumny zwracenego data frame'a, zawierającej nazwy zmiennych, dla których wyliczono statystyki
#' @details
#' W \code{x} muszą znajdować się zarówno zmienne, dla których wyliczona mają zostać statystyki, jak i zmienne grupujące, których nazwy podają parametry \code{grBezLacznie} i \code{grZLacznie}.
#' Jeśli oba argumenty \code{grBezLacznie} i \code{grZLacznie} został podane, dzielenia na grupy ze względu na \code{grZLacznie} zostanie przeprowadzone w ramach grup wyróżnionych ze względu na \code{grBezLacznie}.
#' @return data frame
#' @import plyr
#' @export
parametryEgz = function(x, grBezLacznie, grZLacznie, nazwaPierwKol=NA) {
  stopifnot(is.data.frame(x) | is.list(x),
            is.character(grBezLacznie ) | is.null(grBezLacznie ),
            is.character(grZLacznie   ) | is.null(grZLacznie   ),
            is.character(nazwaPierwKol) | all(is.na(nazwaPierwKol))
  )
  stopifnot(grBezLacznie %in% names(x),
            grZLacznie   %in% names(x)
  )
  if (!is.null(grBezLacznie)) stopifnot(length(grBezLacznie) == 1, grBezLacznie != "")
  if (!is.null(grZLacznie  )) stopifnot(length(grZLacznie  ) == 1, grZLacznie   != "")
  if (is.null(grBezLacznie)) {
    grBezLacznie = "grBezLacznie"
    x = cbind(x, grBezLacznie = 1)
  }
  if (is.null(grZLacznie)) {
    grZLacznie = "grZLacznie"
    x = cbind(x, grZLacznie = 1)
  }
  x = ldply(as.list(x[, !( names(x) %in% c(grBezLacznie, grZLacznie) ), drop=FALSE]),
            function(x, grBezLacznie, grZLacznie) {
              ddply(data.frame(x, grBezLacznie, grZLacznie), "grBezLacznie",
                    function(x) {
                      tempOg = data.frame(grZLacznie="łącznie",   as.list(mojeParametry(x[, !(names(x) %in% c("grBezLacznie", "grZLacznie"))])), check.names=FALSE)
                      if (length(unique(x$grZLacznie)) > 1) {
                        tempGr = ddply(x, "grZLacznie", function(x) {return(mojeParametry(x[, !(names(x) %in% c("grBezLacznie", "grZLacznie"))]))})
                        return(rbind.fill(list(tempOg, tempGr)))
                      } else {
                        return(tempOg)
                      }
                    })
            }, grBezLacznie=x[, grBezLacznie], grZLacznie=x[, grZLacznie],
            .id=nazwaPierwKol)
  if (length(unique(x$grBezLacznie)) == 1) x = x[, names(x) != "grBezLacznie"]
  if (length(unique(x$grZLacznie  )) == 1) x = x[, names(x) != "grZLacznie"  ]
  names(x) = sub("grBezLacznie", grBezLacznie, names(x))
  names(x) = sub("grZLacznie"  , grZLacznie  , names(x))
  return(x)
}
