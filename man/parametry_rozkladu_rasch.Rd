% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/przeksztalc_skale_zmiennych.R
\name{parametry_rozkladu_rasch}
\alias{parametry_rozkladu_rasch}
\title{Wyliczanie EWD}
\usage{
parametry_rozkladu_rasch(dane, czesciEgzWy = "")
}
\arguments{
\item{dane}{data frame z danymi, typowo wczytany do R wynik działania
funkcji \code{\link[EWDdane]{przygotuj_dane_do_ewd}}}

\item{czesciEgzWy}{wektor ciągów znaków, pozwala podać, zmienne związane
z jakimi konstruktami mają zostać pominięte}
}
\value{
data frame
}
\description{
Funkcja oblicza średnie i odchylenia standardowe zmiennych z wynikami
egzaminóW wyskalowanymi modelami rascha. Typowo na potrzeby jednorocznych
modeli maturalnych, gdzie zachodzi ten problem, że wyniki egzaminu
gimnazjalnego nie są wystandaryzowane względem poszczególnych typów szkół
ponadgimnazjalnych.

Pod uwagę brani są przy tym jedynie uczniowie o standardowej długości toku
kształcenia.
}
