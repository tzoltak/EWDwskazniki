.onAttach = function(libname, pkgname){
  packageStartupMessage(paste0(
    '\nJeśli korzystasz z danych udostępnianych przez pakiet EWDdane, zacytuj proszę:\n\n',
    'Żółtak T. (2015).',
    '„Statystyczne modelowanie wskaźników Edukacyjnej Wartości Dodanej. Podsumowanie polskich doświadczeń 2005-2015”.',
    ' Warszawa: Instytut Badań Edukacyjnych.'
  ))
  return(invisible(NULL))
}
