
akk <- c("124 Fake st. Lancaster PA 55417",
         "new city Pennsylvania",
         "1200 rand st somecity PA",
         "1200 rand st anothertown Pa 55279",
         "1200 Pennsylvania Ave New York 23589",
         "163 Pan Ave New York 23589",
         "173 east suite 1200 Green Bay Wisconsin",
         "111 street st hometown Pennsylvania 55434",
         "163 pa street denver co")

akk[str_detect(akk, "([PApa]{2}(\\s\\d{1,5})?$)|(Pennsylvania(\\s\\d{1,5})?$)")]
