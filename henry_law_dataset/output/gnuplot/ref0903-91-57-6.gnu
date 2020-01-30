# load "ref0903-91-57-6.gnu"
# chem = "2-methylnaphthalene"

set terminal postscript eps color
set title "ref = 903; chem = 2-methylnaphthalene; casrn = 91-57-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.5017542E-04 * exp(  -1233.679    *(1/   298.    -1/T))

set label "" at    299.1500    ,   0.4934616E-04 point
set label "" at    308.9500    ,   0.4366812E-04 point
set label "" at    319.1500    ,   0.3810540E-04 point
set label "" at    298.1500    ,   0.5017542E-04 point ps 2 pt 6

plot [290:320] H(T)
