# load "ref2485-75-73-0.gnu"
# chem = "tetrafluoromethane"

set terminal postscript eps color
set title "ref = 2485; chem = tetrafluoromethane; casrn = 75-73-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2162586E-05 * exp(  -1441.721    *(1/   298.    -1/T))

set label "" at    288.1900    ,   0.2670540E-05 point
set label "" at    298.1700    ,   0.2050322E-05 point
set label "" at    308.1500    ,   0.1792279E-05 point
set label "" at    318.2000    ,   0.1659698E-05 point
set label "" at    298.1500    ,   0.2162586E-05 point ps 2 pt 6

plot [280:320] H(T)
