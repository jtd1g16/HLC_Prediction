# load "ref2610-75-73-0.gnu"
# chem = "tetrafluoromethane"

set terminal postscript eps color
set title "ref = 2610; chem = tetrafluoromethane; casrn = 75-73-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1006518E-05 * exp(   840.0000    *(1/   298.    -1/T))

set label "" at    280.0000    ,   0.8385097E-06 point
set label "" at    300.0000    ,   0.1024158E-05 point
set label "" at    298.1500    ,   0.1006518E-05 point ps 2 pt 6

plot [280:300] H(T)
