# load "ref0599-127-18-4.gnu"
# chem = "tetrachloroethene"

set terminal postscript eps color
set title "ref = 599; chem = tetrachloroethene; casrn = 127-18-4"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.5572966E-03 * exp(  -3580.146    *(1/   298.    -1/T))

set label "" at    298.1500    ,   0.5805431E-03 point
set label "" at    303.1500    ,   0.4272395E-03 point
set label "" at    313.1500    ,   0.3257172E-03 point
set label "" at    318.1500    ,   0.2590350E-03 point
set label "" at    298.1500    ,   0.5572966E-03 point ps 2 pt 6

plot [290:320] H(T)
