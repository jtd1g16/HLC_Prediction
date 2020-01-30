# load "ref0857-127-18-4.gnu"
# chem = "tetrachloroethene"

set terminal postscript eps color
set title "ref = 857; chem = tetrachloroethene; casrn = 127-18-4"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.8137342E-03 * exp(  -2056.287    *(1/   298.    -1/T))

set label "" at    313.1500    ,   0.5684276E-03 point
set label "" at    333.1500    ,   0.4584899E-03 point
set label "" at    343.1500    ,   0.2733861E-03 point
set label "" at    353.1500    ,   0.2962961E-03 point
set label "" at    298.1500    ,   0.8137342E-03 point ps 2 pt 6

plot [310:360] H(T)
