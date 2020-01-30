# load "ref0855-56-23-5.gnu"
# chem = "tetrachloromethane"

set terminal postscript eps color
set title "ref = 855; chem = tetrachloromethane; casrn = 56-23-5"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2927002E-03 * exp(  -4215.319    *(1/   298.    -1/T))

set label "" at    293.1500    ,   0.3764756E-03 point
set label "" at    303.1500    ,   0.2268111E-03 point
set label "" at    313.1500    ,   0.1503856E-03 point
set label "" at    298.1500    ,   0.2927002E-03 point ps 2 pt 6

plot [290:320] H(T)
