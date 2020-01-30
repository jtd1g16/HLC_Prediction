# load "ref2905-56-23-5.gnu"
# chem = "tetrachloromethane"

set terminal postscript eps color
set title "ref = 2905; chem = tetrachloromethane; casrn = 56-23-5"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.3931460E-03 * exp(  -2626.244    *(1/   298.    -1/T))

set label "" at    288.1500    ,   0.5431535E-03 point
set label "" at    298.1500    ,   0.3791469E-03 point
set label "" at    308.1500    ,   0.3009872E-03 point
set label "" at    298.1500    ,   0.3931460E-03 point ps 2 pt 6

plot [280:310] H(T)
