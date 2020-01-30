# load "ref0903-56-23-5.gnu"
# chem = "tetrachloromethane"

set terminal postscript eps color
set title "ref = 903; chem = tetrachloromethane; casrn = 56-23-5"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.3306664E-03 * exp(  -3557.366    *(1/   298.    -1/T))

set label "" at    300.7500    ,   0.3018412E-03 point
set label "" at    308.1500    ,   0.2197802E-03 point
set label "" at    318.1500    ,   0.1576541E-03 point
set label "" at    298.1500    ,   0.3306664E-03 point ps 2 pt 6

plot [300:320] H(T)
