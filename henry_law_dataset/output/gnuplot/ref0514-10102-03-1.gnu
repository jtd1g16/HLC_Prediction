# load "ref0514-10102-03-1.gnu"
# chem = "dinitrogen pentoxide"

set terminal postscript eps color
set title "ref = 514; chem = dinitrogen pentoxide; casrn = 10102-03-1"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2095277E-01 * exp(  -3362.310    *(1/   298.    -1/T))

set label "" at    230.0000    ,   0.5921540     point
set label "" at    273.0000    ,   0.5921540E-01 point
set label "" at    298.1500    ,   0.2095277E-01 point ps 2 pt 6

plot [230:280] H(T)
