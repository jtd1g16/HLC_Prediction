# load "ref0379-10024-97-2.gnu"
# chem = "dinitrogen monoxide"

set terminal postscript eps color
set title "ref = 379; chem = dinitrogen monoxide; casrn = 10024-97-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2366929E-03 * exp(  -2774.059    *(1/   298.    -1/T))

set label "" at    283.1500    ,   0.3874782E-03 point
set label "" at    293.1500    ,   0.2773992E-03 point
set label "" at    298.1500    ,   0.2366929E-03 point ps 2 pt 6

plot [280:300] H(T)
