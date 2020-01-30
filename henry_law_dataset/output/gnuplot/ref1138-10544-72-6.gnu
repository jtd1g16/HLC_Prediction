# load "ref1138-10544-72-6.gnu"
# chem = "dinitrogen tetroxide"

set terminal postscript eps color
set title "ref = 1138; chem = dinitrogen tetroxide; casrn = 10544-72-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1629718E-01 * exp(  -3473.526    *(1/   298.    -1/T))

set label "" at    298.1500    ,   0.1629718E-01 point
set label "" at    288.1500    ,   0.2441761E-01 point
set label "" at    298.1500    ,   0.1629718E-01 point ps 2 pt 6

plot [280:300] H(T)
