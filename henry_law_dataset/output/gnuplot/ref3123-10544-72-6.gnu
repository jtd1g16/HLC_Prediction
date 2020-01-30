# load "ref3123-10544-72-6.gnu"
# chem = "dinitrogen tetroxide"

set terminal postscript eps color
set title "ref = 3123; chem = dinitrogen tetroxide; casrn = 10544-72-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1289134E-01 * exp(  -1086.768    *(1/   298.    -1/T))

set label "" at    293.1500    ,   0.1371823E-01 point
set label "" at    303.1500    ,   0.1213916E-01 point
set label "" at    298.1500    ,   0.1289134E-01 point ps 2 pt 6

plot [290:310] H(T)
