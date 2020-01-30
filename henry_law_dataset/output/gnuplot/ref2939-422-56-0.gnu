# load "ref2939-422-56-0.gnu"
# chem = "CF3CF2CHCl2"

set terminal postscript eps color
set title "ref = 2939; chem = CF3CF2CHCl2; casrn = 422-56-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.9811005E-04 * exp(  -3505.029    *(1/   298.    -1/T))

set label "" at    298.0000    ,   0.9869233E-04 point
set label "" at    353.0000    ,   0.1579077E-04 point
set label "" at    298.1500    ,   0.9811005E-04 point ps 2 pt 6

plot [290:360] H(T)
