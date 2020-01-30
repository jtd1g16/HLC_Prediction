# load "ref2929-7783-06-4.gnu"
# chem = "hydrogen sulfide"

set terminal postscript eps color
set title "ref = 2929; chem = hydrogen sulfide; casrn = 7783-06-4"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.9139088E-03 * exp(  -1672.926    *(1/   298.    -1/T))

set label "" at    298.1500    ,   0.9310597E-03 point
set label "" at    313.1500    ,   0.6750501E-03 point
set label "" at    333.1500    ,   0.5148270E-03 point
set label "" at    298.1500    ,   0.9139088E-03 point ps 2 pt 6

plot [290:340] H(T)
