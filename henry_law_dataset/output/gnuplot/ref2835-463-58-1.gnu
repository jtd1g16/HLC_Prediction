# load "ref2835-463-58-1.gnu"
# chem = "carbon oxide sulfide"

set terminal postscript eps color
set title "ref = 2835; chem = carbon oxide sulfide; casrn = 463-58-1"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1990578E-03 * exp(  -3488.327    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.5831335E-03 point
set label "" at    283.1500    ,   0.3669155E-03 point
set label "" at    293.1500    ,   0.2440643E-03 point
set label "" at    298.1500    ,   0.1990578E-03 point ps 2 pt 6

plot [270:300] H(T)
