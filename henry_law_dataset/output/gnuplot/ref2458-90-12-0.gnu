# load "ref2458-90-12-0.gnu"
# chem = "1-methylnaphthalene"

set terminal postscript eps color
set title "ref = 2458; chem = 1-methylnaphthalene; casrn = 90-12-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2763401E-01 * exp(  -4858.255    *(1/   298.    -1/T))

set label "" at    298.4500    ,   0.2861229E-01 point
set label "" at    289.7500    ,   0.4565991E-01 point
set label "" at    281.5500    ,   0.7091178E-01 point
set label "" at    303.4500    ,   0.2021384E-01 point
set label "" at    296.2500    ,   0.2963673E-01 point
set label "" at    298.1500    ,   0.2763401E-01 point ps 2 pt 6

plot [280:310] H(T)
