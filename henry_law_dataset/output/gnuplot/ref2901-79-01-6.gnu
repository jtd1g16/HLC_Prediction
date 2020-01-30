# load "ref2901-79-01-6.gnu"
# chem = "trichloroethene"

set terminal postscript eps color
set title "ref = 2901; chem = trichloroethene; casrn = 79-01-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.9738052E-03 * exp(  -2000.819    *(1/   298.    -1/T))

set label "" at    293.0500    ,   0.1094442E-02 point
set label "" at    302.9000    ,   0.8765332E-03 point
set label "" at    298.1500    ,   0.9738052E-03 point ps 2 pt 6

plot [290:310] H(T)
