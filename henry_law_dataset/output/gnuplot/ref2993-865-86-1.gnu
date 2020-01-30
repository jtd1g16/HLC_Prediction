# load "ref2993-865-86-1.gnu"
# chem = "10:2 FTOH"

set terminal postscript eps color
set title "ref = 2993; chem = 10:2 FTOH; casrn = 865-86-1"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1040488E-03 * exp(  -9565.967    *(1/   298.    -1/T))

set label "" at    278.1500    ,   0.1111043E-02 point
set label "" at    288.1500    ,   0.2790815E-03 point
set label "" at    298.1500    ,   0.1111043E-03 point
set label "" at    298.1500    ,   0.1040488E-03 point ps 2 pt 6

plot [270:300] H(T)
