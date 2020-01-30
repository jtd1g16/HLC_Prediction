# load "ref2993-678-39-7.gnu"
# chem = "8:2 FTOH"

set terminal postscript eps color
set title "ref = 2993; chem = 8:2 FTOH; casrn = 678-39-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.3832958E-03 * exp(  -8607.162    *(1/   298.    -1/T))

set label "" at    278.1500    ,   0.3204284E-02 point
set label "" at    288.1500    ,   0.9456513E-03 point
set label "" at    298.1500    ,   0.4033955E-03 point
set label "" at    298.1500    ,   0.3832958E-03 point ps 2 pt 6

plot [270:300] H(T)
