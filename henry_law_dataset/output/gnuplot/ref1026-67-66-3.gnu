# load "ref1026-67-66-3.gnu"
# chem = "trichloromethane"

set terminal postscript eps color
set title "ref = 1026; chem = trichloromethane; casrn = 67-66-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1961536E-02 * exp(  -4640.310    *(1/   298.    -1/T))

set label "" at    278.1500    ,   0.6005571E-02 point
set label "" at    283.1500    ,   0.4471216E-02 point
set label "" at    288.1500    ,   0.3366089E-02 point
set label "" at    293.1500    ,   0.2564224E-02 point
set label "" at    298.1500    ,   0.1958230E-02 point
set label "" at    298.1500    ,   0.1961536E-02 point ps 2 pt 6

plot [270:300] H(T)
