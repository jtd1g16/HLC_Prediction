# load "ref0791-108-88-3.gnu"
# chem = "methylbenzene"

set terminal postscript eps color
set title "ref = 791; chem = methylbenzene; casrn = 108-88-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1676481E-02 * exp(  -5895.841    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.1021534E-01 point
set label "" at    286.1500    ,   0.3866873E-02 point
set label "" at    296.1500    ,   0.1908763E-02 point
set label "" at    298.1500    ,   0.1676481E-02 point ps 2 pt 6

plot [270:300] H(T)
