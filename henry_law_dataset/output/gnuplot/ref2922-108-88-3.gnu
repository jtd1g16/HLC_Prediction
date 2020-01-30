# load "ref2922-108-88-3.gnu"
# chem = "methylbenzene"

set terminal postscript eps color
set title "ref = 2922; chem = methylbenzene; casrn = 108-88-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1465447E-02 * exp(  -4701.051    *(1/   298.    -1/T))

set label "" at    283.1500    ,   0.3440601E-02 point
set label "" at    288.1500    ,   0.2504370E-02 point
set label "" at    293.1500    ,   0.1887269E-02 point
set label "" at    298.1500    ,   0.1452224E-02 point
set label "" at    303.1500    ,   0.1150552E-02 point
set label "" at    298.1500    ,   0.1465447E-02 point ps 2 pt 6

plot [280:310] H(T)
