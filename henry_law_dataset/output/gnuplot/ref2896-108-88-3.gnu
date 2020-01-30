# load "ref2896-108-88-3.gnu"
# chem = "methylbenzene"

set terminal postscript eps color
set title "ref = 2896; chem = methylbenzene; casrn = 108-88-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1884148E-02 * exp(  -4026.353    *(1/   298.    -1/T))

set label "" at    303.1500    ,   0.1580646E-02 point
set label "" at    313.1500    ,   0.9144588E-03 point
set label "" at    323.1500    ,   0.6670025E-03 point
set label "" at    333.1500    ,   0.4664285E-03 point
set label "" at    298.1500    ,   0.1884148E-02 point ps 2 pt 6

plot [300:340] H(T)
