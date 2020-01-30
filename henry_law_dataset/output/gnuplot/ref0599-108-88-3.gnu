# load "ref0599-108-88-3.gnu"
# chem = "methylbenzene"

set terminal postscript eps color
set title "ref = 599; chem = methylbenzene; casrn = 108-88-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1483336E-02 * exp(  -3382.152    *(1/   298.    -1/T))

set label "" at    298.1500    ,   0.1534873E-02 point
set label "" at    303.1500    ,   0.1177713E-02 point
set label "" at    313.1500    ,   0.8811815E-03 point
set label "" at    318.1500    ,   0.6950164E-03 point
set label "" at    323.1500    ,   0.6367247E-03 point
set label "" at    298.1500    ,   0.1483336E-02 point ps 2 pt 6

plot [290:330] H(T)
