# load "ref0857-95-47-6.gnu"
# chem = "1,2-dimethylbenzene"

set terminal postscript eps color
set title "ref = 857; chem = 1,2-dimethylbenzene; casrn = 95-47-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1436911E-02 * exp(  -2975.389    *(1/   298.    -1/T))

set label "" at    313.1500    ,   0.9371374E-03 point
set label "" at    333.1500    ,   0.4729305E-03 point
set label "" at    343.1500    ,   0.3540000E-03 point
set label "" at    353.1500    ,   0.3371645E-03 point
set label "" at    298.1500    ,   0.1436911E-02 point ps 2 pt 6

plot [310:360] H(T)
