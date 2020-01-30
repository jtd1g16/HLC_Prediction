# load "ref0857-108-88-3.gnu"
# chem = "methylbenzene"

set terminal postscript eps color
set title "ref = 857; chem = methylbenzene; casrn = 108-88-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1609873E-02 * exp(  -2549.048    *(1/   298.    -1/T))

set label "" at    313.1500    ,   0.1083085E-02 point
set label "" at    333.1500    ,   0.6389977E-03 point
set label "" at    343.1500    ,   0.5222376E-03 point
set label "" at    353.1500    ,   0.4325241E-03 point
set label "" at    298.1500    ,   0.1609873E-02 point ps 2 pt 6

plot [310:360] H(T)
