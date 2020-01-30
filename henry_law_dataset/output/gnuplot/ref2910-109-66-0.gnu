# load "ref2910-109-66-0.gnu"
# chem = "pentane"

set terminal postscript eps color
set title "ref = 2910; chem = pentane; casrn = 109-66-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1107973E-04 * exp(  -2327.361    *(1/   298.    -1/T))

set label "" at    273.2000    ,   0.3074551E-04 point
set label "" at    283.2000    ,   0.1581197E-04 point
set label "" at    298.2000    ,   0.9072444E-05 point
set label "" at    313.2000    ,   0.6218192E-05 point
set label "" at    343.2000    ,   0.3714222E-05 point
set label "" at    373.2000    ,   0.2867456E-05 point
set label "" at    298.1500    ,   0.1107973E-04 point ps 2 pt 6

plot [270:380] H(T)
