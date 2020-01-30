# load "ref0857-110-82-7.gnu"
# chem = "cyclohexane"

set terminal postscript eps color
set title "ref = 857; chem = cyclohexane; casrn = 110-82-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.5447562E-04 * exp(  -3751.888    *(1/   298.    -1/T))

set label "" at    313.1500    ,   0.2688509E-04 point
set label "" at    333.1500    ,   0.1805078E-04 point
set label "" at    343.1500    ,   0.1051485E-04 point
set label "" at    353.1500    ,   0.6811404E-05 point
set label "" at    298.1500    ,   0.5447562E-04 point ps 2 pt 6

plot [310:360] H(T)
