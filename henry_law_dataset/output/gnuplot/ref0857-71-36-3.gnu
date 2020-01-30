# load "ref0857-71-36-3.gnu"
# chem = "1-butanol"

set terminal postscript eps color
set title "ref = 857; chem = 1-butanol; casrn = 71-36-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.6066588     * exp(  -5575.180    *(1/   298.    -1/T))

set label "" at    313.1500    ,   0.2484950     point
set label "" at    333.1500    ,   0.8592172E-01 point
set label "" at    343.1500    ,   0.5047128E-01 point
set label "" at    353.1500    ,   0.3368239E-01 point
set label "" at    298.1500    ,   0.6066588     point ps 2 pt 6

plot [310:360] H(T)
