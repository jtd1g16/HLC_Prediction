# load "ref0477-7782-39-0.gnu"
# chem = "D2"

set terminal postscript eps color
set title "ref = 477; chem = D2; casrn = 7782-39-0"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.7949240E-05 * exp(  -780.4319    *(1/   298.    -1/T))

set label "" at    283.1500    ,   0.9148552E-05 point
set label "" at    288.1500    ,   0.8711606E-05 point
set label "" at    293.1500    ,   0.8258275E-05 point
set label "" at    298.1500    ,   0.7974260E-05 point
set label "" at    303.1500    ,   0.7619241E-05 point
set label "" at    298.1500    ,   0.7949240E-05 point ps 2 pt 6

plot [280:310] H(T)
