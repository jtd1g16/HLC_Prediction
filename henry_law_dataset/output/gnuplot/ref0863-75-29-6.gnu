# load "ref0863-75-29-6.gnu"
# chem = "2-chloropropane"

set terminal postscript eps color
set title "ref = 863; chem = 2-chloropropane; casrn = 75-29-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.5581417E-03 * exp(  -4346.366    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.2198983E-02 point
set label "" at    283.1500    ,   0.1181168E-02 point
set label "" at    293.1500    ,   0.6659776E-03 point
set label "" at    303.1500    ,   0.4649278E-03 point
set label "" at    298.1500    ,   0.5581417E-03 point ps 2 pt 6

plot [270:310] H(T)
