# load "ref0477-7440-59-7.gnu"
# chem = "helium"

set terminal postscript eps color
set title "ref = 477; chem = helium; casrn = 7440-59-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.3828936E-05 * exp(  -119.9671    *(1/   298.    -1/T))

set label "" at    288.1500    ,   0.3890456E-05 point
set label "" at    293.1500    ,   0.3847307E-05 point
set label "" at    298.1500    ,   0.3821637E-05 point
set label "" at    303.1500    ,   0.3811259E-05 point
set label "" at    298.1500    ,   0.3828936E-05 point ps 2 pt 6

plot [280:310] H(T)
