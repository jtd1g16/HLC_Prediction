# load "ref0863-540-54-5.gnu"
# chem = "1-chloropropane"

set terminal postscript eps color
set title "ref = 863; chem = 1-chloropropane; casrn = 540-54-5"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.7710190E-03 * exp(  -4437.565    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.3116273E-02 point
set label "" at    283.1500    ,   0.1671227E-02 point
set label "" at    293.1500    ,   0.9172899E-03 point
set label "" at    303.1500    ,   0.6408464E-03 point
set label "" at    298.1500    ,   0.7710190E-03 point ps 2 pt 6

plot [270:310] H(T)
