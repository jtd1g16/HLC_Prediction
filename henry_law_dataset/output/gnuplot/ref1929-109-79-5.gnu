# load "ref1929-109-79-5.gnu"
# chem = "1-butanethiol"

set terminal postscript eps color
set title "ref = 1929; chem = 1-butanethiol; casrn = 109-79-5"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1473271E-02 * exp(  -3557.836    *(1/   298.    -1/T))

set label "" at    292.8000    ,   0.1888802E-02 point
set label "" at    312.8000    ,   0.7894709E-03 point
set label "" at    332.8000    ,   0.4402698E-03 point
set label "" at    298.1500    ,   0.1473271E-02 point ps 2 pt 6

plot [290:340] H(T)
