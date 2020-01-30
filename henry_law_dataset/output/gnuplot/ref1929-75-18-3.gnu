# load "ref1929-75-18-3.gnu"
# chem = "dimethyl sulfide"

set terminal postscript eps color
set title "ref = 1929; chem = dimethyl sulfide; casrn = 75-18-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.5168415E-02 * exp(  -3585.139    *(1/   298.    -1/T))

set label "" at    292.6000    ,   0.6588323E-02 point
set label "" at    302.7000    ,   0.4257070E-02 point
set label "" at    312.5000    ,   0.2943719E-02 point
set label "" at    332.8000    ,   0.1491696E-02 point
set label "" at    298.1500    ,   0.5168415E-02 point ps 2 pt 6

plot [290:340] H(T)
