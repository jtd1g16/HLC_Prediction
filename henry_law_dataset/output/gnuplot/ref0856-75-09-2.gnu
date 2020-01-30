# load "ref0856-75-09-2.gnu"
# chem = "dichloromethane"

set terminal postscript eps color
set title "ref = 856; chem = dichloromethane; casrn = 75-09-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.3859721E-02 * exp(  -3476.787    *(1/   298.    -1/T))

set label "" at    293.1500    ,   0.4699635E-02 point
set label "" at    303.1500    ,   0.3183623E-02 point
set label "" at    308.1500    ,   0.2667360E-02 point
set label "" at    313.1500    ,   0.2193163E-02 point
set label "" at    298.1500    ,   0.3859721E-02 point ps 2 pt 6

plot [290:320] H(T)
