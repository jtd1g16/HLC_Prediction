# load "ref0599-71-43-2.gnu"
# chem = "benzene"

set terminal postscript eps color
set title "ref = 599; chem = benzene; casrn = 71-43-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1867921E-02 * exp(  -3768.229    *(1/   298.    -1/T))

set label "" at    298.1500    ,   0.1869173E-02 point
set label "" at    303.1500    ,   0.1459946E-02 point
set label "" at    313.1500    ,   0.1124058E-02 point
set label "" at    318.1500    ,   0.8089535E-03 point
set label "" at    323.1500    ,   0.6901561E-03 point
set label "" at    298.1500    ,   0.1867921E-02 point ps 2 pt 6

plot [290:330] H(T)
