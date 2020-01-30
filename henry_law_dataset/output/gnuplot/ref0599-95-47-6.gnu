# load "ref0599-95-47-6.gnu"
# chem = "1,2-dimethylbenzene"

set terminal postscript eps color
set title "ref = 599; chem = 1,2-dimethylbenzene; casrn = 95-47-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1872226E-02 * exp(  -3400.779    *(1/   298.    -1/T))

set label "" at    298.1500    ,   0.1977802E-02 point
set label "" at    303.1500    ,   0.1571534E-02 point
set label "" at    313.1500    ,   0.9054342E-03 point
set label "" at    318.1500    ,   0.9310597E-03 point
set label "" at    323.1500    ,   0.8507959E-03 point
set label "" at    298.1500    ,   0.1872226E-02 point ps 2 pt 6

plot [290:330] H(T)
