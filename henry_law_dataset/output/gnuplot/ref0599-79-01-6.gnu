# load "ref0599-79-01-6.gnu"
# chem = "trichloroethene"

set terminal postscript eps color
set title "ref = 599; chem = trichloroethene; casrn = 79-01-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.9549246E-03 * exp(  -3510.151    *(1/   298.    -1/T))

set label "" at    298.1500    ,   0.9581779E-03 point
set label "" at    303.1500    ,   0.7533765E-03 point
set label "" at    313.1500    ,   0.5981353E-03 point
set label "" at    318.1500    ,   0.4405907E-03 point
set label "" at    323.1500    ,   0.3752560E-03 point
set label "" at    298.1500    ,   0.9549246E-03 point ps 2 pt 6

plot [290:330] H(T)
