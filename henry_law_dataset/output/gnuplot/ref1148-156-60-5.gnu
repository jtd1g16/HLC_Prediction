# load "ref1148-156-60-5.gnu"
# chem = "(E)-1,2-dichloroethene"

set terminal postscript eps color
set title "ref = 1148; chem = (E)-1,2-dichloroethene; casrn = 156-60-5"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.9885780E-03 * exp(  -4311.996    *(1/   298.    -1/T))

set label "" at    293.1500    ,   0.1311242E-02 point
set label "" at    303.1500    ,   0.7399240E-03 point
set label "" at    313.1500    ,   0.4900768E-03 point
set label "" at    323.1500    ,   0.3308746E-03 point
set label "" at    298.1500    ,   0.9885780E-03 point ps 2 pt 6

plot [290:330] H(T)
