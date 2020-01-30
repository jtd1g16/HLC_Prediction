# load "ref2931-156-60-5.gnu"
# chem = "($E$)-1,2-dichloroethene"

set terminal postscript eps color
set title "ref = 2931; chem = ($E$)-1,2-dichloroethene; casrn = 156-60-5"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.9474289E-03 * exp(  -4134.498    *(1/   298.    -1/T))

set label "" at    293.1500    ,   0.1252504E-02 point
set label "" at    303.1500    ,   0.7121796E-03 point
set label "" at    313.1500    ,   0.4782088E-03 point
set label "" at    323.1500    ,   0.3351159E-03 point
set label "" at    298.1500    ,   0.9474289E-03 point ps 2 pt 6

plot [290:330] H(T)
