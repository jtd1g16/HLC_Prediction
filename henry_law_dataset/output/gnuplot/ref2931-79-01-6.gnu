# load "ref2931-79-01-6.gnu"
# chem = "trichloroethene"

set terminal postscript eps color
set title "ref = 2931; chem = trichloroethene; casrn = 79-01-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.9675760E-03 * exp(  -4308.489    *(1/   298.    -1/T))

set label "" at    293.1500    ,   0.1283154E-02 point
set label "" at    303.1500    ,   0.7243487E-03 point
set label "" at    313.1500    ,   0.4798986E-03 point
set label "" at    323.1500    ,   0.3241567E-03 point
set label "" at    298.1500    ,   0.9675760E-03 point ps 2 pt 6

plot [290:330] H(T)
