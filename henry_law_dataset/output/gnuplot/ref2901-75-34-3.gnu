# load "ref2901-75-34-3.gnu"
# chem = "1,1-dichloroethane"

set terminal postscript eps color
set title "ref = 2901; chem = 1,1-dichloroethane; casrn = 75-34-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1652583E-02 * exp(  -2076.457    *(1/   298.    -1/T))

set label "" at    293.0500    ,   0.1865527E-02 point
set label "" at    302.9000    ,   0.1481603E-02 point
set label "" at    298.1500    ,   0.1652583E-02 point ps 2 pt 6

plot [290:310] H(T)
