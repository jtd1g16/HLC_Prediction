# load "ref2901-107-06-2.gnu"
# chem = "1,2-dichloroethane"

set terminal postscript eps color
set title "ref = 2901; chem = 1,2-dichloroethane; casrn = 107-06-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.5824526E-02 * exp(  -3041.874    *(1/   298.    -1/T))

set label "" at    293.0500    ,   0.6956201E-02 point
set label "" at    302.9000    ,   0.4963369E-02 point
set label "" at    298.1500    ,   0.5824526E-02 point ps 2 pt 6

plot [290:310] H(T)
