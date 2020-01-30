# load "ref2901-67-66-3.gnu"
# chem = "trichloromethane"

set terminal postscript eps color
set title "ref = 2901; chem = trichloromethane; casrn = 67-66-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2385583E-02 * exp(  -2009.396    *(1/   298.    -1/T))

set label "" at    293.0500    ,   0.2682456E-02 point
set label "" at    302.9000    ,   0.2146322E-02 point
set label "" at    298.1500    ,   0.2385583E-02 point ps 2 pt 6

plot [290:310] H(T)
