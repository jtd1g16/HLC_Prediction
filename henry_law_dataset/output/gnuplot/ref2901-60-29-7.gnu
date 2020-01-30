# load "ref2901-60-29-7.gnu"
# chem = "diethyl ether"

set terminal postscript eps color
set title "ref = 2901; chem = diethyl ether; casrn = 60-29-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.6959423E-02 * exp(  -3887.694    *(1/   298.    -1/T))

set label "" at    293.0500    ,   0.8732252E-02 point
set label "" at    302.9000    ,   0.5672422E-02 point
set label "" at    298.1500    ,   0.6959423E-02 point ps 2 pt 6

plot [290:310] H(T)
