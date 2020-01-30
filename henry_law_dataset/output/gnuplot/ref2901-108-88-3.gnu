# load "ref2901-108-88-3.gnu"
# chem = "methylbenzene"

set terminal postscript eps color
set title "ref = 2901; chem = methylbenzene; casrn = 108-88-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1488278E-02 * exp(  -6472.521    *(1/   298.    -1/T))

set label "" at    293.0500    ,   0.2171512E-02 point
set label "" at    302.9000    ,   0.1058852E-02 point
set label "" at    298.1500    ,   0.1488278E-02 point ps 2 pt 6

plot [290:310] H(T)
