# load "ref2463-108-88-3.gnu"
# chem = "methylbenzene"

set terminal postscript eps color
set title "ref = 2463; chem = methylbenzene; casrn = 108-88-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1365664E-02 * exp(  -4142.510    *(1/   298.    -1/T))

set label "" at    284.1500    ,   0.2662080E-02 point
set label "" at    288.1500    ,   0.2173932E-02 point
set label "" at    298.1500    ,   0.1477639E-02 point
set label "" at    308.1500    ,   0.8322060E-03 point
set label "" at    298.1500    ,   0.1365664E-02 point ps 2 pt 6

plot [280:310] H(T)
