# load "ref2463-67-66-3.gnu"
# chem = "trichloromethane"

set terminal postscript eps color
set title "ref = 2463; chem = trichloromethane; casrn = 67-66-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2216459E-02 * exp(  -4697.443    *(1/   298.    -1/T))

set label "" at    288.1500    ,   0.3829312E-02 point
set label "" at    298.1500    ,   0.2216459E-02 point
set label "" at    298.1500    ,   0.2216459E-02 point ps 2 pt 6

plot [280:300] H(T)
