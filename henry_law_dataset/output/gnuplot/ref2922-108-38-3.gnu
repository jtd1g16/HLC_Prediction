# load "ref2922-108-38-3.gnu"
# chem = "1,3-dimethylbenzene"

set terminal postscript eps color
set title "ref = 2922; chem = 1,3-dimethylbenzene; casrn = 108-38-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1827590E-02 * exp(  -4485.876    *(1/   298.    -1/T))

set label "" at    283.1500    ,   0.4120226E-02 point
set label "" at    288.1500    ,   0.3088723E-02 point
set label "" at    293.1500    ,   0.2420627E-02 point
set label "" at    298.1500    ,   0.1613582E-02 point
set label "" at    303.1500    ,   0.1547294E-02 point
set label "" at    298.1500    ,   0.1827590E-02 point ps 2 pt 6

plot [280:310] H(T)
