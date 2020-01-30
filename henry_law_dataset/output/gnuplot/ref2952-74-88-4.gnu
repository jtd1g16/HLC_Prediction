# load "ref2952-74-88-4.gnu"
# chem = "iodomethane"

set terminal postscript eps color
set title "ref = 2952; chem = iodomethane; casrn = 74-88-4"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2005600E-02 * exp(  -3092.361    *(1/   298.    -1/T))

set label "" at    283.1500    ,   0.3539713E-02 point
set label "" at    288.1500    ,   0.2782633E-02 point
set label "" at    293.1500    ,   0.2413387E-02 point
set label "" at    298.1500    ,   0.2016977E-02 point
set label "" at    298.1500    ,   0.2005600E-02 point ps 2 pt 6

plot [280:300] H(T)
