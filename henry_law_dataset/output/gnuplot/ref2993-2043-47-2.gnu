# load "ref2993-2043-47-2.gnu"
# chem = "4:2 FTOH"

set terminal postscript eps color
set title "ref = 2993; chem = 4:2 FTOH; casrn = 2043-47-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.7213584E-02 * exp(  -7165.643    *(1/   298.    -1/T))

set label "" at    278.1500    ,   0.4127918E-01 point
set label "" at    288.1500    ,   0.1605946E-01 point
set label "" at    298.1500    ,   0.7340591E-02 point
set label "" at    298.1500    ,   0.7213584E-02 point ps 2 pt 6

plot [270:300] H(T)
