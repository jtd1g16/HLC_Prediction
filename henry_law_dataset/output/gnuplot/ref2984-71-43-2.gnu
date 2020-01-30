# load "ref2984-71-43-2.gnu"
# chem = "benzene"

set terminal postscript eps color
set title "ref = 2984; chem = benzene; casrn = 71-43-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1656135E-02 * exp(  -3532.658    *(1/   298.    -1/T))

set label "" at    288.1500    ,   0.2515541E-02 point
set label "" at    298.1500    ,   0.1632505E-02 point
set label "" at    308.1500    ,   0.1136384E-02 point
set label "" at    318.1500    ,   0.7861067E-03 point
set label "" at    298.1500    ,   0.1656135E-02 point ps 2 pt 6

plot [280:320] H(T)
