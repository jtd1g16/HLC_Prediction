# load "ref2984-100-41-4.gnu"
# chem = "ethylbenzene"

set terminal postscript eps color
set title "ref = 2984; chem = ethylbenzene; casrn = 100-41-4"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1377041E-02 * exp(  -4500.566    *(1/   298.    -1/T))

set label "" at    288.1500    ,   0.2385427E-02 point
set label "" at    298.1500    ,   0.1339998E-02 point
set label "" at    308.1500    ,   0.8223166E-03 point
set label "" at    318.1500    ,   0.5479397E-03 point
set label "" at    298.1500    ,   0.1377041E-02 point ps 2 pt 6

plot [280:320] H(T)
