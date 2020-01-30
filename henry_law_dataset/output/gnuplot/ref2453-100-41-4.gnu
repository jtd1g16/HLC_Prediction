# load "ref2453-100-41-4.gnu"
# chem = "ethylbenzene"

set terminal postscript eps color
set title "ref = 2453; chem = ethylbenzene; casrn = 100-41-4"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1259117E-02 * exp(  -4550.829    *(1/   298.    -1/T))

set label "" at    288.1500    ,   0.2170271E-02 point
set label "" at    298.1500    ,   0.1249253E-02 point
set label "" at    308.1500    ,   0.7438429E-03 point
set label "" at    318.1500    ,   0.4941242E-03 point
set label "" at    298.1500    ,   0.1259117E-02 point ps 2 pt 6

plot [280:320] H(T)
