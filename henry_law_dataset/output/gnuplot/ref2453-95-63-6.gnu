# load "ref2453-95-63-6.gnu"
# chem = "1,2,4-trimethylbenzene"

set terminal postscript eps color
set title "ref = 2453; chem = 1,2,4-trimethylbenzene; casrn = 95-63-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1625976E-02 * exp(  -4787.824    *(1/   298.    -1/T))

set label "" at    288.1500    ,   0.2867456E-02 point
set label "" at    298.1500    ,   0.1608776E-02 point
set label "" at    308.1500    ,   0.9558188E-03 point
set label "" at    318.1500    ,   0.5989384E-03 point
set label "" at    298.1500    ,   0.1625976E-02 point ps 2 pt 6

plot [280:320] H(T)
