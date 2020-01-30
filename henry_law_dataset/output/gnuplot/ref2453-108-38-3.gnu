# load "ref2453-108-38-3.gnu"
# chem = "1,3-dimethylbenzene"

set terminal postscript eps color
set title "ref = 2453; chem = 1,3-dimethylbenzene; casrn = 108-38-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1391958E-02 * exp(  -4694.312    *(1/   298.    -1/T))

set label "" at    288.1500    ,   0.2459640E-02 point
set label "" at    298.1500    ,   0.1363101E-02 point
set label "" at    308.1500    ,   0.8102769E-03 point
set label "" at    318.1500    ,   0.5321337E-03 point
set label "" at    298.1500    ,   0.1391958E-02 point ps 2 pt 6

plot [280:320] H(T)
