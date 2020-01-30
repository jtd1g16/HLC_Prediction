# load "ref1992-75-44-5.gnu"
# chem = "phosgene"

set terminal postscript eps color
set title "ref = 1992; chem = phosgene; casrn = 75-44-5"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.6771981E-03 * exp(  -4150.536    *(1/   298.    -1/T))

set label "" at    288.1500    ,   0.1075746E-02 point
set label "" at    298.1500    ,   0.6809771E-03 point
set label "" at    308.1500    ,   0.4539847E-03 point
set label "" at    318.6500    ,   0.2664693E-03 point
set label "" at    298.1500    ,   0.6771981E-03 point ps 2 pt 6

plot [280:320] H(T)
