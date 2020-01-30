# load "ref2453-95-47-6.gnu"
# chem = "1,2-dimethylbenzene"

set terminal postscript eps color
set title "ref = 2453; chem = 1,2-dimethylbenzene; casrn = 95-47-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1937821E-02 * exp(  -4530.513    *(1/   298.    -1/T))

set label "" at    288.1500    ,   0.3333850E-02 point
set label "" at    298.1500    ,   0.1895271E-02 point
set label "" at    308.1500    ,   0.1179998E-02 point
set label "" at    318.1500    ,   0.7529512E-03 point
set label "" at    298.1500    ,   0.1937821E-02 point ps 2 pt 6

plot [280:320] H(T)
