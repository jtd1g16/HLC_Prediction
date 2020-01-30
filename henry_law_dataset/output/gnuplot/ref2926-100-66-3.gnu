# load "ref2926-100-66-3.gnu"
# chem = "methoxybenzene"

set terminal postscript eps color
set title "ref = 2926; chem = methoxybenzene; casrn = 100-66-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2884162E-01 * exp(  -4160.853    *(1/   298.    -1/T))

set label "" at    280.9500    ,   0.6627774E-01 point
set label "" at    293.1600    ,   0.3739318E-01 point
set label "" at    308.1500    ,   0.1875997E-01 point
set label "" at    322.9000    ,   0.9675159E-02 point
set label "" at    298.1500    ,   0.2884162E-01 point ps 2 pt 6

plot [280:330] H(T)
