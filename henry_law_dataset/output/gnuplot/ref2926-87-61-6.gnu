# load "ref2926-87-61-6.gnu"
# chem = "1,2,3-trichlorobenzene"

set terminal postscript eps color
set title "ref = 2926; chem = 1,2,3-trichlorobenzene; casrn = 87-61-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.6339958E-02 * exp(  -4569.558    *(1/   298.    -1/T))

set label "" at    280.9500    ,   0.1647081E-01 point
set label "" at    293.0600    ,   0.8090922E-02 point
set label "" at    308.1400    ,   0.3843188E-02 point
set label "" at    322.9900    ,   0.1969463E-02 point
set label "" at    298.1500    ,   0.6339958E-02 point ps 2 pt 6

plot [280:330] H(T)
