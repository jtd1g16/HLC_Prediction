# load "ref2926-95-73-8.gnu"
# chem = "1-methyl-2,4-dichlorobenzene"

set terminal postscript eps color
set title "ref = 2926; chem = 1-methyl-2,4-dichlorobenzene; casrn = 95-73-8"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2704616E-02 * exp(  -4947.140    *(1/   298.    -1/T))

set label "" at    280.9400    ,   0.7750968E-02 point
set label "" at    293.0900    ,   0.3294161E-02 point
set label "" at    308.1500    ,   0.1702828E-02 point
set label "" at    323.1000    ,   0.7339776E-03 point
set label "" at    298.1500    ,   0.2704616E-02 point ps 2 pt 6

plot [280:330] H(T)
