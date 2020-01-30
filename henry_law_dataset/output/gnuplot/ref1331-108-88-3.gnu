# load "ref1331-108-88-3.gnu"
# chem = "methylbenzene"

set terminal postscript eps color
set title "ref = 1331; chem = methylbenzene; casrn = 108-88-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1158936E-02 * exp(  -5370.392    *(1/   298.    -1/T))

set label "" at    319.1500    ,   0.3589068E-03 point
set label "" at    314.6500    ,   0.4393583E-03 point
set label "" at    310.0500    ,   0.5877467E-03 point
set label "" at    298.1500    ,   0.1158936E-02 point ps 2 pt 6

plot [310:320] H(T)
