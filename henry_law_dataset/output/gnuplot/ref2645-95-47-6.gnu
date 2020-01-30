# load "ref2645-95-47-6.gnu"
# chem = "1,2-dimethylbenzene"

set terminal postscript eps color
set title "ref = 2645; chem = 1,2-dimethylbenzene; casrn = 95-47-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2027819E-02 * exp(  -4279.145    *(1/   298.    -1/T))

set label "" at    278.1500    ,   0.5689488E-02 point
set label "" at    288.1500    ,   0.3339160E-02 point
set label "" at    298.1500    ,   0.2027113E-02 point
set label "" at    298.1500    ,   0.2027819E-02 point ps 2 pt 6

plot [270:300] H(T)
