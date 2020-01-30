# load "ref1149-7553-56-2.gnu"
# chem = "molecular iodine"

set terminal postscript eps color
set title "ref = 1149; chem = molecular iodine; casrn = 7553-56-2"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2776827E-01 * exp(  -4323.814    *(1/   298.    -1/T))

set label "" at    298.1500    ,   0.2823768E-01 point
set label "" at    313.1500    ,   0.1344254E-01 point
set label "" at    333.1500    ,   0.6137266E-02 point
set label "" at    298.1500    ,   0.2776827E-01 point ps 2 pt 6

plot [290:340] H(T)
