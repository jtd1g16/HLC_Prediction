# load "ref3116-95-47-6.gnu"
# chem = "1,2-dimethylbenzene"

set terminal postscript eps color
set title "ref = 3116; chem = 1,2-dimethylbenzene; casrn = 95-47-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1682298E-02 * exp(  -2541.189    *(1/   298.    -1/T))

set label "" at    313.0000    ,   0.1158750E-02 point
set label "" at    323.0000    ,   0.8540418E-03 point
set label "" at    333.0000    ,   0.6526169E-03 point
set label "" at    343.0000    ,   0.5776817E-03 point
set label "" at    298.1500    ,   0.1682298E-02 point ps 2 pt 6

plot [310:350] H(T)
