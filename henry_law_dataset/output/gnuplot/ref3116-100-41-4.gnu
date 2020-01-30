# load "ref3116-100-41-4.gnu"
# chem = "ethylbenzene"

set terminal postscript eps color
set title "ref = 3116; chem = ethylbenzene; casrn = 100-41-4"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1170203E-02 * exp(  -2660.269    *(1/   298.    -1/T))

set label "" at    313.0000    ,   0.7671460E-03 point
set label "" at    323.0000    ,   0.5976448E-03 point
set label "" at    333.0000    ,   0.4448707E-03 point
set label "" at    343.0000    ,   0.3709243E-03 point
set label "" at    298.1500    ,   0.1170203E-02 point ps 2 pt 6

plot [310:350] H(T)
