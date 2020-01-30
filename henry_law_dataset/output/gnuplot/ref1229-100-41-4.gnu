# load "ref1229-100-41-4.gnu"
# chem = "ethylbenzene"

set terminal postscript eps color
set title "ref = 1229; chem = ethylbenzene; casrn = 100-41-4"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1271063E-02 * exp(  -5278.199    *(1/   298.    -1/T))

set label "" at    283.1500    ,   0.3267958E-02 point
set label "" at    288.1500    ,   0.2338681E-02 point
set label "" at    293.1500    ,   0.1716388E-02 point
set label "" at    298.1500    ,   0.1258831E-02 point
set label "" at    303.1500    ,   0.9581779E-03 point
set label "" at    298.1500    ,   0.1271063E-02 point ps 2 pt 6

plot [280:310] H(T)
