# load "ref0857-141-78-6.gnu"
# chem = "ethyl ethanoate"

set terminal postscript eps color
set title "ref = 857; chem = ethyl ethanoate; casrn = 141-78-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.4362825E-01 * exp(  -3896.944    *(1/   298.    -1/T))

set label "" at    313.1500    ,   0.2396614E-01 point
set label "" at    333.1500    ,   0.1057776E-01 point
set label "" at    343.1500    ,   0.7640791E-02 point
set label "" at    353.1500    ,   0.5959978E-02 point
set label "" at    298.1500    ,   0.4362825E-01 point ps 2 pt 6

plot [310:360] H(T)
