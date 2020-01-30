# load "ref0477-7446-09-5.gnu"
# chem = "sulfur dioxide"

set terminal postscript eps color
set title "ref = 477; chem = sulfur dioxide; casrn = 7446-09-5"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1344933E-01 * exp(  -2884.623    *(1/   298.    -1/T))

set label "" at    288.1500    ,   0.1884329E-01 point
set label "" at    293.1500    ,   0.1583928E-01 point
set label "" at    298.1500    ,   0.1343608E-01 point
set label "" at    303.1500    ,   0.1146983E-01 point
set label "" at    308.1500    ,   0.9831279E-02 point
set label "" at    298.1500    ,   0.1344933E-01 point ps 2 pt 6

plot [280:310] H(T)
