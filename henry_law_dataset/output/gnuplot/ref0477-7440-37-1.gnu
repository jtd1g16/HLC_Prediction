# load "ref0477-7440-37-1.gnu"
# chem = "argon"

set terminal postscript eps color
set title "ref = 477; chem = argon; casrn = 7440-37-1"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1383466E-04 * exp(  -1477.437    *(1/   298.    -1/T))

set label "" at    288.1500    ,   0.1652201E-04 point
set label "" at    293.1500    ,   0.1500909E-04 point
set label "" at    298.1500    ,   0.1375833E-04 point
set label "" at    303.1500    ,   0.1271512E-04 point
set label "" at    308.1500    ,   0.1184669E-04 point
set label "" at    298.1500    ,   0.1383466E-04 point ps 2 pt 6

plot [280:310] H(T)
