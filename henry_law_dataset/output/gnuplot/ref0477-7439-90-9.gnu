# load "ref0477-7439-90-9.gnu"
# chem = "krypton"

set terminal postscript eps color
set title "ref = 477; chem = krypton; casrn = 7439-90-9"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2480949E-04 * exp(  -1886.200    *(1/   298.    -1/T))

set label "" at    288.1500    ,   0.3111054E-04 point
set label "" at    293.1500    ,   0.2753304E-04 point
set label "" at    298.1500    ,   0.2464374E-04 point
set label "" at    303.1500    ,   0.2227877E-04 point
set label "" at    308.1500    ,   0.2034529E-04 point
set label "" at    298.1500    ,   0.2480949E-04 point ps 2 pt 6

plot [280:310] H(T)
