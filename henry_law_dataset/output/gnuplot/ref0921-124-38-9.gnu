# load "ref0921-124-38-9.gnu"
# chem = "carbon dioxide"

set terminal postscript eps color
set title "ref = 921; chem = carbon dioxide; casrn = 124-38-9"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.3574256E-03 * exp(  -2176.717    *(1/   298.    -1/T))

set label "" at    278.1500    ,   0.6349462E-03 point
set label "" at    278.1500    ,   0.6316849E-03 point
set label "" at    303.1200    ,   0.3029114E-03 point
set label "" at    303.1200    ,   0.3012624E-03 point
set label "" at    308.0800    ,   0.2700923E-03 point
set label "" at    308.0800    ,   0.2678698E-03 point
set label "" at    333.1500    ,   0.1682636E-03 point
set label "" at    333.1500    ,   0.1688283E-03 point
set label "" at    338.1500    ,   0.1564215E-03 point
set label "" at    338.1500    ,   0.1557611E-03 point
set label "" at    298.1500    ,   0.3574256E-03 point ps 2 pt 6

plot [270:340] H(T)
