# load "ref0921-75-46-7.gnu"
# chem = "trifluoromethane"

set terminal postscript eps color
set title "ref = 921; chem = trifluoromethane; casrn = 75-46-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1353439E-03 * exp(  -2156.050    *(1/   298.    -1/T))

set label "" at    278.2000    ,   0.2353973E-03 point
set label "" at    278.2000    ,   0.2364029E-03 point
set label "" at    308.1500    ,   0.9866627E-04 point
set label "" at    308.1500    ,   0.9857839E-04 point
set label "" at    338.2100    ,   0.6042353E-04 point
set label "" at    338.2100    ,   0.5979031E-04 point
set label "" at    298.1500    ,   0.1353439E-03 point ps 2 pt 6

plot [270:340] H(T)
