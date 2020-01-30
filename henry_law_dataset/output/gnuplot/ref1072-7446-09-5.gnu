# load "ref1072-7446-09-5.gnu"
# chem = "sulfur dioxide"

set terminal postscript eps color
set title "ref = 1072; chem = sulfur dioxide; casrn = 7446-09-5"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1224117E-01 * exp(  -3121.720    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.3227397E-01 point
set label "" at    283.1500    ,   0.2164717E-01 point
set label "" at    291.1500    ,   0.1525142E-01 point
set label "" at    298.1500    ,   0.1210274E-01 point
set label "" at    308.1500    ,   0.8757266E-02 point
set label "" at    323.1500    ,   0.5510190E-02 point
set label "" at    298.1500    ,   0.1224117E-01 point ps 2 pt 6

plot [270:330] H(T)
