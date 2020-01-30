# load "ref0484-141-46-8.gnu"
# chem = "2-hydroxyethanal"

set terminal postscript eps color
set title "ref = 484; chem = 2-hydroxyethanal; casrn = 141-46-8"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    408.5862     * exp(  -4629.041    *(1/   298.    -1/T))

set label "" at    298.1500    ,    408.5862     point
set label "" at    318.1500    ,    153.9600     point
set label "" at    298.1500    ,    408.5862     point ps 2 pt 6

plot [290:320] H(T)
