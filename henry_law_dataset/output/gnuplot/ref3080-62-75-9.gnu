# load "ref3080-62-75-9.gnu"
# chem = "N-nitrosodimethylamine"

set terminal postscript eps color
set title "ref = 3080; chem = N-nitrosodimethylamine; casrn = 62-75-9"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    6.122821     * exp(  -6422.005    *(1/   298.    -1/T))

set label "" at    273.1500    ,    37.68667     point
set label "" at    283.1500    ,    23.39173     point
set label "" at    293.1500    ,    9.167028     point
set label "" at    303.1500    ,    4.161718     point
set label "" at    313.1500    ,    2.074496     point
set label "" at    298.1500    ,    6.122821     point ps 2 pt 6

plot [270:320] H(T)
