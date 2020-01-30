# load "ref0287-7790-92-3.gnu"
# chem = "hypochlorous acid"

set terminal postscript eps color
set title "ref = 287; chem = hypochlorous acid; casrn = 7790-92-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    5.985909     * exp(  -4884.724    *(1/   298.    -1/T))

set label "" at    293.1500    ,    7.915684     point
set label "" at    313.1500    ,    2.730911     point
set label "" at    298.1500    ,    5.985909     point ps 2 pt 6

plot [290:320] H(T)
