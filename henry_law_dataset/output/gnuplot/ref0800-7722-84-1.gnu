# load "ref0800-7722-84-1.gnu"
# chem = "hydrogen peroxide"

set terminal postscript eps color
set title "ref = 800; chem = hydrogen peroxide; casrn = 7722-84-1"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    845.4516     * exp(  -6523.072    *(1/   298.    -1/T))

set label "" at    283.1500    ,    2694.301     point
set label "" at    295.1500    ,    1056.008     point
set label "" at    298.1500    ,    845.4516     point ps 2 pt 6

plot [280:300] H(T)
