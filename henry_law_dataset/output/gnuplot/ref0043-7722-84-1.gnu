# load "ref0043-7722-84-1.gnu"
# chem = "hydrogen peroxide"

set terminal postscript eps color
set title "ref = 43; chem = hydrogen peroxide; casrn = 7722-84-1"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    703.3185     * exp(  -6981.630    *(1/   298.    -1/T))

set label "" at    273.1500    ,    5990.624     point
set label "" at    278.1500    ,    3770.047     point
set label "" at    283.1500    ,    2417.962     point
set label "" at    288.1500    ,    1628.423     point
set label "" at    293.1500    ,    1036.269     point
set label "" at    298.1500    ,    700.7155     point
set label "" at    298.1500    ,    703.3185     point ps 2 pt 6

plot [270:300] H(T)
