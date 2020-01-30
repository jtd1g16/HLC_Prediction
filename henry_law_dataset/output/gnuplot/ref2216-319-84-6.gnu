# load "ref2216-319-84-6.gnu"
# chem = "alpha-HCH"

set terminal postscript eps color
set title "ref = 2216; chem = alpha-HCH; casrn = 319-84-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =    1.738057     * exp(  -7467.126    *(1/   298.    -1/T))

set label "" at    278.1500    ,    10.52632     point
set label "" at    283.1500    ,    6.666667     point
set label "" at    293.1500    ,    2.564103     point
set label "" at    303.1500    ,    1.176471     point
set label "" at    308.1500    ,   0.7692308     point
set label "" at    298.1500    ,    1.738057     point ps 2 pt 6

plot [270:310] H(T)
