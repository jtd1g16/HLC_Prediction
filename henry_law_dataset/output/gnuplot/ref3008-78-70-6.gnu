# load "ref3008-78-70-6.gnu"
# chem = "linalool"

set terminal postscript eps color
set title "ref = 3008; chem = linalool; casrn = 78-70-6"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2009071     * exp(  -4412.241    *(1/   298.    -1/T))

set label "" at    298.0000    ,   0.2092277     point
set label "" at    293.0000    ,   0.2563040     point
set label "" at    288.0000    ,   0.3319023     point
set label "" at    283.0000    ,   0.4265482     point
set label "" at    278.0000    ,   0.6126820     point
set label "" at    298.1500    ,   0.2009071     point ps 2 pt 6

plot [270:300] H(T)
