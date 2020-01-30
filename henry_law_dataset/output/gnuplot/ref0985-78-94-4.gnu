# load "ref0985-78-94-4.gnu"
# chem = "3-buten-2-one"

set terminal postscript eps color
set title "ref = 985; chem = 3-buten-2-one; casrn = 78-94-4"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2087826     * exp(  -7787.910    *(1/   298.    -1/T))

set label "" at    278.0000    ,    1.491843     point
set label "" at    283.0000    ,   0.7589119     point
set label "" at    288.0000    ,   0.5220155     point
set label "" at    293.0000    ,   0.3420716     point
set label "" at    298.0000    ,   0.2124203     point
set label "" at    298.1500    ,   0.2087826     point ps 2 pt 6

plot [270:300] H(T)
