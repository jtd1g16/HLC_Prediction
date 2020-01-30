# load "ref0483-78-93-3.gnu"
# chem = "2-butanone"

set terminal postscript eps color
set title "ref = 483; chem = 2-butanone; casrn = 78-93-3"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1718731     * exp(  -5741.968    *(1/   298.    -1/T))

set label "" at    273.0000    ,    1.013284     point
set label "" at    298.0000    ,   0.1735474     point
set label "" at    298.1500    ,   0.1718731     point ps 2 pt 6

plot [270:300] H(T)
