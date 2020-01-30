# load "ref2981-100-41-4.gnu"
# chem = "ethylbenzene"

set terminal postscript eps color
set title "ref = 2981; chem = ethylbenzene; casrn = 100-41-4"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.1289830E-02 * exp(  -5632.540    *(1/   298.    -1/T))

set label "" at    277.6500    ,   0.5332443E-02 point
set label "" at    279.4800    ,   0.4604674E-02 point
set label "" at    280.2100    ,   0.4253592E-02 point
set label "" at    282.1100    ,   0.3760243E-02 point
set label "" at    284.9000    ,   0.3039526E-02 point
set label "" at    285.2500    ,   0.2997849E-02 point
set label "" at    288.2500    ,   0.2440914E-02 point
set label "" at    291.0800    ,   0.2082495E-02 point
set label "" at    293.2100    ,   0.1788437E-02 point
set label "" at    298.1500    ,   0.1289830E-02 point ps 2 pt 6

plot [270:300] H(T)
