# load "ref0379-7664-41-7.gnu"
# chem = "ammonia"

set terminal postscript eps color
set title "ref = 379; chem = ammonia; casrn = 7664-41-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2708120     * exp(  -2058.840    *(1/   298.    -1/T))

set label "" at    273.1500    ,   0.4975573     point
set label "" at    277.1500    ,   0.4610110     point
set label "" at    281.1500    ,   0.4169794     point
set label "" at    283.1500    ,   0.3830751     point
set label "" at    285.1500    ,   0.3773510     point
set label "" at    286.1500    ,   0.3685446     point
set label "" at    288.1500    ,   0.3390435     point
set label "" at    289.1500    ,   0.3412450     point
set label "" at    293.1500    ,   0.2994150     point
set label "" at    297.1500    ,   0.2813620     point
set label "" at    301.1500    ,   0.2580253     point
set label "" at    303.1500    ,   0.2333676     point
set label "" at    298.1500    ,   0.2708120     point ps 2 pt 6

plot [270:310] H(T)
