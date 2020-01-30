# load "ref2489-120-12-7.gnu"
# chem = "anthracene"

set terminal postscript eps color
set title "ref = 2489; chem = anthracene; casrn = 120-12-7"
set xlabel "temperature [K]"
set ylabel "H [mol*m-3*Pa-1]"
set dummy T

# regresion from table data:
H(T) =   0.2272172     * exp(  -5627.912    *(1/   298.    -1/T))

set label "" at    282.0900    ,   0.6944444     point
set label "" at    284.7200    ,   0.5681818     point
set label "" at    286.5400    ,   0.4784689     point
set label "" at    289.0300    ,   0.4149378     point
set label "" at    291.7400    ,   0.3367003     point
set label "" at    295.6900    ,   0.2624672     point
set label "" at    297.7600    ,   0.2325581     point
set label "" at    300.2500    ,   0.1937984     point
set label "" at    301.3500    ,   0.1798561     point
set label "" at    302.2700    ,   0.1700680     point
set label "" at    303.6800    ,   0.1594896     point
set label "" at    308.0200    ,   0.1293661     point
set label "" at    313.0600    ,   0.9425071E-01 point
set label "" at    318.0500    ,   0.7153076E-01 point
set label "" at    323.0700    ,   0.5285412E-01 point
set label "" at    298.1500    ,   0.2272172     point ps 2 pt 6

plot [280:330] H(T)
