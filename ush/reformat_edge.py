import sys
import netCDF4

fin = open(sys.argv[1],"r")
#fout = netcdf open(sys.argv[2], "w")

lats = []
lons = []
names = []
for line in fin:
    words = line.split()
    lats.append(float(words[0]) )
    lons.append(float(words[1]) )
    if (len(words) > 2):
      names.append(words[2])
    else:
      names.append("null")

#debug:
npts = len(lats)
#debug:
print("npts = ",npts)
#debug:
for i in range(0,npts):
    #debug:
    print(i,lats[i],lons[i],names[i])
    #debug:


