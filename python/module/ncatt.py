##################Function to get attributes of netcdf file

def gncatt(nc_fid, verb=True):
    '''
    parameter
    --------
    nc_fid :  dataset read by netCDF4.Dataset
    verb : Boolean    (whether results are printed)

    Returns
    ------
    nc_attrs : list    (A Python list of the NetCDF file global attributes)
    nc_dims : list    (A Python list of the NetCDF file dimensions)
    nc_vars : list    (A Python list of the NetCDF file variables)
    '''
    
    def print_ncattr(key):
        """
        Prints the NetCDF file attributes for a given key

        Parameters
        ----------
        key : unicode
            a valid netCDF4.Dataset.variables key
        """
        try:
            print ("\t\ttype:", repr(nc_fid.variables[key].dtype))
            for ncattr in nc_fid.variables[key].ncattrs():
                print ('\t\t%s:' % ncattr,\
                      repr(nc_fid.variables[key].getncattr(ncattr)))
        except KeyError:
            print ("\t\tWARNING: %s does not contain variable attributes" % key)

    # NetCDF global attributes
    nc_attrs = nc_fid.ncattrs()
    if verb:
        print ("NetCDF Global Attributes:")
        for nc_attr in nc_attrs:
            print ('\t%s:' % nc_attr, repr(nc_fid.getncattr(nc_attr)))
    nc_dims = [dim for dim in nc_fid.dimensions]  # list of nc dimensions
    # Dimension shape information.
    if verb:
        print ("NetCDF dimension information:")
        for dim in nc_dims:
            print ("\tName:", dim )
            print ("\t\tsize:", len(nc_fid.dimensions[dim]))
            print_ncattr(dim)
    # Variable information.
    nc_vars = [var for var in nc_fid.variables]  # list of nc variables
    if verb:
        print ("NetCDF variable information:")
        for var in nc_vars:
            if var not in nc_dims:
                print ('\tName:', var)
                print ("\t\tdimensions:", nc_fid.variables[var].dimensions)
                print ("\t\tsize:", nc_fid.variables[var].size)
                print_ncattr(var)
    return nc_attrs, nc_dims, nc_vars




##################Function to copy attributes of netcdf file
import numpy as np

def cpncatt(srcnc, dstnc, variable, dim = ['time', 'lat', 'lon'], 
            copyvalue = False):
    '''
    parameter
    --------
    srcnc : source netcdf file providing attributes
    dstnc : destination netcdf file needing attributes
    variable : variable to be created
    dim : dimensions to be created
    copyvalue : default False, not copy the value of variables
    
    Return
    --------
    
    '''
    # Copy global attributes 
    dstnc.setncatts({a:srcnc.getncattr(a) for a in srcnc.ncattrs()}) 
    
    #create dimensions
    dstnc.createDimension(dim[0], None)
    dstnc.createDimension(dim[1], len(srcnc.variables[dim[1]][:]))
    dstnc.createDimension(dim[2], len(srcnc.variables[dim[2]][:]))

    # create variables
    dstnc.createVariable(dim[0], np.float64, (dim[0],))
    dstnc.createVariable(dim[1], np.float64, (dim[1],))
    dstnc.createVariable(dim[2], np.float64, (dim[2],))
    dstnc.createVariable(variable, np.float32, (dim[0], dim[1], dim[2]))
  
    # Copy variables attributes
    for name, var in dstnc.variables.items():
        dstnc.variables[name].setncatts({a:srcnc.variables[name].getncattr(a) 
                                        for a in srcnc.variables[name].ncattrs()})
    if copyvalue :
        # Copy variables' value
        dstnc.variables[dim[0]][:] = srcnc.variables[dim[0]][:]
        dstnc.variables[dim[1]][:] = srcnc.variables[dim[1]][:]
        dstnc.variables[dim[2]][:] = srcnc.variables[dim[2]][:]
        dstnc.variables[variable][:] = srcnc.variables[variable][:]
    


    
    

    