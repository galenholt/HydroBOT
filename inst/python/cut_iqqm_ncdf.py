from py_ewr import data_inputs
import xarray as xr
from glob import glob
import re
import os


def cut_iqqm_ncdf(infile, newpath, newname = 'cut_flows', timeclip = slice(None, None)):
     
    # open the ncdf
    dataset = xr.open_dataset(infile, engine='netcdf4')
    iqqm_dict = data_inputs.get_iqqm_codes()

    # the nodes are ints, but the above is str
    ints_list = list(map(int, list(iqqm_dict)))

    # Get just the nodes that match gauges
    dataset = dataset.sel(node=dataset['node'].isin(ints_list))
    
    # cut to just flow
    dataset = dataset.drop_vars(['Orders', 'IDT Result']) 

    # cut the time if timeclip isn't None
    # slice(None, timeclip) # where timeclip = 2000
    dataset = dataset.isel(time = timeclip)
    dataset.to_netcdf(os.path.join(newpath, f"{newname}.nc"))

def cut_all_ncdf(inparent, outparent, newname = 'cutflows', single_dir = False, timeclip = slice(None, None)):
    # get the tree to all files
    filelist = glob(f"{inparent}/**/*(Gauge).nc", recursive = True)

    # clean up to get the internal tree
    juststruct = [item.replace("Straight Node (Gauge).nc", '') for item in filelist]
    juststruct = [item.replace(inparent, '') for item in juststruct]
    # knock off the starting/ending slashes
    oschar = re.escape(os.sep)
    juststruct = [re.sub(f"^{oschar}|{oschar}$", '', item) for item in juststruct]

    # it should work to just dump everything in one directory as long as the filenames are distinct.
    if single_dir:
        juststruct = [item.replace(os.sep, '_') for item in juststruct]

    newpaths = [os.path.join(outparent, subpath) for subpath in juststruct]

    for directory in newpaths:
        if not os.path.exists(directory):
            os.makedirs(directory)

    for inp, outp in zip(filelist, newpaths):
        cut_iqqm_ncdf(inp, outp, newname, timeclip = timeclip)

