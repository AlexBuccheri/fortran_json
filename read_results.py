
""" 
Extract data from JSON outout 
Useful ref:
https://stackoverflow.com/questions/26646362/numpy-array-is-not-json-serializable
A Buccheri 2020 
"""

import json
import numpy as np


def get_numpy_array(data:dict, key:str, order='F') -> np.ndarray:
    """
    Extract flattened n-d numpy arrays from JSON dictionary

    Note, reshape shouldn't create a copy BUT it would be preferable
    if the JSON data for multi-dimensional arrays was stored 
    {key: [ [],[],...,[]]} so one could avoid the additional step 

    :param data:  JSON dictionary 
    :param key:   key for data to extract 
    :param order: optional string. Reshape data row-major (C) or column-major (F) 
    :return : numpy array with correct shape 
    """
    assert key in data, 'key not in data'
    assert key + "_shape" in data, 'key_shape not in data'
    data_shape = tuple(data[key + "_shape"])
    return np.asarray(data[key]).reshape(data_shape, order='F')
    

with open('results.json') as fid:
    data = json.load(fid)
fid.close()

print('Keys in data dictionary:', [key for key in data.keys()])

n_atoms = data['structure']['n_atoms']
assert n_atoms == 2

positions = get_numpy_array(data['structure'], 'positions')
expected_positions = np.transpose(np.array([[0,0,0], [1,1,1]]))
assert np.array_equal(positions, expected_positions)



