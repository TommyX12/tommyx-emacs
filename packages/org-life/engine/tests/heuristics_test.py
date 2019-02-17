import test_util
test_util.allow_parent_import()

from heuristics import *
from data_structure import *
from usable_time_parser import *

import unittest

class HeuristicsTest(unittest.TestCase):
    '''
    TODO: Test other stuff
    '''

    def test_plot(self):
        # return # comment this to plot
    
        import matplotlib.pyplot as plt
        import numpy as np
        
        variable_time_std = 0.25
        total_time_std = 0.25
        mean_scale = 1.1
        e = GaussianProbabilityEstimator(variable_time_std, total_time_std, mean_scale)
        step = 0.01
        X = np.arange(0, 16, step)
        for center in [2, 4, 8, 16]:
            Y = [
                e.get_success_probability(None, x, center)
                for x in X
            ]
            # Y.append(Y[-1])
            # Y = [
            #     (Y[i + 1] - Y[i]) / step
            #     for i in range(len(Y) - 1)
            # ]
            plt.plot(X, Y)
            
        plt.show()

if __name__ == '__main__':
    unittest.main()
