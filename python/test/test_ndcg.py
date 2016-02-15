# Copyright 2016 Krysta M Bouzek
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at

#    http://www.apache.org/licenses/LICENSE-2.0

#    Unless required by applicable law or agreed to in writing, software
#    distributed under the License is distributed on an "AS IS" BASIS,
#    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#    See the License for the specific language governing permissions and
#    limitations under the License.

import unittest
import ndcg
import numpy as np

class TestNDCG(unittest.TestCase):

    def xtest_dcg_none(self):
        self.assertAlmostEqual(0.0, ndcg.dcg(None))
        self.assertAlmostEqual(0.0, ndcg.dcg([]))
        self.assertAlmostEqual(0.0, ndcg.dcg(np.asarray([])))


    def xtest_dcg_zeros(self):
        self.assertAlmostEqual(0.0, ndcg.dcg([0, 0, 0, 0]))
        self.assertAlmostEqual(0.0, ndcg.dcg([0, 0, 0, 0], False))


    def test_dcg(self):
        self.assertAlmostEqual(2.5, ndcg.dcg([1, 1, 0, 1], False))
        self.assertAlmostEqual(18.4922829, ndcg.dcg([8, 9, 1, 0, 2], False))
        self.assertAlmostEqual(2.0616063, ndcg.dcg([1, 1, 0, 1]))
        self.assertAlmostEqual(579.0656625, ndcg.dcg([8, 9, 1, 0, 2]))


if __name__ == '__main__':
    unittest.main()
