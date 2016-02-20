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

    def test_dcg_none(self):
        self.assertAlmostEqual(0.0, ndcg.dcg(None))
        self.assertAlmostEqual(0.0, ndcg.dcg([]))
        self.assertAlmostEqual(0.0, ndcg.dcg(np.asarray([])))


    def test_dcg_zeros(self):
        self.assertAlmostEqual(0.0, ndcg.dcg([0, 0, 0, 0]))
        self.assertAlmostEqual(0.0, ndcg.dcg([0, 0, 0, 0], False))


    def test_dcg(self):
        self.assertAlmostEqual(2.5, ndcg.dcg([1, 1, 0, 1], False))
        self.assertAlmostEqual(18.4922829, ndcg.dcg([8, 9, 1, 0, 2], False))
        # from wikipedia
        self.assertAlmostEqual(8.0971714, ndcg.dcg([3, 2, 3, 0, 1, 2], False))


    def test_dcg_alternate(self):
        self.assertAlmostEqual(2.0616063, ndcg.dcg([1, 1, 0, 1]))
        self.assertAlmostEqual(579.0656625, ndcg.dcg([8, 9, 1, 0, 2]))


    def test_cum_gain(self):
        # from wikipedia
        self.assertAlmostEqual(11.0, ndcg.cum_gain([3, 2, 3, 0, 1, 2]))
        # order is irrelevant
        self.assertAlmostEqual(11.0, ndcg.cum_gain([3, 1, 0, 3, 2, 2]))


    def test_idcg_none(self):
        # from wikipedia
        self.assertAlmostEqual(0.0, ndcg.idcg([], False))
        self.assertAlmostEqual(0.0, ndcg.idcg(np.asarray([]), False))
        self.assertAlmostEqual(0.0, ndcg.idcg(None, False))


    def test_idcg_zeros(self):
        self.assertAlmostEqual(0.0, ndcg.idcg([0, 0, 0, 0], False))


    def test_idcg(self):
        # from wikipedia
        self.assertAlmostEqual(8.6925361, ndcg.idcg([3, 2, 3, 0, 1, 2], False))
        # order is irrelevant
        self.assertAlmostEqual(8.6925361, ndcg.idcg([3, 2, 3, 0, 1, 2], False))


    def test_ndcg_none(self):
        # from wikipedia
        self.assertAlmostEqual(0.0, ndcg.ndcg([], 0, False))
        self.assertAlmostEqual(0.0, ndcg.ndcg(np.asarray([]), 0, False))
        self.assertAlmostEqual(0.0, ndcg.ndcg(None, 0, False))


    def test_ndcg_zeros(self):
        self.assertAlmostEqual(0.0, ndcg.ndcg([0, 0, 0, 0], 6, False))


    def test_ndcg(self):
        # from wikipedia
        self.assertAlmostEqual(0.9315085, ndcg.ndcg([3, 2, 3, 0, 1, 2], 6, False))

    def test_ndcg_nranks(self):
        self.assertAlmostEqual(0.9491769, ndcg.ndcg([3, 2, 3, 0], 4, False))
        self.assertAlmostEqual(0.9491769, ndcg.ndcg([3, 2, 3], 4, False))



if __name__ == '__main__':
    unittest.main()
