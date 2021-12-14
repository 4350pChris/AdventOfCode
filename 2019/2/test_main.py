import main
import unittest

class TestMain(unittest.TestCase):
  def test_first(self):
    codes = [
      [1,0,0,0,99],
      [2,3,0,3,99],
      [2,4,4,5,99,0],
      [1,1,1,4,99,5,6,0,99]
    ]
    results = [
      [2,0,0,0,99],
      [2,3,0,6,99],
      [2,4,4,5,99,9801],
      [30,1,1,4,2,5,6,0,99]
    ]
    for start, end in zip(codes, results):
      self.assertEqual(end, main.first(start))
