import sys
import csv
import re
from sets import Set
from numpy import array, diff, array_equal
sys.path.append("fpdb-0.40.2/pyfpdb")
#import HandHistoryConverter 
import Configuration
import Card
import AbsoluteToFpdb
import FulltiltToFpdb
import iPokerToFpdb
import OnGameToFpdb
import PokerStarsToFpdb
import PartyPokerToFpdb

filenames = sys.argv[-1]
with open(filenames) as f:
  content = f.readlines()
  for i in content: 
    re_seat = re.compile("Seat \d: ")
    re_mucked = re.compile("\[Mucked\]")
    p = re.compile("^Seat \d: (\S+) .*\[Mucked\] \[(\w+) (\w+)\]")
    g = p.search(i)
    if (g is not None) :
      print("X", g.group(1), "X", g.group(2), "X", g.group(3), "X")

