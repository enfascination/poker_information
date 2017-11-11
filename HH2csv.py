#python -i HH2csv.py 10/abstmp.txt

import sys
import csv
import re
from random import randint
from sets import Set
from numpy import array, diff, array_equal
import logging
import time
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

#logging.disable("ERROR")

### helpers
### http://stackoverflow.com/questions/1157106/remove-all-occurences-of-a-value-from-a-python-list
def remove_values_from_list(the_list, val):
     return [value for value in the_list if value != val]

### http://stackoverflow.com/questions/7627548/find-sub-list-inside-a-list-in-python
def getsubidx(x, y):
     l1, l2 = len(x), len(y)
     for i in range(l1):
           if array_equal(x[i:i+l2], y): return i
     return -1

def cardsuit(card): 
     return card[1]

def cardsuits(fullhand):
     return [cardsuit(card) for card in fullhand]

def cardvalue(card): 
     if card[0] == "T": return 10
     elif card[0] == "J": return 11
     elif card[0] == "Q": return 12
     elif card[0] == "K": return 13
     elif card[0] == "A": return 14
     else: return int(card[0])

def cardvalues(cards):
     return [cardvalue(card) for card in cards]

### score a hand; i invented an ordinal ranking for this.
def gethandrank(community, hole, holerank=0):
     handscore = 0
     ### can hand be ranked? Is there enough info?
     if holerank == 170: return -1
     ### test for high card
     fullhand = list(community)
     fullhand.extend(hole)
     highholecard = max(cardvalues(hole))
     handscore = highholecard
     ### test for pair
     #print(fullhand)
     #print(hole, holerank)
     nicevalues = sorted(cardvalues(fullhand), reverse=True)
     tabled_hand = [nicevalues.count(value) for value in nicevalues]
     #print(tabled_hand)
     #print((max(tabled_hand) == 2) , (tabled_hand.count(2) == 2), (max(tabled_hand) == 2) & (tabled_hand.count(2) == 2))
     if (max(tabled_hand) == 2) & (tabled_hand.count(2) == 2):
         pair_cards = [nicevalues[i] for i in range(len(nicevalues)) if tabled_hand[i] == 2]
         handscore = 10000 + 100*pair_cards[1] + highholecard
     ### test for two pair
     if (max(tabled_hand) == 2) & (tabled_hand.count(2) == 4):
         pair_cards = [nicevalues[i] for i in range(len(nicevalues)) if tabled_hand[i] == 2]
         handscore = 100000 + 100*max(pair_cards) + highholecard
     ### test for three ofa kind
     if (max(tabled_hand) == 3) :
         pair_cards = [nicevalues[i] for i in range(len(nicevalues)) if tabled_hand[i] == 3]
         handscore = 1000000 + 100*max(pair_cards) + highholecard
     ### test for straight
     straight_idx = getsubidx(diff(nicevalues),array([-1,-1,-1,-1]))
     if straight_idx >= 0:
         straight = True
         handscore = 10000000 + 100*nicevalues[straight_idx] + highholecard
     else: straight = False
     ### test for flush
     if max([cardsuits(fullhand).count(suit) for suit in cardsuits(fullhand)]) >= 5:
         flush = True
         handscore = 100000000 + highholecard
     else: flush = False
     ### test for full house
     if (max(tabled_hand) == 3) & (((tabled_hand.count(2) == 2) & (tabled_hand.count(3) == 3)) | (tabled_hand.count(3) == 6)):
         three_cards = [nicevalues[i] for i in range(len(nicevalues)) if tabled_hand[i] == 3]
         ### not implementing the full tie breaking here, not caring about breaking ties iwth pairs
         handscore = 200000000  + 100*three_cards[1]  + highholecard
     ### test for four of a kind
     if (max(tabled_hand) == 4) & (tabled_hand.count(4) == 4):
         four_cards = [nicevalues[i] for i in range(len(nicevalues)) if tabled_hand[i] == 4]
         handscore = 300000000 + 100*four_cards[1] + highholecard
     ### test for straight flush
     if straight & flush: 
         handscore = 400000000  + 100*nicevalues[straight_idx] + highholecard
     return handscore
player_dict = {}
player_count = 1

### this is temporary having this code here
#code = "ABS" # "FTP", "IPN", "ONG", "PS", "PTY"
code =  "PS"#, "IPN", "ONG", "PS", "PTY"
startbatch = time.time()
outfile = code+str(randint(1,1000))+'pokerhandrank.csv'
with open(outfile, 'wb') as csvfile:
  pokerdata = csv.writer(csvfile, delimiter=',', quotechar='|', quoting=csv.QUOTE_MINIMAL)
  for xx, filename in enumerate(sys.argv[1:]):
    startfile = time.time()
    #print filename
    #if filename[0] <= 1: next
    config =Configuration.Config()
    config.site_ids = config.supported_sites
    if code == "ABS":
      hhc = AbsoluteToFpdb.Absolute(config, in_path=filename, sitename="Absolute")
      ### a hack for reconstructing mucked holes
      s = re.compile("\nSeat \d: (\S+) .*\[Mucked\] \[(\w+) (\w+)\]")
      re_mucked = re.compile(".*\[Mucked\].*")
    elif code == "FTP":
      hhc = FulltiltToFpdb.Fulltilt(config, in_path=filename, sitename="Fulltilt")
    elif code == "IPN":
      hhc = iPokerToFpdb.iPoker(config, in_path=filename, sitename="iPoker")
    elif code == "ONG":
      hhc = OnGameToFpdb.OnGame(config, in_path=filename, sitename="OnGame")
    elif code == "PS":
      hhc = PokerStarsToFpdb.PokerStars(config, in_path=filename, sitename="PokerStars")
    elif code == "PTY":
      hhc = PartyPokerToFpdb.PartyPoker(config, in_path=filename, sitename="PartyPoker")
    i=0
    num_mucks = 0
    num_holepairs = 0
    num_players = 0
    num_lines = 0
    best_hand = 0
    all_holdem = True
    #muckcaughtcount = 0
    for i in range(len(hhc.getProcessedHands())):
      #print("%d, %d, %d"%(1,2,3))
      ### handID, bigblind, playerID, betsize, holecardrank(ifknown)
      handstruct = hhc.getProcessedHands()[i]
      handid = handstruct.handid
      #print(i, handid)
      ### add ABS muck, since it is not parsed by library
      if code == "ABS":
        p = s.findall(handstruct.handText)
        p_test = re_mucked.findall(handstruct.handText)
        #if (len(p_test) > 0) & (len(p) == 0): 
          #print "missed muck!", num_mucks, p_test, p
          #print handstruct.handText
        #print p
        if len(p) > 0 :
          #muckcaughtcount += 1
          #print muckcaughtcount, p[0]
          ### in case there were multiple mucks ina  round
          for i in p: handstruct.addShownCards([AbsoluteToFpdb.validCard(i[1]), AbsoluteToFpdb.validCard(i[2])], i[0], mucked=True)
        #filenames = sys.argv
        #if (g is not None) :
                    #cards = [validCard(card) for card in cards.split(' ')  if card != 'H']
          #print("X", g.group(1), "X", g.group(2), "X", g.group(3), "X")
      #print(handstruct.handText)
      bb = float(handstruct.bb)
      for j in range(len(handstruct.players)):
        player = handstruct.players[j][1]
        if not player_dict.get(player, False): 
          player_dict[player] = player_count
          player_count += 1
        num_players += 1
        holerank = Card.calcStartCards(handstruct, player)
        won = handstruct.collectees.get(player, 0)
        bets = 0.0
        fullhand = []
        for street_num, street in enumerate(handstruct.allStreets):
          #print street_num, street, bets, player, handstruct.bets, holerank
          bets = bets + float(sum(handstruct.bets[street][player]))
          if street_num == 0: ### I want to record blinds in the blinds antes row as posted bets.  that street can act like a control, since I know that any action there is non strategic.
            bets_tmp = bets
            for l in handstruct.actions[street]:
              if l[0] == player:
                if l[1] == 'small blind': bets += float(handstruct.gametype['sb'])
                elif l[1] == 'big blind': bets += float(handstruct.gametype['bb'])
          holecards = remove_values_from_list(handstruct.join_holecards(player, asList=True), u'0x') 
          if holecards == []: holecards = [u'0x',u'0x']
          #if street == "BLINDSANTES": fullhand.extend( holecards )
          fullhand.extend( handstruct.board[street])
          #print street, holecards, fullhand
          handrank = gethandrank(fullhand, holecards, holerank)
          ### https://coderwall.com/p/rcmaea list of lists comprehension ncomprehension
          #holecards = [card for l in (hhc.getProcessedHands()[3].holecards['PREFLOP']['4QICpZ6v7I+i8nJKeSSE8Q']) for card in l]
          num_lines+=1
          pokerdatarow = (code, bb, int(handid), len(handstruct.players), street_num, player_dict[player], handstruct.players[j][2], handstruct.get_actions_short(player, street), float(bets), won, holerank, handrank)
          pokerdata.writerow(pokerdatarow)
          if street_num == 0: bets = bets_tmp
          #if int(handstruct.handid) == 3017615590:
            #print("%s, %d, %d, %d, %s, %d, %d, %d, %d"%pokerdatarow)
          #print(pokerdatarow)
        if handrank > best_hand: best_hand = handrank
        if (street == 4) and (holerank != 170): 
          num_holepairs+=1
          if won == 0: num_mucks+=1
      if handstruct.gametype['category'] != "holdem": all_holdem = not all_holdem
      #if i > 2: break
    ### print summary
    end = time.time()
    print("SUMMARY %s -> %s: %d hands in, %d out (%d erred, %d partial); %d lines in %.2f s (net %.1f s).\n \
        %d players for ~%.2f/hand. best hand: %d. %d pairs, %d mucks. all holdem? %s"%(
      filename, outfile, hhc.numHands, len(hhc.processedHands), hhc.numErrors, hhc.numPartial, num_lines, end-startfile, end-startbatch, num_players, (num_players+0.0)/hhc.numHands, best_hand, num_holepairs, num_mucks, all_holdem))

### summary stats

### test handranking:
hand_nothing = ['2d','3d','4d','5h','7h']
hand_lowpair = ['2d','2h','4d','5h','7h']
hand_highpair = ['2d','3d','4h','Ad','Ah']
hand_twopair1 = ['2d','2h','4h','Kd','Kh']
hand_twopair2 = ['2d','2h','4h','Ad','Ah']
hand_threekind1 = ['Td','Th','Tc','2d','Kh']
hand_threekind2 = ['Td','2h','Kc','Kd','Kh']
hand_straight1 = ['2d','3d','4d','5h','6h']
hand_straight2 = ['Ad','Kd','Jd','Qh','Th']
hand_flush1 = ['2d','3d','4d','5d','7d']
hand_flush2 = ['2h','3h','4h','5h','7h']
hand_fullhouse1 = ['Td','Th','Tc','Kd','Kh']
hand_fullhouse2 = ['Td','Th','Kc','Kd','Kh']
hand_fourkind1 = ['2d','2s','2c','2h','Kh']
hand_fourkind2 = ['Td','Ts','Tc','Th','Kh']
hand_fourkind3 = ['Td','Ks','Kc','Kd','Kh']
hand_straightflush = ['2d','3d','4d','5d','6d']
hand_royalflush = ['Th','Jh','Qh','Kh','Ah']
assert(gethandrank(hand_nothing, ['8h','9d']) <  gethandrank(hand_nothing, ['8h','Td']))
assert(gethandrank(hand_nothing, ['8h','9d']) <  gethandrank(hand_lowpair, ['8h','9d']))
assert(gethandrank(hand_lowpair, ['8h','9d']) <  gethandrank(hand_lowpair, ['8h','Td']))
assert(gethandrank(hand_lowpair, ['8h','9d']) <  gethandrank(hand_highpair, ['8h','9d']))
assert(gethandrank(hand_highpair, ['8h','9d']) <  gethandrank(hand_twopair1, ['9h','8d']))
assert(gethandrank(hand_twopair1, ['8h','9d']) <  gethandrank(hand_twopair1, ['8h','Td']))
assert(gethandrank(hand_twopair1, ['8h','9d']) <  gethandrank(hand_twopair2, ['8h','9d']))
assert(gethandrank(hand_twopair2, ['8h','9d']) <  gethandrank(hand_threekind1, ['8h','9d']))
assert(gethandrank(hand_threekind1, ['8h','9d']) <  gethandrank(hand_threekind1, ['8h','Td']))
assert(gethandrank(hand_threekind1, ['8h','9d']) <  gethandrank(hand_threekind2, ['8h','9d']))
assert(gethandrank(hand_threekind2, ['8h','9d']) <  gethandrank(hand_straight1, ['8h','9d']))
assert(gethandrank(hand_straight1, ['8h','9d']) <  gethandrank(hand_straight1, ['8h','Td']))
assert(gethandrank(hand_straight1, ['8h','9d']) <  gethandrank(hand_straight2, ['8h','9d']))
assert(gethandrank(hand_straight2, ['8h','9d']) <  gethandrank(hand_flush1, ['8h','9d']))
assert(gethandrank(hand_flush1, ['8h','9d']) ==  gethandrank(hand_flush2, ['8h','9d']))
assert(gethandrank(hand_flush1, ['8h','9d']) <  gethandrank(hand_flush2, ['8h','Td']))
assert(gethandrank(hand_flush2, ['8h','9d']) <  gethandrank(hand_fullhouse1, ['8h','9d']))
assert(gethandrank(hand_fullhouse1, ['8h','9d']) <  gethandrank(hand_fullhouse1, ['8h','Td']))
assert(gethandrank(hand_fullhouse1, ['8h','9d']) <  gethandrank(hand_fullhouse2, ['8h','9d']))
assert(gethandrank(hand_fullhouse2, ['8h','9d']) <  gethandrank(hand_fourkind1, ['8h','9d']))
assert(gethandrank(hand_fourkind1, ['8h','9d']) <  gethandrank(hand_fourkind1, ['8h','Td']))
assert(gethandrank(hand_fourkind1, ['8h','9d']) <  gethandrank(hand_fourkind2, ['8h','9d']))
assert(gethandrank(hand_fourkind2, ['8h','9d']) <  gethandrank(hand_fourkind3, ['8h','9d']))
assert(gethandrank(hand_fourkind3, ['8h','9d']) <  gethandrank(hand_straightflush, ['8h','9d']))
assert(gethandrank(hand_straightflush, ['8h','9d']) <  gethandrank(hand_straightflush, ['8h','Td']))
assert(gethandrank(hand_straightflush, ['8h','9d']) <  gethandrank(hand_royalflush, ['8h','9d']))

### TODO
### output is a csv file, each row gives game ID, player ID, big blind, ante, chips, folded?, won?, ExtrA?, IntrA?, ExtrA, IntrA
#import HandHistoryConverter 
#hhc = HandHistoryConverter.HandHistoryConverter(Configuration.Config(), in_path="/Users/sethfrey/projecto/research_projects/poker_information/10/abstmp.txt", sitename="Absolute")
#hhc = HandHistoryConverter.AbsoluteToFpdb(Configuration.Config(), in_path="/Users/sethfrey/projecto/research_projects/poker_information/10/abstmp.txt", sitename="Absolute")
#hhc = AbsoluteToFpdb.Absolute(config, in_path=filename[1], sitename="Absolute")
#handstruct = hhc.getProcessedHands()[740]
#handstruct = hhc.getProcessedHands()[390]
#print(handstruct)



