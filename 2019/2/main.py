from itertools import count

def get_codes():
  with open('2/input') as f:
    return map(int, f.readline().split(','))

def first(codes):
  for i in count(0, 4):
    op = codes[i]
    if op == 99:
      return codes
    elif op == 1:
      fn = lambda x,y: x+y
    elif op == 2:
      fn = lambda x,y: x*y
    else:
      print('Crashed at ' + str(op))
      return
    p1, p2, pOut = codes[i+1:i+4]
    codes[pOut] = fn(codes[p1], codes[p2])

if __name__ == "__main__":
  codes = get_codes()
  codes[1] = 12
  codes[2] = 2
  result = first(codes)
  print('Result: ' + str(result[0]))

  target = 19690720
  for noun in range(100):
    for verb in range(100):
      codes = get_codes()
      codes[1] = noun
      codes[2] = verb
      result = first(codes)
      if result[0] == target:
        print('Done!')
        print(100 * noun + verb)

