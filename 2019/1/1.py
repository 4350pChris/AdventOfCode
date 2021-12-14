def get_lines():
  for row in open('1/input'):
    yield int(row)

def calc_fuel(mass):
  return mass // 3 - 2

def first():
  return sum((calc_fuel(l) for l in get_lines()))

print(first())

def calc_fuel2(mass):
  s = 0
  while True:
    fuel = calc_fuel(mass)
    if fuel < 0:
      return s
    s += fuel
    mass = fuel

def second():
  return sum(calc_fuel2(l) for l in get_lines())

print(second())
