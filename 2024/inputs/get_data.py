from aocd import get_data


def get_input(day):
    return get_data(day=day, year=2024, session="53616c7465645f5fc9eec288e5d079f6722760ca02aa3d747e4f478c6f2f4b21641651aacef5db6c6b03ef97813dc40b18eaab1e21f59a7b788f632126d374c7")


def write_input(day, data):
    with open(f'./{day}.txt', 'w') as f:
        f.write(data)


for day in range(6, 24):
    data = get_input(day)
    write_input(day, data)
