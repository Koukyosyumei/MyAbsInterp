import copy

def g(x, y, table):
    return min(y, max(x, table[(x, y)]))


def f(x, y, z, table):
    return max(min(y, z), table[(z, x, table[(y, 1, 1)])])


def fixedpoint_iteration(g, table):
    is_converge = False
    num_itr = 0
    while not is_converge:
        num_itr += 1
        prev_table = copy.deepcopy(table)
        for k in table.keys():
            table[k] = g(*k, prev_table)
        print(num_itr, table)
        is_converge = True
        for n, p in zip(table.values(), prev_table.values()):
            is_converge = is_converge and n == p
    return num_itr


if __name__ == "__main__":
    table_g = {(0, 0): 0, (1, 0): 0, (0, 1): 0, (1, 1): 0}
    num_itr = fixedpoint_iteration(g, table_g)
    print(num_itr, table_g)

    table_f = {
        (0, 0, 0): 0,
        (0, 0, 1): 0,
        (0, 1, 0): 0,
        (0, 1, 1): 0,
        (1, 0, 0): 0,
        (1, 0, 1): 0,
        (1, 1, 0): 0,
        (1, 1, 1): 0,
    }
    num_itr = fixedpoint_iteration(f, table_f)
    print(num_itr, table_f)
