# Ocena 6 / Grade 6

def to_number(s):
    if s.isdigit():
        return int(s)
    else:
        return s

def parse(s):
    lista = s.split()
    if len(lista) == 3:
        return (lista[2], "SET", (to_number(lista[0]),))
    elif len(lista) ==  4:
        return (lista[3], "NOT", (to_number(lista[1]),))
    else:
        return (lista[4], lista[1], (to_number(lista[0]), to_number(lista[2])))


def read(filename):
    file = open(filename)
    lista = []
    for line in file:
        lista.append(parse(line))
    file.close()
    return lista

# Ocena 7 / Grade 7

def inputs(exprs):
    re = set()
    for name, op, variables in exprs:
        for variable in variables:
            if type(variable) is str:
                re.add(variable)
    return re

def outputs(exprs):
    re = set()
    for name, op, variables in exprs:
        if type(name) is str:
            re.add(name)
    return re

def check_names(exprs):
    a = inputs(exprs)
    b = outputs(exprs)
    for input in a:
        if input not in b:
            return False
    return True

def check_operators(exprs):
    valid = ("SET", "NOT", "AND", "OR", "LSHIFT", "RSHIFT")
    for name, op, var in exprs:
        if op not in valid:
            return False
    return True

# Ocena 8

def get_value(name, variables):
    if type(name) == str:
        return variables[name]
    else:
        return name

def get_values(args, variables):
    re = ()
    for arg in args:
        t =  (get_value(arg, variables),)
        re += t
    return re

def compute_expr(op, args):
    a = args[0]
    if len(args) == 2:
        b = args[1]
    if op == "SET":
        return a
    if op == "NOT":
        return ~a
    if op == "AND":
        return a & b
    if op == "OR":
        return a | b
    if op == "LSHIFT":
        return a << b
    if op == "RSHIFT":
        return a >> b

def compute_list(exprs):
    re = {}
    for name, op, args in exprs:
        xs = ()
        #Pogleda če je arg = var, če je ga nadomesti z vrednostjo iz slovarja
        for arg in args:
            if type(arg) == str:
                xs+= (re[arg],)
            else:
                xs+= (arg,)
        re[name] = compute_expr(op, xs)
    return re

# Ocena 9

def dict_expr(exprs):
    re = {}
    for name, op, arg in exprs:
        re[name] = (op, arg)
    return re

def compute(var, exprs, variables):
    op, args = exprs[var]
    clean_arg = ()
    for arg in args:
        if type(arg) == str:
            if arg in variables:
                clean_arg += (variables[arg],)
            else:
                variables[arg] = compute(arg, exprs, variables)
                clean_arg += (variables[arg],)
        else:
            clean_arg+= (arg,)
    rez = compute_expr(op, clean_arg)
    return rez

def compute_file(var, filename):
    lista_izrazov = read(filename)
    slovar_izrazov = {}
    slovar_sprem = {}
    for line in lista_izrazov:
        ime, op, vars = line
        slovar_izrazov[ime] = (op, vars)
        for sprem in vars:
            if type(sprem) == int and op == "SET":
                slovar_sprem[ime] = sprem
    return compute(var, slovar_izrazov, slovar_sprem)

# Ocena 10

def computable(exprs):
#Sestavi slovarja izrazov
    exprs_slovar = {}
    variables_slovar = {}
    for line in exprs:
        ime, op, variables = line
        exprs_slovar[ime] = (op, variables)
#While over for, dokler ni nič več mogoče zračunati, če je bilo mogoče vse, vrne True. V nasprotnem primeru False.
    x = len(exprs_slovar)
    while x > 0:
        lala = []
        for key, value in exprs_slovar.items():
            op, vars_1 = exprs_slovar[key]
            c = 0
            for each in vars_1:
                if type(each) == int:
                    c+=1
                elif each in variables_slovar:
                    c+=1
            if c == len(vars_1):
                variables_slovar[key] = compute(key, exprs_slovar, variables_slovar)
                lala.append(key)

        for each in lala:
            exprs_slovar.pop(each, None)
        if len(exprs_slovar) == x:
            return False
        else:
            x = len(exprs_slovar)
    return True
# TESTI
import unittest
from itertools import permutations


class Test06(unittest.TestCase):
    def test_to_number(self):
        self.assertEqual(to_number("42"), 42)
        self.assertEqual(to_number("156"), 156)
        self.assertEqual(to_number("1"), 1)
        self.assertEqual(to_number("123456789"), 123456789)

        self.assertEqual(to_number("a"), "a")
        self.assertEqual(to_number("abc"), "abc")
        self.assertEqual(to_number("I know places we can hide"),
                         "I know places we can hide")

    def test_parse_set(self):
        self.assertIsInstance(
            parse("123 -> ax"), tuple, "`parse` must return a tuple")
        t = parse("123 -> ax")[-1]
        self.assertIsInstance(
            t, tuple,
            "the last element of result of `parse` must be a tuple, not {}".
            format(type(t).__name__))

        self.assertEqual(parse("123 -> ax"), ("ax", "SET", (123,)))
        self.assertEqual(parse("9 -> blabla"), ("blabla", "SET", (9,)))
        self.assertEqual(parse("42 -> b"), ("b", "SET", (42,)))
        self.assertEqual(parse("abc -> b"), ("b", "SET", ("abc",)))

    def test_parse_not(self):
        self.assertIsInstance(
            parse("NOT 123 -> ax"), tuple, "`parse` must return a tuple")
        t = parse("NOT 123 -> ax")[-1]
        self.assertIsInstance(
            t, tuple,
            "the last element of result of `parse` must be a tuple, not {}".
            format(type(t).__name__))

        self.assertEqual(parse("NOT 123 -> ax"), ("ax", "NOT", (123,)))
        self.assertEqual(parse("NOT 9 -> blabla"), ("blabla", "NOT", (9,)))
        self.assertEqual(parse("NOT 42 -> b"), ("b", "NOT", (42,)))
        self.assertEqual(parse("NOT abc -> b"), ("b", "NOT", ("abc",)))

    def test_parse_binary(self):
        self.assertIsInstance(
            parse("x AND yyy -> dd"), tuple, "`parse` must return a tuple")
        t = parse("x AND yyy -> dd")[-1]
        self.assertIsInstance(
            t, tuple,
            "the last element of result of `parse` must be a tuple, not {}".
            format(type(t).__name__))

        self.assertEqual(parse("x AND yyyyyy -> dd"),
                         ("dd", "AND", ("x", "yyyyyy")))
        self.assertEqual(parse("abc OR x -> z"), ("z", "OR", ("abc", "x")))
        self.assertEqual(parse("abc OR 15 -> z"), ("z", "OR", ("abc", 15)))
        self.assertEqual(parse("42 OR 15 -> z"), ("z", "OR", (42, 15)))
        self.assertEqual(parse("42 OR e -> z"), ("z", "OR", (42, "e")))
        self.assertEqual(parse("abc LSHIFT x -> z"),
                         ("z", "LSHIFT", ("abc", "x")))
        self.assertEqual(parse("abc RSHIFT x -> z"),
                         ("z", "RSHIFT", ("abc", "x")))

    def test_read(self):
        self.assertEqual(
                read("input1.txt"),
                [('x', 'SET', (123,)),
                 ('y', 'SET', (456,)),
                 ('d', 'AND', ('x', 'y')),
                 ('e', 'OR', ('x', 'y')),
                 ('f', 'LSHIFT', ('x', 2)),
                 ('g', 'RSHIFT', ('y', 2)),
                 ('h', 'NOT', ('x',)),
                 ('i', 'NOT', ('y',))]
        )


class Test07(unittest.TestCase):
    def test_outputs(self):
        p = read("input1.txt")
        self.assertSetEqual(outputs(p), set('xydefghi'))

        self.assertSetEqual(outputs([('a', 'SET', ('b',)),
                                     ('e', 'AND', (12, 'x')),
                                     ('f', 'AND', ('z', 5)),
                                     ('g', 'OR', (7, 5)),
                                     ('b', 'NOT', ('c', ))]), set('aefbg'))

        self.assertSetEqual(outputs([('a', 'SET', ('b',))]), {'a'})
        self.assertSetEqual(outputs([]), set())

    def test_inputs(self):
        p = read("input1.txt")
        self.assertSetEqual(inputs(p), {'x', 'y'})

        self.assertSetEqual(inputs([('a', 'SET', ('b',)),
                                    ('e', 'AND', (12, 'x')),
                                    ('f', 'AND', ('z', 5)),
                                    ('g', 'OR', (7, 5)),
                                    ('b', 'NOT', ('c',))]), set('bxcz'))

        self.assertSetEqual(inputs([('a', 'SET', ('b',))]), {'b'})
        self.assertSetEqual(inputs([('a', 'AND', ('b', 'c'))]), {'b', 'c'})
        self.assertSetEqual(inputs([]), set())
        self.assertSetEqual(inputs([('a', 'SET', (12,)),
                                    ('b', 'AND', (12, 15))]), set())

    def test_check_names(self):
        p = read("input1.txt")
        self.assertTrue(check_names(p))
        self.assertTrue(check_names([]))
        self.assertTrue(check_names([('a', 'SET', (12,)),
                                     ('b', 'AND', (12, 15))]))

        self.assertFalse(check_names([('a', 'SET', ('b',))]))
        self.assertFalse(check_names([('a', 'AND', ('b', 'c'))]))
        self.assertFalse(check_names([('a', 'AND', (5, 'c'))]))
        self.assertFalse(check_names([('a', 'AND', ('b', 12))]))
        self.assertFalse(check_names([('a', 'SET', ('b',)),
                                      ('e', 'AND', (12, 'x')),
                                      ('f', 'AND', ('z', 5)),
                                      ('g', 'OR', (7, 5)),
                                      ('b', 'NOT', ('c',))]))

    def test_check_operators(self):
        self.assertTrue(check_operators([]))
        self.assertTrue(check_operators([('a', 'SET', (12,))]))
        self.assertTrue(check_operators([('a', 'SET', (12,)),
                                         ('b', 'AND', (12, 15))]))
        self.assertTrue(check_operators([('a', 'SET', ('b',)),
                                         ('e', 'LSHIFT', (12, 'x')),
                                         ('f', 'RSHIFT', ('z', 5)),
                                         ('g', 'OR', (7, 5)),
                                         ('b', 'NOT', ('c',))]))

        self.assertFalse(check_operators([('a', 'WRONG', ('b',)),
                                          ('e', 'LSHIFT', (12, 'x')),
                                          ('f', 'RSHIFT', ('z', 5)),
                                          ('g', 'OR', (7, 5)),
                                          ('b', 'NOT', ('c',))]))

        self.assertFalse(check_operators([('a', 'SET', ('b',)),
                                          ('e', 'LSHIFT', (12, 'x')),
                                          ('f', 'NOSUCHTHING', ('z', 5)),
                                          ('g', 'OR', (7, 5)),
                                          ('b', 'NOT', ('c',))]))

        self.assertFalse(check_operators([('a', 'SET', ('b',)),
                                          ('e', 'LSHIFT', (12, 'x')),
                                          ('f', 'RSHIFT', ('z', 5)),
                                          ('g', 'OR', (7, 5)),
                                          ('b', 'FAIL', ('c',))]))


class Test08(unittest.TestCase):
    def test_compute_expr(self):
        self.assertEqual(compute_expr("SET", (42,)), 42)
        self.assertEqual(compute_expr("SET", (123,)), 123)

        self.assertEqual(compute_expr("NOT", (42,)), ~42)
        self.assertEqual(compute_expr("NOT", (0,)), ~0)

        self.assertEqual(compute_expr("AND", (12, 24)), 12 & 24)
        self.assertEqual(compute_expr("AND", (1234, 45678)), 1234 & 45678)

        self.assertEqual(compute_expr("OR", (12, 24)), 12 | 24)
        self.assertEqual(compute_expr("OR", (1234, 45678)), 1234 | 45678)

        self.assertEqual(compute_expr("LSHIFT", (123, 1)), 123 << 1)
        self.assertEqual(compute_expr("LSHIFT", (123, 3)), 123 << 3)

        self.assertEqual(compute_expr("RSHIFT", (123, 1)), 123 >> 1)
        self.assertEqual(compute_expr("RSHIFT", (123, 3)), 123 >> 3)

    def test_get_value(self):
        t = {'a': 13, 'bcd': 42, 'agr': 66}
        self.assertEqual(get_value(42, t), 42)
        self.assertEqual(get_value(15, t), 15)
        self.assertEqual(get_value('bcd', t), 42)
        self.assertEqual(get_value('agr', t), 66)
        self.assertEqual(get_value(42, {}), 42)
        self.assertEqual(get_value('x', {'x': 12}), 12)

        self.assertRaises(KeyError, get_value, 'bcd', {})
        self.assertRaises(KeyError, get_value, 'bcd', {'x': 12})

    def test_get_values(self):
        t = {'a': 13, 'bcd': 42, 'agr': 66}
        self.assertEqual(get_values((42, 15), t), (42, 15))
        self.assertEqual(get_values((42, 15, 1, 8), t), (42, 15, 1, 8))
        self.assertEqual(get_values((15,), t), (15,))

        self.assertEqual(get_values(('a', 15), t), (13, 15))
        self.assertEqual(get_values((7, 'bcd'), t), (7, 42))
        self.assertEqual(get_values(('bcd', 'agr'), t), (42, 66))
        self.assertEqual(get_values(('bcd', 13, 'agr'), t), (42, 13, 66))

        t = {'a': 13, 'x': 42}
        self.assertEqual(get_values(('a', 15, 'x'), t), (13, 15, 42))

        self.assertRaises(KeyError, get_values, (7, 'bcd'), t)

    def test_compute_list(self):
        self.assertEqual(compute_list([('a', 'SET', (12,))]), {'a': 12})
        self.assertEqual(compute_list([('a', 'SET', (12,)),
                                       ('b', 'SET', (42,))]),
                         {'a': 12, 'b': 42})

        self.assertEqual(compute_list([('a', 'SET', (12,)),
                                       ('b', 'SET', ('a',))]),
                         {'a': 12, 'b': 12})

        self.assertEqual(compute_list([('a', 'SET', (12,)),
                                       ('b', 'NOT', ('a',))]),
                         {'a': 12, 'b': ~12})

        self.assertEqual(compute_list([('a', 'SET', (12,)),
                                       ('b', 'NOT', ('a',)),
                                       ('c', 'LSHIFT', ('a', 2)),
                                       ('d', 'AND', ('b', 'c'))]),
                         {'a': 12,
                          'b': ~12,
                          'c': 12 << 2,
                          'd': ~12 & (12 << 2)})

        t = read("input1.txt")
        self.assertEqual(
            compute_list(t),
            {'d': 72, 'x': 123, 'h': -124, 'y': 456, 'e': 507, 'f': 492,
             'i': -457, 'g': 114})


class Test09(unittest.TestCase):
    def test_dict_expr(self):
        self.assertEqual(dict_expr([('a', 'SET', (12,))]),
                         {'a': ('SET', (12,))})
        self.assertEqual(dict_expr([('a', 'SET', (12,)),
                                    ('b', 'SET', (42,))]),
                         {'a': ('SET', (12,)), 'b': ('SET', (42,))})

        self.assertEqual(dict_expr([('a', 'SET', (12,)),
                                    ('b', 'SET', ('a',))]),
                         {'a': ('SET', (12,)), 'b': ('SET', ('a',))})

        self.assertEqual(dict_expr([('a', 'SET', (12,)),
                                    ('b', 'NOT', ('a',)),
                                    ('c', 'LSHIFT', ('a', 2)),
                                    ('d', 'AND', ('b', 'c'))]),
                         {'a': ('SET', (12,)),
                          'b': ('NOT', ('a', )),
                          'c': ('LSHIFT', ('a', 2)),
                          'd': ('AND', ('b', 'c'))})

    def test_compute(self):
        t = read('input1.txt')
        izrazi = dict_expr(t)
        self.assertEqual(compute('i', izrazi, {}), -457)

        t = read('input2.txt')
        izrazi = dict_expr(t)
        self.assertEqual(compute('a', izrazi, {}), 46065)

    def test_compute_file(self):
        self.assertEqual(compute_file('i', 'input1.txt'), -457)
        self.assertEqual(compute_file('a', 'input2.txt'), 46065)


class Test10(unittest.TestCase):
    def test_computable(self):
        self.assertTrue(computable([('a', 'SET', (12,))]))
        self.assertTrue(computable([]))
        self.assertTrue(computable([('a', 'SET', (12,)),
                                    ('b', 'SET', (42,))]))
        self.assertTrue(computable([('a', 'SET', (12,)),
                                    ('b', 'SET', ('a',))]))
        self.assertTrue(computable([('a', 'SET', ('b',)),
                                    ('b', 'SET', (12,))]))
        for t in permutations([('a', 'SET', (12,)),
                               ('b', 'NOT', ('a',)),
                               ('c', 'LSHIFT', ('a', 2)),
                               ('d', 'AND', ('b', 'c'))
                               ]):
            u = t[:]
            self.assertTrue(computable(t))
            self.assertEqual(u, t)

        for t in permutations([('a', 'AND', ('b', 'd')),
                               ('b', 'AND', ('c', 'd')),
                               ('c', 'LSHIFT', ('f', 2)),
                               ('d', 'OR', ('c', 'f')),
                               ('e', 'NOT', ('d',)),
                               ('f', 'SET', ('g',)),
                               ('g', 'SET', (42,))]):
            u = t[:]
            self.assertTrue(computable(t))
            self.assertEqual(u, t)

        self.assertTrue(computable(read("input1.txt")))
        self.assertTrue(computable(read("input2.txt")))

    def test_missing_variables(self):
        self.assertFalse(computable([('a', 'AND', ('b', 12))]))
        self.assertFalse(computable([('a', 'AND', ('12', 'b'))]))
        self.assertFalse(computable([('a', 'AND', ('c', 'b'))]))

        self.assertFalse(computable([('a', 'SET', (12,)),
                                     ('b', 'AND', ('a', 7)),
                                     ('c', 'NOT', ('a', )),
                                     ('d', 'AND', ('5', 'doesnotexist')),
                                     ('e', 'AND', (7, 'b')),
                                     ]))

    def test_cycles(self):
        self.assertFalse(computable([('a', 'SET', ('b',)),
                                     ('b', 'SET', ('a', ))]))
        self.assertFalse(computable([('a', 'SET', ('b',)),
                                     ('b', 'AND', ('a', 12))]))
        for t in permutations([('a', 'SET', ('b',)),
                               ('b', 'SET', ('c',)),
                               ('c', 'SET', ('d',)),
                               ('d', 'SET', ('e',)),
                               ('e', 'SET', ('f',)),
                               ('f', 'SET', ('g',)),
                               ('g', 'SET', ('a',))]):
            u = t[:]
            self.assertFalse(computable(t))
            self.assertEqual(u, t)

        for t in permutations([('a', 'AND', ('b', 'd')),
                               ('b', 'AND', ('c', 'd')),
                               ('c', 'LSHIFT', ('f', 2)),
                               ('d', 'OR', ('c', 'f')),
                               ('e', 'NOT', ('d',)),
                               ('f', 'SET', ('g',)),
                               ('g', 'SET', ('a',))]):
            u = t[:]
            self.assertFalse(computable(t))
            self.assertEqual(u, t)

        for t in permutations([('a', 'AND', ('b', 'd')),
                               ('b', 'AND', ('c', 'd')),
                               ('c', 'LSHIFT', ('e', 2)),
                               ('d', 'OR', ('c', 'f')),
                               ('e', 'NOT', ('d',)),
                               ('f', 'SET', ('g',)),
                               ('g', 'SET', (42,))]):
            u = t[:]
            self.assertFalse(computable(t))
            self.assertEqual(u, t)


if __name__ == "__main__":
    unittest.main()
