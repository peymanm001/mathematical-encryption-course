# In The Name of GOD
# Polynomial
# peymanm001

# peymanm001.ir


class Mod:
    def __init__(self, value, mod=5):
        assert (mod >= 2)
        if value < 0:
            self.value = (-(value // mod)) * mod + value
        else:
            self.value = value % mod
        self.mod = mod

    def __neg__(self):
        return Mod(-self.value, self.mod)

    def inverse(self):
        for i in range(1, self.mod):
            m = Mod(self.value * i, self.mod)
            if m.value == 1:
                return Mod(i, self.mod)
        return False

    def __add__(self, other):
        assert (self.mod == other.mod)
        return Mod(self.value + other.value, self.mod)

    def __radd__(self, other):
        if other == 0:
            return self
        return self.__add__(other)

    def __sub__(self, other):
        assert (self.mod == other.mod)
        return Mod(self.value - other.value, self.mod)

    def __mul__(self, other):
        assert (self.mod == other.mod)
        return Mod(self.value * other.value, self.mod)

    def __floordiv__(self, other):
        assert (other.inverse is not False)
        return self * other.inverse()

    def __truediv__(self, other):
        return self.__floordiv__(other)

    def __eq__(self, other):
        if other == 0:
            return self.value == 0
        assert (self.mod == other.mod)
        return self.value == other.value

    def __gt__(self, other):
        if other == 0:
            return self.value > 0
        assert (self.mod == other.mod)
        return self.value > other.value

    def __lt__(self, other):
        if other == 0:
            return self.value < 0
        assert (self.mod == other.mod)
        return self.value < other.value

    def __str__(self):
        return str(self.value)


class Term:
    def __init__(self, degree, coef):
        assert degree >= 0
        if coef == 0 or degree < 0:
            raise Exception(":(")
        self.degree = degree
        self.coef = coef

    def __mul__(self, other):
        deg = self.degree + other.degree
        coef = self.coef * other.coef
        return Term(deg, coef)

    def __neg__(self):
        return Term(self.degree, -self.coef)

    def __str__(self):
        if self.degree == 0:
            return str(self.coef)
        if self.degree == 1:
            return str(self.coef) + "x"
        return str(self.coef) + "x^" + str(self.degree)


class Polynomial:
    def __init__(self, terms):
        self.terms = sorted(terms, key=lambda term: term.degree, reverse=True)
        if terms == []:
            self.degree = -1
        else:
            self.degree = self.terms[0].degree

    def isEmpty(self):
        return len(self.terms) == 0

    def __addTerm(self, terms, term):
        if term.coef == 0:
            return terms
        if terms == []:
            return [term]
        if term.degree > terms[0].degree:
            return [term] + terms
        if term.degree < terms[0].degree:
            return [terms[0]] + self.__addTerm(terms[1:], term)
        coef = terms[0].coef + term.coef
        if coef == 0:
            return terms[1:]
        return [Term(term.degree, coef)] + terms[1:]

    def addTerm(self, term):
        # self.terms = sorted(self.__addTerm(self.terms, term), key=lambda term: term.degree, reverse=True)
        # return self
        return Polynomial(sorted(self.__addTerm(self.terms, term), key=lambda term: term.degree, reverse=True))

    def __add__(self, other):
        def add(p1, p2):
            if p1 == []:
                return p2
            if p2 == []:
                return p2
            return add(p1[1:], other.__addTerm(p2, p1[0]))
        return Polynomial(add(self.terms, other.terms))

    def __radd__(self, other):
        if other == 0:
            return self
        return self.__add__(other)

    def mulTerm(self, term):
        # self.terms = sorted([term * t for t in self.terms], key=lambda term: term.degree, reverse=True)
        # return self
        lst = []
        for t in self.terms:
            tmp = term *t
            lst.append(tmp)
        return Polynomial(sorted(lst, key=lambda term: term.degree, reverse=True))
        # return Polynomial(sorted([term * t for t in self.terms], key=lambda term: term.degree, reverse=True))

    def __mul__(self, other):
        return sum([Polynomial([term * t for t in self.terms]) for term in other.terms])

    def __sub__(self, other):

        return self + (-other)

    def __div(self, dividend, divisor, qoutient):
            if dividend.degree < divisor.degree:
                return qoutient, dividend
            coef = dividend.terms[0].coef / divisor.terms[0].coef
            deg = dividend.terms[0].degree - divisor.terms[0].degree
            term = Term(deg, coef)
            tmp = - divisor.mulTerm(term)
            return self.__div(dividend + tmp, divisor, qoutient.addTerm(term))

    def __truediv__(self, other):
        return self.__div(self, other, Polynomial([]))[0]

    def __floordiv__(self, other):
        return self.__truediv__(self, other)

    def __mod__(self, other):
        return self.__div(self, other, Polynomial([]))[1]

    def __neg__(self):
        return Polynomial([-term for term in self.terms])

    def __str__(self):
        if self.isEmpty():
            return "0"
        tmp = ""
        for term in self.terms[:-1]:
            tmp += str(term) + " + "
        return tmp + str(self.terms[-1])


class Matrix:
    def __init__(self, data, size=-1, itemType=int):
        self.itemType = itemType
        if size != -1:
            if data.lower() == 'i':
                items = []
                for i in range(size):
                    items.append([])
                    for j in range(size):
                        if i == j:
                            items[i].append(itemType(1))
                        else:
                            items[i].append(itemType(0))
            elif data.lower() == 'z':
                items = [[0 for i in range(size)] for j in range(size)]
        else:
            items = data
        self.rows = [[item for item in row] for row in items]
        if self.rows == []:
            self.cols = []
        else:
            self.cols = [[items[i][j] for j in range(len(items))] for i in range(len(items[0]))]

    def item(self, i, j):
        return self.rows[i][j]

    def setItem(self, i, j, value):
        self.rows[i][j] = value

    def swapRows(self, i, j):
        tmp = self.rows[i][:]
        self.rows[i] = self.rows[j][:]
        self.rows[j] = tmp

    def inv(self):
        mcpy = Matrix(self.rows[:])
        size = len(self.rows)
        iMat = Matrix('i', size, self.itemType)
        for i in reversed(range(1, size)):
            if self.item(i-1, 0) < self.item(i, 0):
                self.swapRows(i-1, i)
                iMat.swapRows(i-1, i)

        for i in range(size):
            for j in range(size):
                if i != j:
                    # fixme : fucking zero may be exist in next row too
                    # todo: check all rows for select best row
                    if self.itemType(0) == self.item(i, i):
                        self.swapRows(i, i+1)
                        iMat.swapRows(i, i+1)
                    #
                    tmp = self.item(j, i) / self.item(i, i)
                    for k in range(size):
                        value = self.item(j, k) - self.item(i, k) * tmp
                        self.setItem(j, k, value)
                        value = iMat.item(j, k) - iMat.item(i, k) * tmp
                        iMat.setItem(j, k, value)

        for i in range(size):
            tmp = self.item(i, i)
            for j in range(size):
                self.setItem(i, j, self.item(i, j) / tmp)
                iMat.setItem(i, j, iMat.item(i, j) / tmp)

        self.rows = mcpy.rows
        self.cols = mcpy.cols
        return iMat

    def __neg__(self):
        data = [[-item for item in row] for row in self.rows]
        return Matrix(data)

    def __add__(self, other):
        assert (len(self.rows) == len(other.rows)
                and len(self.cols) == len(other.cols)
                and self.itemType == other.itemType)
        res = Matrix([[self.itemType(0) for i in range(len(self.cols))] for j in range(len(self.rows))])
        for i in range(len(self.rows)):
            for j in range(len(self.cols)):
                res.setItem(i, j, self.item(i, j) + other.item(i, j))
        return res

    def __mul__(self, other):
        assert (len(self.cols) == len(other.rows)
                and self.itemType == other.itemType)
        res = Matrix([[self.itemType(0) for i in range(len(other.cols))] for j in range(len(self.rows))])
        for i in range(len(self.rows)):
            for j in range(len(other.cols)):
                s = 0
                for k in range(len(self.cols)):
                    s += self.item(i,k) * other.item(k, j)
                res.setItem(i, j, s)
        return res

    def __sub__(self, other):
        assert (len(self.rows) == len(other.rows) and
                len(self.cols == len(other.cols) and
                self.itemType == other.itemType))
        return self + (-other)

    def __str__(self):
        s = '\n'.join([' '.join([str(item) for item in row]) for row in self.rows])
        return s + '\n'


p1 = Polynomial([Term(4,Mod(2,5)), Term(3,Mod(1,5)), Term(5,Mod(3,5)), Term(6,Mod(1,5))])
p2 = Polynomial([Term(4,Mod(3,5)), Term(3,Mod(1,5)), Term(1,Mod(2,5)), Term(0, Mod(2,5))])

print(p1)
print(p2)
print(p1 * p2)
print(p1 / p2)
print(p1 % p2)

m1 = Matrix([[Mod(2,5), Mod(3,5), Mod(1,5)],
             [Mod(1,5), Mod(4,5), Mod(2,5)],
             [Mod(1,5), Mod(0,5), Mod(2,5)]], itemType=Mod)

m2 = Matrix([[2, 3, 1],
             [1, 4, 2],
             [1, 0, 2]])
m1inv = m1.inv()

print(m1)
print(-m1)
print(m1inv)
print(m1 + m1inv)
print(m1 * m1inv)
# print(Matrix("i", 3, Mod))

