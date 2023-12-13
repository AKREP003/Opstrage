from logical_prediction import gr
import string
import random




def binToInt(x):

    #x = x[::-1]


    power = 0

    result = 0

    for i in x:

        if i:

            result += 2 ** power

        power += 1

    return result


def concat(x):

    res = ""

    for i in x:

        res = res + " " + str(i)

    return res

def reverse(T):

    x = []

    for vaL in range(len(T)):

        g = []

        for i in T[vaL]:

            f = (i[0], not i[1])

            g.append(f)

        x.append(g)

    return x


def Ksize(K):
    l = 0

    for i in K:

        l += len(i)

    return l



def catculate(T):
    size = 0

    for vaL in T:

        t = []

        for i in vaL:
            t.append(i[1])

        g = []

        for i in range(len(t)):

            if t[i]:
                g.append(i)

        if not g:

            continue

        K = gr(concat(g), "")

        size += Ksize(K)

    return size

def generateString(le):
    chars = string.digits
    return ''.join(random.choice(chars) for _ in range(le))

def int_to_bool_list(num):
   bin_string = format(num, '04b')

   return [x == '1' for x in bin_string[::-1]]


def strTobool(x):
    res = []

    for i in x:

        res.append(int_to_bool_list(ord(i)))

    return res

def contentToTruthTables(x):

    r1 = []

    for i in range(len(x[0])):

        r2 = []

        for j in x:

            r2.append((int_to_bool_list(i),j[i]))

        r1.append(r2)

    return r1

for LEN in range(1,100):

    st = contentToTruthTables(strTobool(generateString(LEN)))

    rev = catculate(reverse(st))

    F = catculate(st)

    print(min(rev, F))



