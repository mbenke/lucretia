class A:
  a = 42
  b = 43
  d = 44
  e = 45

x = A()
# t = x.a + None
print("x.a", x.a)

x.a = True
# t = x.a + None
print("x.a", x.a)

A.a = None
# t = A.a + 7
print("x.a", x.a)

if False:
  x.b = None
# t = x.b + 7
print("x.b", x.b)

A.c = None
# t = x.b + 7
print("x.c", x.c)

if True:
  A.d = None
# t = x.d + 7
print("x.d", x.d)



class B(A):
  e = True
  f = 46

print("B.e", B.e)
print("B.f", B.f)
y = B()
print("y.e", y.e)
print("y.f", y.f)

y.a = None
if True:
  y.b = True
B.g = None
if True:
  B.h = None
