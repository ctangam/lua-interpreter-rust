local a,b = 100,200
t = {k=300, z=a, 10,20,30}
t.k = 400 -- set
t.x = t.z -- new
t.f = print -- new
t.f(t.k)
t.f(t.x)
t.f(t[2])
t.f(t[1000])
local key = "kkk"
local t = { 100, 200, 300;  -- list style
        x="hello", y="world";  -- record style
        [key]="vvv";  -- general style
      }
print(t[1])
print(t['x'])
print(t.kkk)
print(t)