print "tab:\thi" -- tab
print "\xE4\xBD\xA0\xE5\xA5\xBD" -- 你好
print "\xE4\xBD" -- invalid UTF-8
print "\72\101\108\108\111" -- Hello
print "null: \0." -- '\0'
local a = 456
a = 123
print(a)
a = a
print(a)
a = g
print(a)
g = 123
print(g)
g = a
print(g)
g = g2
print(g)