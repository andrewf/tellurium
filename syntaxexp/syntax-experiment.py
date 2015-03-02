square = ('[', ']')
curly = ('{', '}')
wordy = ('', 'end')
parens = ('(', ')')


protocol = '#'
subscript = square
generic_trigger = '!'
segment = square

template = """segment{10}.text{11} {3}
    var {6}
        x i16 = 17
        y []u8 = "hello"
    {7}
    generic{0}N uintp, T{1}
    fun{5}cdecl fuzz(arr ptr [N]T) -> T {3}
        print{2}{0}T{1}(@arr{8}3{9})
        return @arr{8}0{9}
    {4}
{4}
"""

i = 0
for generic in [square, curly]:
    for generic_trigger in ['', '!']:
        for block in [curly, wordy]:
            for vargroup in [parens]:#, curly]:
                    # reject invalid combinations
                    #if block == generic:
                    #    break
                    if not generic_trigger and (generic == subscript):
                        break
                    # generate sample
                    out = template.format(
                        generic[0], generic[1],    # 0, 1
                        generic_trigger,           # 2
                        block[0], block[1],        # 3, 4
                        protocol,                  # 5
                        vargroup[0], vargroup[1],  # 6, 7
                        subscript[0], subscript[1],# 8, 9
                        segment[0], segment[1]     # 10, 11
                    )
                    print "try%d.te" % i
                    open("try%d.te" % i, 'w').write(out)
                    i += 1

