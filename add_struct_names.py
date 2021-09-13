
mexe = ''

with open('arb_os/arbos_before.mexe') as file:
    mexe = file.readlines()[0]

text = 'Struct":['

struct = mexe.find(text, 0) + len(text)

while struct is not 8:

    print(struct, mexe[(struct - len(text)):struct])

    mexe = mexe[0:struct] + '[' + mexe[struct:]

    i = struct + 1
    count = 1

    while count > 0:
        if mexe[i] == '[':
            count += 1
        elif mexe[i] == ']':
            count -= 1
        i += 1

    mexe = mexe[0:i] + ']' + mexe[i:]
    
    print(struct, mexe[(struct - len(text)):(struct + 1)])
    
    struct = mexe.find(text, struct) + len(text)

with open("arb_os/arbos_before.mexe.new", "w") as file:
    file.write(mexe)
