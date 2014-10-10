#!/usr/bin/env python2.7
#
# Usage: ./asm.py foo.s > foo.out

import sys
import re

filename = sys.argv[1]


def parse(line):
    line = line.strip()
    m = re.match(r'¥w+', line)
    opcode = m.group()
    t = line[m.end():].strip()
    operands = re.split(r',¥s*', t)
    return opcode, operands


def parse_addr(operand):
    m = re.match(r'¥[¥$(¥d+) *(¥+|-) *(¥d+)¥]', operand)
    if m:
        base = int(m.group(1))
        offset = int(m.group(3)) * (1 if m.group(2) == '+' else -1)
    else:
        m = re.match(r'¥[(.+)¥]', operands[1])
        if m.group(1)[0] == '$':
            base = m.group(1)
            offset = 0
        else:
            base = 0
            offset = m.group(1)
    return base, offset


def mkop(op, a0, a1, a2, imm):

    def decode(x):
        if isinstance(x, str):
            if x[0] == '$':
                x = x[1:]
            x = int(x)
        return x

    a0 = decode(a0)
    a1 = decode(a1)
    a2 = decode(a2)
    imm = decode(imm)

    head = chr(op * 8 + a0) + chr(a1 * 8 + a2)
    tail = chr((imm / 0x100) % 0x100) + chr(imm % 0x100)
    return head + tail


def on_add(operands):
    if len(operands) == 4:
        return mkop(0, operands[0], operands[1], operands[2], operands[3])
    elif operands[2][0] == '$':
        return mkop(0, operands[0], operands[1], operands[2], 0)
    else:
        return mkop(0, operands[0], operands[1], 0, operands[2])

def on_sub(operands):
    return mkop(1, operands[0], operands[1], operands[2], 0)

def on_shift(operands):
    return mkop(2, operands[0], operands[1], 0, operands[2])

def on_fneg(operands):
    return mkop(3, operands[0], operands[1], 0, 0)

def on_fadd(operands):
    return mkop(4, operands[0], operands[1], operands[2], 0)

def on_fmul(operands):
    return mkop(5, operands[0], operands[1], operands[2], 0)

def on_finv(operands):
    return mkop(6, operands[0], operands[1], 0, 0)

def on_fsqrt(operands):
    return mkop(7, operands[0], operands[1], 0, 0)

def on_load(operands):
    base, offset = parse_addr(operands[1])
    return mkop(8, operands[0], 0, base, offset)

def on_store(operands):
    base, offset = parse_addr(operands[1])
    return mkop(9, operands[0], 0, base, offset)

def on_read(operands):
    return mkop(10, operands[0], 0, 0, 0)

def on_write(operands):
    return mkop(11, operands[0], 0, 0, 0)

def on_beq(operands):
    return mkop(12, operands[0], operands[1], operands[2], operands[3])

def on_ble(operands):
    return mkop(13, operands[0], operands[1], operands[2], operands[3])


table = {
    'add': on_add,
    'sub': on_sub,
    'shift': on_shift,
    'fneg': on_fneg,
    'fadd': on_fadd,
    'fmul': on_fmul,
    'finv': on_finv,
    'fsqrt': on_fsqrt,
    'load': on_load,
    'store': on_store,
    'read': on_read,
    'write': on_write,
    'beq': on_beq,
    'ble': on_ble,
}


lines0 = []
with open(filename, 'r') as f:
    lines0 = map(lambda x: x.strip(), list(f))


# 1. macro expansion
lines1 = []
for line in lines0:
    if line[0] == '.':
        print >> sys.stderr, '[WARNING] unsupported pseudoinstruction', line
    else:
        # TODO
        line = line.replace('$sp', '$13')
        line = line.replace('$bp', '$14')
        line = line.replace('$ip', '$15')
        lines1.append(line)


# 2. label resolution (by 2-pass algorithm)
lines2 = []
i = 0
labels = {}
for line in lines1:
    if line[-1] == ':':
        labels[line[:-1]] = str(i)
    else:
        i += 1
        lines2.append(line)
lines3 = []
for line in lines2:
    opcode, operands = parse(line)

    def subst(ident):
        if ident in labels:
            return labels[ident]
        else:
            return ident
    operands = map(subst, operands)
    lines3.append("{} {}".format(opcode, ', '.join(operands)))


# 3. assemble
for line in lines3:
    opcode, operands = parse(line)

    if opcode in table:
        byterepr = table[opcode](operands)   # Result!
        print >> sys.stderr, 'op = {}, operands = {}, code = {}'.format(opcode, operands, repr(byterepr))
        sys.stdout.write(byterepr)
    else:
        print >> sys.stderr, '[ERROR] unsupported opcode: {}¥n  {}'.format(opcode, line)
        sys.exit(1)
