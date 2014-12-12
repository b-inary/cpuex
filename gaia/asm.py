#!/usr/bin/env python2.7

import sys
import os.path
import re
import struct
import argparse


srcs = {'_main': {0: 'br main'}}
filename = ''
pos = 0

def error(msg):
    print >> sys.stderr, '{}:{}: error:'.format(filename, pos), msg
    print >> sys.stderr, '    ' + srcs[filename][pos]
    sys.exit(1)


# ----------------------------------------------------------------------
#       utility functions (mainly parsing)
# ----------------------------------------------------------------------

regs = {'rsp': 30, 'rbp': 31}
for i in range(32):
    regs['r' + str(i)] = i

def is_reg(operand):
    return operand in regs

def parse_imm(operand):
    try:
        imm = int(operand, 0)
        return True, imm
    except ValueError:
        return False, 0

def parse_float(operand):
    try:
        f = float(operand)
        return True, f
    except ValueError:
        return False, 0.0

def float_bit(f):
    try:
        s = struct.pack('>f', f)
        return struct.unpack('>i', s)[0]
    except OverflowError:
        error('float too large')

def parse_memaccess(operand):
    m = re.match(r'\[(r\w+)\s*([+-])\s*(\w+)\]$', operand)
    if m:
        base = m.group(1)
        disp = m.group(2) + m.group(3)
        if is_reg(base) and parse_imm(disp)[0]:
            return True, base, int(disp, 0)
    m = re.match(r'\[(r\w+)\]$', operand)
    if m and is_reg(m.group(1)):
        return True, m.group(1), 0
    m = re.match(r'\[([+-]?\w+)\]$', operand)
    if m and parse_imm(m.group(1))[0]:
        return True, 'r0', int(m.group(1), 0)
    return False, 'r0', 0

def check_operands_n(operands, n, m=-1):
    if len(operands) < n:
        error('too few operands')
    if len(operands) > max(n, m):
        error('too many operands')

def check_imm_range(imm, b):
    x = 1 << (b - 1)
    return -x <= imm < x

def cast_short(imm):
    imm = imm & 0xffff
    return imm if imm < 0x8000 else imm - 0x10000

def regnum(reg):
    if reg not in regs:
        error('invalid register name: ' + reg)
    return regs[reg]

def code_i(rx, ra, rb, imm, tag):
    x = regnum(rx)
    a = regnum(ra)
    b = regnum(rb)
    success, imm = parse_imm(imm)
    if not success:
        error('invalid syntax: expected immediate value')
    if not check_imm_range(imm, 8):
        error('immediate value exceeds valid range')
    c1 = x >> 1
    c2 = ((x & 1) << 7) + (a << 2) + (b >> 3)
    c3 = ((b & 7) << 5) + ((imm >> 3) & 31)
    c4 = ((imm & 7) << 5) + tag
    return chr(c1) + chr(c2) + chr(c3) + chr(c4)

def code_f(rx, ra, rb, sign, tag):
    x = regnum(rx)
    a = regnum(ra)
    b = regnum(rb)
    c1 = (1 << 4) + (x >> 1)
    c2 = ((x & 1) << 7) + (a << 2) + (b >> 3)
    c3 = ((b & 7) << 5)
    c4 = (sign << 5) + tag
    return chr(c1) + chr(c2) + chr(c3) + chr(c4)

def code_m(op, rx, ra, pred, disp):
    x = regnum(rx)
    a = regnum(ra)
    success, disp = parse_imm(disp)
    if not success:
        error('invalid syntax: expected displacement')
    if not check_imm_range(disp, 16):
        error('displacement exceeds valid range')
    c1 = (op << 4) + (x >> 1)
    c2 = ((x & 1) << 7) + (a << 2) + pred
    c3 = (disp >> 8) & 255
    c4 = disp & 255
    return chr(c1) + chr(c2) + chr(c3) + chr(c4)

def parse(line):
    line = line.strip()
    m = re.match(r'\S+', line)
    mnemonic = m.group()
    t = line[m.end():].strip()
    operands = re.split(r',\s*', t)
    if operands == ['']:
        return mnemonic, []
    return mnemonic,  operands


# ----------------------------------------------------------------------
#       mnemonic definitions
# ----------------------------------------------------------------------

def on_alu3(operands, tag):
    check_operands_n(operands, 3)
    return code_i(operands[0], operands[1], operands[2], '0', tag)

def on_alu4(operands, tag):
    check_operands_n(operands, 4)
    return code_i(operands[0], operands[1], operands[2], operands[3], tag)

def on_fpu2(operands, sign, tag):
    check_operands_n(operands, 2)
    return code_f(operands[0], operands[1], 'r0', sign, tag)

def on_fpu3(operands, sign, tag):
    check_operands_n(operands, 3)
    return code_f(operands[0], operands[1], operands[2], sign, tag)

# def on_other0(operands, op, pred):
#     check_operands_n(operands, 2)
#     return code_m(op, 'r0', 'r0', pred, '0')

def on_other1(operands, op, pred):
    check_operands_n(operands, 1)
    return code_m(op, operands[0], 'r0', pred, '0')

def on_other2(operands, op, pred):
    check_operands_n(operands, 2)
    return code_m(op, operands[0], 'r0', pred, operands[1])

def on_other3(operands, op, pred):
    check_operands_n(operands, 3)
    return code_m(op, operands[0], operands[1], pred, operands[2])

alu3_table = {
    'cmpne':     8,
    'cmpeq':     9,
    'cmplt':    10,
    'cmple':    11,
    'fcmpne':   12,
    'fcmpeq':   13,
    'fcmplt':   14,
    'fcmple':   15,
}

alu4_table = {
    'add':       0,
    'sub':       1,
    'shl':       2,
    'shr':       3,
    'sar':       4,
    'and':       5,
    'or':        6,
    'xor':       7
}

fpu2_table = {
    'finv':      4,
    'fsqrt':     5,
    'ftoi':      6,
    'itof':      7,
    'floor':     8
}

fpu3_table = {
    'fadd':      0,
    'fsub':      1,
    'fmul':      2,
    'fdiv':      3
}

# other0_table = {
#     'sysenter':  4,
#     'sysexit':   5
# }

other1_table = {
    'jr':       12
}

other2_table = {
    'ldl':       2,
    'jl':       11
}

other3_table = {
    'ldh':       3,
    'st':        6,
    'ld':        8,
    'bne':      13,
    'beq':      15
}

sign_table = {
    '':          0,
    'neg':       1,
    'abs':       2,
    'abs.neg':   3
}

def code(mnemonic, operands):
    if mnemonic in alu3_table:
        return on_alu3(operands, alu3_table[mnemonic])
    if mnemonic in alu4_table:
        return on_alu4(operands, alu4_table[mnemonic])
    fpu_mnemonic, fpu_suffix = mnemonic, ''
    if '.' in mnemonic:
        fpu_mnemonic, fpu_suffix = mnemonic.split('.', 1)
    if fpu_mnemonic in fpu2_table:
        return on_fpu2(operands, sign_table[fpu_suffix], fpu2_table[fpu_mnemonic])
    if fpu_mnemonic in fpu3_table:
        return on_fpu3(operands, sign_table[fpu_suffix], fpu3_table[fpu_mnemonic])
    pred = 0
    if mnemonic in ['jl', 'jr', 'bne+', 'beq+']:
        pred = 3
    if mnemonic in ['bne-', 'bne+']:
        mnemonic = 'bne'
    if mnemonic in ['beq-', 'beq+']:
        mnemonic = 'beq'
    if mnemonic in other1_table:
        return on_other1(operands, other1_table[mnemonic], pred)
    if mnemonic in other2_table:
        return on_other2(operands, other2_table[mnemonic], pred)
    if mnemonic in other3_table:
        return on_other3(operands, other3_table[mnemonic], pred)
    error('unknown mnemonic')


# ----------------------------------------------------------------------
#       macro definitions
# ----------------------------------------------------------------------

def expand_nop(operands):
    check_operands_n(operands, 0)
    return ['add r0, r0, r0, 0']

def expand_mov(operands):
    check_operands_n(operands, 2)
    if is_reg(operands[0]) and is_reg(operands[1]):
        return ['add {}, {}, r0, 0'.format(operands[0], operands[1])]
    success, base, disp = parse_memaccess(operands[1])
    if success:
        if disp & 3 != 0:
            error('displacement must be multiple of 4')
        return ['ld {}, {}, {}'.format(operands[0], base, disp >> 2)]
    success, base, disp = parse_memaccess(operands[0])
    if success:
        if disp & 3 != 0:
            error('displacement must be multiple of 4')
        return ['st {}, {}, {}'.format(operands[1], base, disp >> 2)]
    success, imm = parse_imm(operands[1])
    if success:
        if check_imm_range(imm, 16):
            return ['ldl {}, {}'.format(operands[0], operands[1])]
        return [
            'ldl {}, {}'.format(operands[0], cast_short(imm)),
            'ldh {}, {}, {}'.format(operands[0], operands[0], cast_short(imm >> 16))
        ]
    success, imm = parse_float(operands[1])
    if success:
        b = float_bit(imm)
        return [
            'ldl {}, {}'.format(operands[0], cast_short(b)),
            'ldh {}, {}, {}'.format(operands[0], operands[0], cast_short(b >> 16))
        ]
    error('invalid syntax')

def expand_alu(op, operands):
    check_operands_n(operands, 3, 4)
    if (len(operands) == 4):
        return ['{} {}'.format(op, ', '.join(operands))]
    if is_reg(operands[2]):
        return ['{} {}, 0'.format(op, ', '.join(operands))]
    success, imm = parse_imm(operands[2])
    if success:
        if check_imm_range(imm, 8):
            return ['{} {}, {}, r0, {}'.format(op, operands[0], operands[1], imm)]
        return expand_mov(['r29', str(imm)]) + ['{} {}, {}, r29, 0'.format(op, operands[0], operands[1])]
    error('invalid syntax')

def expand_and(operands):
    check_operands_n(operands, 3, 4)
    if (len(operands) == 4):
        return ['and {}'.format(', '.join(operands))]
    if is_reg(operands[2]):
        return ['and {}, -1'.format(', '.join(operands))]
    success, imm = parse_imm(operands[2])
    if success:
        if check_imm_range(imm, 8):
            return ['and {}, {}, {}, {}'.format(operands[0], operands[1], operands[1], imm)]
        return expand_mov(['r29', str(imm)]) + ['and {}, {}, r29, -1'.format(operands[0], operands[1])]
    error('invalid syntax')

def expand_neg(operands):
    check_operands_n(operands, 2)
    return ['sub {}, r0, {}, 0'.format(operands[0], operands[1])]

def expand_not(operands):
    check_operands_n(operands, 2)
    return ['xor {}, {}, r0, -1'.format(operands[0], operands[1])]

def expand_cmpgt(operands):
    check_operands_n(operands, 3)
    return ['cmple {}, {}, {}'.format(operands[0], operands[2], operands[1])]

def expand_cmpge(operands):
    check_operands_n(operands, 3)
    return ['cmplt {}, {}, {}'.format(operands[0], operands[2], operands[1])]

def expand_fcmpgt(operands):
    check_operands_n(operands, 3)
    return ['fcmple {}, {}, {}'.format(operands[0], operands[2], operands[1])]

def expand_fcmpge(operands):
    check_operands_n(operands, 3)
    return ['fcmplt {}, {}, {}'.format(operands[0], operands[2], operands[1])]

def expand_read(operands):
    check_operands_n(operands, 1)
    return [
        'ld r29, r0, 0x3000',
        'beq r29, r0, -2',
        'ld {}, r0, 0x3004'.format(operands[0])
    ]

def expand_write(operands):
    check_operands_n(operands, 1)
    return [
        'ld r29, r0, 0x3008',
        'beq r29, r0, -2',
        'st {}, r0, 0x300c'.format(operands[0])
    ]

def expand_br(operands):
    check_operands_n(operands, 1)
    return ['jl r29, {}'.format(operands[0])]

def expand_push(operands):
    check_operands_n(operands, 1)
    return [
        'sub rsp, rsp, r0, 4',
        'st {}, rsp, 0'.format(operands[0])
    ]

def expand_pop(operands):
    check_operands_n(operands, 1)
    return [
        'ld {}, rsp, 0'.format(operands[0]),
        'add rsp, rsp, r0, 4'
    ]

def expand_call(operands):
    check_operands_n(operands, 1)
    return [
        'st rbp, rsp, -1',
        'sub rsp, rsp, r0, 4',
        'add rbp, rsp, r0, 0',
        'jl r28, {}'.format(operands[0]),
        'add rsp, rbp, r0, 4',
        'ld rbp, rsp, -1'
    ]

def expand_ret(operands):
    check_operands_n(operands, 0)
    return ['jr r28']

def expand_enter(operands):
    check_operands_n(operands, 1)
    success, imm = parse_imm(operands[0])
    if success:
        return expand_alu('sub', ['rsp', 'rsp', str(imm * 4 + 4)]) + ['st r28, rsp, 0']
    error('invalid syntax')

def expand_leave(operands):
    check_operands_n(operands, 0)
    return ['ld r28, rsp, 0']

def expand_halt(operands):
    check_operands_n(operands, 0)
    return ['beq+ r31, r31, -1']

def expand_branch(op, operands, pred, swap):
    check_operands_n(operands, 3)
    pred = '+' if (pred == '+' and not swap) or (pred == '-' and swap) else \
           '-' if (pred == '-' and not swap) or (pred == '+' and swap) else \
           ''
    return [
        '{} r29, {}, {}'.format(op, operands[swap], operands[1 - swap]),
        'bne{} r0, r29, {}'.format(pred, operands[2])
    ]

macro_table = {
    'nop':      expand_nop,
    'mov':      expand_mov,
    'and':      expand_and,
    'neg':      expand_neg,
    'not':      expand_not,
    'cmpgt':    expand_cmpgt,
    'cmpge':    expand_cmpge,
    'fcmpgt':   expand_fcmpgt,
    'fcmpge':   expand_fcmpge,
    'read':     expand_read,
    'write':    expand_write,
    'br':       expand_br,
    'push':     expand_push,
    'pop':      expand_pop,
    'call':     expand_call,
    'ret':      expand_ret,
    'enter':    expand_enter,
    'leave':    expand_leave,
    'halt':     expand_halt
}

branch_table = {
    'blt':  ('cmplt', 0),
    'ble':  ('cmple', 0),
    'bgt':  ('cmple', 1),
    'bge':  ('cmplt', 1),
    'bfne': ('fcmpne', 0),
    'bfeq': ('fcmpeq', 0),
    'bflt': ('fcmplt', 0),
    'bfle': ('fcmple', 0),
    'bfgt': ('fcmple', 1),
    'bfge': ('fcmplt', 1),
}

def expand_macro(mnemonic, operands):
    if mnemonic in macro_table:
        return macro_table[mnemonic](operands)
    if mnemonic in ['add', 'sub', 'shl', 'shr', 'sar', 'or', 'xor']:
        return expand_alu(mnemonic, operands)
    m = re.match(r'(\w+)([+-]?)$', mnemonic)
    if m:
        br_mnemonic, pred = m.groups()
        if br_mnemonic in branch_table:
            op, swap = branch_table[br_mnemonic]
            return expand_branch(op, operands, pred, swap)
    return ['{} {}'.format(mnemonic, ', '.join(operands)).strip()]


# ----------------------------------------------------------------------
#       label resolution
# ----------------------------------------------------------------------

labels = {}
rev_labels = {}

def add_label(label, i):
    dic = labels.get(label, {})
    if filename in dic and dic[filename][0] >= 0:
        error('duplicate declaration of label \'{}\''.format(label))
    val = dic.get(filename, [-1, False, False])
    dic[filename] = [i, val[1], False]
    labels[label] = dic
    rev_labels[i] = rev_labels.get(i, []) + [label]

def add_global(label):
    dic = labels.get(label, {})
    val = dic.get(filename, [-1, False, False])
    dic[filename] = [val[0], True, False]
    labels[label] = dic

def check_global(label):
    if labels[label][filename][0] < 0:
        error('label \'{}\' is not declared'.format(label))

def subst(label, cur):
    if parse_imm(label)[0]:
        return label
    if label not in labels:
        error('label \'{}\' is not declared'.format(label))
    if filename in labels[label]:
        labels[label][filename][2] = True
        return str(labels[label][filename][0] - cur - 1)
    else:
        decl = ''
        for key in labels[label]:
            if labels[label][key][1]:
                if decl:
                    error('label \'{}\' is declared in multiple files ({}, {})'.format(label, decl, key))
                decl = key
        if not decl:
            error('label \'{}\' is not declared'.format(label))
        labels[label][decl][2] = True
        return str(labels[label][decl][0] - cur - 1)

def warn_unused_label(label):
    if not labels[label][filename][2] and not (filename == library and labels[label][filename][1]):
        print >> sys.stderr, '{}:{}: warning: unused label \'{}\''.format(filename, pos, label)

def show_label(i):
    if i in rev_labels:
        return '# {}'.format(', '.join(rev_labels[i]))
    return ''


# ----------------------------------------------------------------------
#       main process
# ----------------------------------------------------------------------

# parse command line arguments
argparser = argparse.ArgumentParser(usage='%(prog)s [options] file...')
argparser.add_argument('inputs', nargs='*', help='input files', metavar='file...')
argparser.add_argument('-o', help='set output file to <file>', metavar='<file>')
argparser.add_argument('-s', action='store_const', const=True, help='output preprocessed assembly')
argparser.add_argument('-k', action='store_const', const=True, help='output as array of std_logic_vector format')
argparser.add_argument('-a', action='store_const', const=True, help='output as rs232c send test format')
args = argparser.parse_args()
if args.inputs == []:
    argparser.print_help()
    sys.exit(1)

# 0. preprocess
lines0 = [('br main', '_main', 0)]
for filename in args.inputs:
    with open(filename, 'r') as f:
        filename = re.sub(r'.*[/\\]', '', filename)
        srcs[filename] = {}
        for pos, line in enumerate(f):
            line = line.strip()
            srcs[filename][pos + 1] = line
            line = re.sub(r'[;#].*', '', line).strip()
            if line:
                lines0.append((line, filename, pos + 1))

# 1. macro expansion
lines1 = []
for line, filename, pos in lines0:
    mnemonic, operands = parse(line)
    lines = expand_macro(mnemonic, operands)
    lines1.extend(map(lambda x: (x, filename, pos), lines))

# 2. label resolution (by 2-pass algorithm)
i = 0
lines2 = []
lines3 = []
for line, filename, pos in lines1:
    mnemonic, operands = parse(line)
    if mnemonic[-1] == ':':
        if len(operands) > 0:
            error('invalid syntax')
        add_label(line[:-1], i)
    elif mnemonic == '.global':
        check_operands_n(operands, 1)
        add_global(operands[0])
    else:
        lines2.append((line, filename, pos))
        i += 1
for i, (line, filename, pos) in enumerate(lines2):
    mnemonic, operands = parse(line)
    if mnemonic in ['jl', 'bne', 'bne-', 'bne+', 'beq', 'beq-', 'beq+']:
        check_operands_n(operands, 2, 3)
        operands[-1] = subst(operands[-1], i)
    lines3.append(('{} {}'.format(mnemonic, ', '.join(operands)), filename, pos))
for line, filename, pos in lines1:
    mnemonic, operands = parse(line)
    if mnemonic[-1] == ':':
        warn_unused_label(line[:-1])
    if mnemonic == '.global':
        check_global(operands[0])

# 3. assemble
if not args.o:
    m = re.match(r'(.*)\.', args.inputs[0])
    args.o = '{}.out'.format(m.group(1) if m else args.inputs[0])
with open(args.o, 'w') as f:
    for i, (line, filename, pos) in enumerate(lines3):
        mnemonic, operands = parse(line)
        byterepr = code(mnemonic, operands)
        if args.k:
            f.write("{} => x\"{}\",\n".format(i, ''.join('{:02x}'.format(ord(x)) for x in byterepr)))
        elif args.a:
            fmt = """
            wait for BR; RS_RX <= '0';
            wait for BR; RS_RX <= '{}';
            wait for BR; RS_RX <= '{}';
            wait for BR; RS_RX <= '{}';
            wait for BR; RS_RX <= '{}';
            wait for BR; RS_RX <= '{}';
            wait for BR; RS_RX <= '{}';
            wait for BR; RS_RX <= '{}';
            wait for BR; RS_RX <= '{}';
            wait for BR; RS_RX <= '1';

            wait for (2 * BR);

            """
            for b in byterepr:
                a = ord(b)
                ps = ['1' if a & (1 << j) else '0' for j in range(8)]
                f.write(fmt.format(*ps))
        else:
            f.write(byterepr)
    if args.k:
        f.write("others => (others => '0')\n")
if args.s:
    with open(args.o + '.s', 'w') as f:
        for i, (line, filename, pos) in enumerate(lines3):
            mnemonic, operands = parse(line)
            f.write('{:7} {:19} {}'.format(mnemonic, ', '.join(operands), show_label(i)).strip() + '\n')
