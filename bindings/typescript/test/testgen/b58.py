#!/usr/bin/env python3

'''
generate base58 tests

really checking to see if my code matches the base58 package
'''

import base58 as b
import random as r

###########################
### case generation
###########################

def random_bytes():
    len = r.randint(0, 999)
    #len = 10
    acc = []
    for _ in range(len):
        randbyte = r.randint(0, 255)
        acc.append(randbyte)
    return bytes(acc)

def case():
    decoded_bytes = random_bytes()
    encoded_bytes = b.b58encode(decoded_bytes)
    return {'db': decoded_bytes, 'eb': encoded_bytes}

def cases(n):
    return [case() for _ in range(n)]

###########################
### js formatting
###########################

def format_decoded_js(decoded_bytes):
    '''
    format the bytes as a js Uint8Array
    '''
    return f'new Uint8Array({list(decoded_bytes)})'

def format_encoded_js(encoded_bytes):
    s = str(encoded_bytes)[2:-1]
    return '"' + s + '"'

def format_case_js(case):
    decoded_bytes = case['db']
    encoded_bytes = case['eb']
    db_js = format_decoded_js(decoded_bytes)
    eb_js = format_encoded_js(encoded_bytes)
    return '{encoded: ' + eb_js + ',\n  ' + 'decoded: ' + db_js + '},\n '


def format_cases_js(cases):
    s = '['
    for case in cases:
        s += format_case_erl(case)
    # each case adds ",\n " so remove that
    # >>> "12345678"[:-3]
    # '12345'
    s = s[:-3]
    s += ']'
    return s


###########################
### erlang formatting
###########################

def format_decoded_erl(decoded_bytes):
    '''
    format the bytes as a erl Uint8Array
    '''
    bytes_list = list(decoded_bytes)
    byteslstr = f'{bytes_list}'
    return '<<' + byteslstr[1:-1] + '>>'

def format_encoded_erl(encoded_bytes):
    s = str(encoded_bytes)[2:-1]
    return '"' + s + '"'

def format_case_erl(case):
    decoded_bytes = case['db']
    encoded_bytes = case['eb']
    db_erl = format_decoded_erl(decoded_bytes)
    eb_erl = format_encoded_erl(encoded_bytes)
    return '{{encoded, ' + eb_erl + '},\n ' + '{decoded, ' + db_erl + '}}.\n'

def format_cases_erl(cases):
    s = ''
    for case in cases:
        s += format_case_erl(case)
    return s


###########################
### main
###########################

def main():
    c = cases(100)
    #print(format_cases_js(c))
    print(format_cases_erl(c))

if __name__ == '__main__':
    main()
