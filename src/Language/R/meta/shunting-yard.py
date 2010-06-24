#!/usr/bin/env python
# Copyright (c) 2010 the authors listed at the following URL, and/or
# the authors of referenced articles or incorporated external code:
# http://en.literateprograms.org/Shunting_yard_algorithm_(Python)?action=history&offset=20080610082258
# 
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
# 
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
# CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
# TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
# SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
# 
# Retrieved from: http://en.literateprograms.org/Shunting_yard_algorithm_(Python)?oldid=13636


import re, sys

try:
    expr = sys.argv[1]
except:
    print "Usage %s expression" % sys.argv[0]
    sys.exit(1)

tokens = re.split(' *([+\-*/()]|\d+\.\d+|\d+) *', expr)


prec = {'-u': 5, '*': 4, '/': 3, '+': 2, '-': 1, '(': 0, '': 9}

right = {'-u': 1}

def getprec(op):
    return prec.get(op, -1)


# Parsing

op_stack = []
rpn = []

last = ''
for token in tokens:
    if not token:
        continue

    if token == '-' and getprec(last) >= 0:
        token = '-u'


    if re.match('^\d+$|^\d+\.\d+$', token):
        if re.match('^\d+$|^\d+\.\d+$', last) or last == ')':
            raise Exception, "Value tokens must be separated by an operator"

        rpn.append(token)
    elif token == '(':
        op_stack.append(token)
    elif token == ')':
        while op_stack[-1] != '(':
            t = op_stack.pop()
            rpn.append(t)
        if op_stack.pop() != '(':
            raise Exception, "No matching ("
    elif getprec(token) > 0:
        pr = getprec(token)
        if token in right:
            while op_stack and pr < getprec(op_stack[-1]):
                rpn.append(op_stack.pop())
        else:
            while op_stack and pr <= getprec(op_stack[-1]):
               rpn.append(op_stack.pop())

        op_stack.append(token)
    else:
        raise Exception, "Unknown token: \"%s\"" % token

    last = token


while op_stack:
    rpn.append(op_stack.pop())





for token in rpn:
    if token == '-u':
        print '_1*',
    else:
        print token,
print 'p'




