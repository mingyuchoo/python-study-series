#!/usr/bin/env python


def left(s):
    return 2*s + 10


def right(s):
    return s + 15


def what_value_of_s_balances_the_scale():
    for s in range(100):
        if left(s) == right(s):
            print("I found the number s: {}".format(s))
        else:
            pass


if __name__ == "__main__":
    what_value_of_s_balances_the_scale()
