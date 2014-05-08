#!/usr/bin/env python
import matplotlib.pyplot as plt
import sys


def plot_graph(pairs, closest_pairs):
    plt.scatter(*zip(*pairs))
    plt.plot(*zip(*closest_pairs), color='red')
    plt.show()

def arg_to_pairs(arg):
    pairs = []
    for i in arg.strip('()').split('),('):
        pairs.append(tuple([int(j) for j in i.split(',')]))
    return pairs

if __name__ == '__main__':
    if len(sys.argv) == 3:
        pairs = arg_to_pairs(sys.argv[1])
        closest_pairs = arg_to_pairs(sys.argv[2])
        plot_graph(pairs, closest_pairs)
    else:
        print('Usage: %s [pairs] [closest pair]' % sys.argv[0])
        print("Ex: %s '(1,2),(1,3),(7,8),(22,44)' '(1,2),(1,3)'" % sys.argv[0])
