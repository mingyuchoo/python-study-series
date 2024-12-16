import threading

def print_numbers():
    for i in range(1, 11):
        print(i)


def print_letters():
    for i in range(ord('a'), ord('k')):
        print(chr(i))


t1 = threading.Thread(target=print_numbers)
t2 = threading.Thread(target=print_letters)

t1.start()
t2.start()


t1.join()
t2.join()
