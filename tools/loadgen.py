import random
import string

KEY_SIZES = [20, 25, 27, 30, 32, 33, 35, 40, 42, 50, 100]
KEY_DIVS = len(KEY_SIZES) - 1
VALUE_SIZES = [1, 5, 10, 100, 500, 2000]
VALUE_DIVS = len(VALUE_SIZES) - 1

NUM_KEYS = 100
NUM_READS = 1000

def pick_random_size():
    keyx = random.random() * KEY_DIVS + 1.0
    valx = random.random() * VALUE_DIVS + 1.0

    keyind = int(keyx)
    valind = int(valx)

    keystart, keyend = tuple(KEY_SIZES[keyind-1:keyind+1])
    valstart, valend = tuple(VALUE_SIZES[valind-1:valind+1])

    keyoff = keyx - keyind
    valoff = valx - valind

    keylen = keystart + int((keyend - keystart) * keyoff)
    vallen = valstart + int((valend - valstart) * valoff)

    return keylen, vallen

def random_str(size):
    letter_choices = string.ascii_letters + string.digits + string.punctuation
    return ''.join([random.choice(letter_choices) for i in range(0, size)])

def main():
    sizes = [pick_random_size() for i in range(0, NUM_KEYS)]
    pairs = [(random_str(keysize), random_str(valsize))
            for (keysize, valsize) in sizes]
    for key, val in pairs:
        print(key, val)
    print('---')
    for i in range(0, NUM_READS):
        print(random.choice(pairs)[0])

if __name__ == '__main__':
    main()
