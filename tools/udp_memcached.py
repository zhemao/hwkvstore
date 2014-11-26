import socket
import struct

BUFSIZE = 4096

MAGIC_OFFSET = 8
OPCODE_OFFSET = 9
STATUS_OFFSET = 14
VALUE_OFFSET = 8 + 28

class Client(object):
    def __init__(self, host = '127.0.0.1', port = 11211):
        self.socket = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        self.host = host
        self.port = port
        self.rid = 0

    def get(self, key):
        header = struct.pack("!HHHH", self.rid, 0, 1, 0)
        lenbytes = struct.pack("!H", len(key))
        packet = header + "\x80\x00" + lenbytes + ("\x00" * 6) + lenbytes + \
            ("\x00" * 12) + key
        #packet = header + "get " + key + "\r\n"

        self.socket.sendto(packet, (self.host, self.port))
        (resp, server) = self.socket.recvfrom(BUFSIZE)

        (respid,) = struct.unpack("!H", resp[0:2])
        if respid != self.rid:
            raise Exception("incorrect response ID")
        if ord(resp[MAGIC_OFFSET]) != 0x81:
            raise Exception("incorrect magic %x" % ord(resp[MAGIC_OFFSET]))
        if ord(resp[OPCODE_OFFSET]) != 0:
            raise Exception("incorrect opcode %x" % ord(resp[OPCODE_OFFSET]))

        (status,) = struct.unpack("!H", resp[STATUS_OFFSET:STATUS_OFFSET+2])
        if status != 0:
            return None
        self.rid += 1
        return resp[VALUE_OFFSET:]
