#! /usr/bin/env python

import sys

if len(sys.argv) < 2:
  print "usage: %s <prelude.log>" % sys.argv[0]
  sys.exit(-1)

lines = map(lambda x: x.strip(), open(sys.argv[1]).readlines())

flag = False
srams = {}
llc_tag = []
llc_data = []
dcache_tag = []
dcache_data = []
icache_tag = []
icache_data = []
vu_icache_tag = []
vu_icache_data = []
vu_rf = []
vu_ldq = []
for line in lines:
  if line.find('Total cell area') != -1:
    area = float(line.split(':')[1].strip())
  if flag:
    if count == 0: sram_name = line
    if count == 1: sram_type = line
    if count == 2: sram_height = line
    if count == 3: sram_width = line
    count += 1
    if count == 4:
      flag = False
      srams[sram_name] = (sram_type, float(sram_height), float(sram_width))
      tag = False
      if sram_name.find('tag') != -1: tag = True
      if sram_name.find('llc') != -1:
        if tag: llc_tag.append(sram_name)
        else: llc_data.append(sram_name)
      elif sram_name.find('Frontend') != -1:
        if tag: vu_icache_tag.append(sram_name)
        else: vu_icache_data.append(sram_name)
      elif sram_name.find('icache') != -1:
        if tag: icache_tag.append(sram_name)
        else: icache_data.append(sram_name)
      elif sram_name.find('dcache') != -1:
        if tag: dcache_tag.append(sram_name)
        else: dcache_data.append(sram_name)
      elif sram_name.find('b8lane') != -1:
        vu_rf.append(sram_name)
      elif sram_name.find('vldq') != -1:
        vu_ldq.append(sram_name)
  if line.find('SRAM INFO') == 0:
    flag = True
    count = 0

def sort(l, func):
  for i in range(len(l)):
    for j in range(i, len(l)):
      if func(l[i]) > func(l[j]):
        t = l[i]
        l[i] = l[j]
        l[j] = t

def extractT(x):
  anchor = x.split('/T')
  if len(anchor) > 1:
    return int(anchor[1].split('/')[0])
  else:
    return x

def extractBank(x):
  anchor = x.split('Bank_')
  if len(anchor) > 1:
    anchor = anchor[1].split('/')
    if len(anchor) > 1:
      return int(anchor[0])
    else:
      return 0
  else:
    return x

sort(llc_tag, extractT)
sort(llc_data, extractT)
sort(dcache_tag, extractT)
sort(dcache_data, extractT)
sort(icache_tag, extractT)
sort(icache_data, extractT)
sort(vu_rf, extractBank)

print "area: %f" % area
#print llc_tag
#print llc_data
#print dcache_tag
#print dcache_data
#print icache_tag
#print icache_data
#print vu_icache_tag
#print vu_icache_data
#print vu_rf
#print vu_ldq

llc_width = 0
for sram in llc_tag+llc_data:
  llc_width += (srams[sram][2] + 10.0)
llc_width -= 10.0
llc_width = (int(llc_width)/50+1)*50
llc_height = area/0.7/float(llc_width)
llc_height = (int(llc_height)/50+1)*50
ratio = float(llc_width) / float(llc_height)

if ratio > 2.0:
  # tile llc data rams in two rows
  pass
else:
  # tile llc data rams in one row with tags
  llc_list = llc_data[0:len(llc_data)/2] + llc_tag + llc_data[len(llc_data)/2:len(llc_data)]
  f = lambda x, y: x + ' ' + y
  print 'set_fp_macro_array -name vrf -elements [list [get_cells "%s"] [get_cells "%s"]] -x_offset 10 -y_offset 10 -align_2d left-bottom -rectilinear' % (reduce(f, vu_rf[0:len(vu_rf)/2]), reduce(f, vu_rf[len(vu_rf)/2:len(vu_rf)]))
  print 'set_fp_macro_array -name dcache -elements [list [get_cells "%s"]] -x_offset 10 -y_offset 10 -align_edge top -rectilinear' % (reduce(f, dcache_data + dcache_tag))
  print 'set_fp_macro_array -name icache -elements [list [get_cells "%s"]] -x_offset 10 -y_offset 10 -align_edge bottom -rectilinear' % (reduce(f, icache_data + icache_tag))
  print 'set_fp_macro_array -name llc -elements [list [get_cells "%s"]] -x_offset 10 -y_offset 10 -align_edge top -rectilinear' % (reduce(f, llc_list))
  print 'set_fp_macro_array -name vu_icache -elements [list [get_cells "%s"]] -x_offset 10 -y_offset 10 -align_edge top -rectilinear' % (reduce(f, vu_icache_tag + vu_icache_data))

print "%d/%d=%f" % (llc_width,llc_height,float(llc_width)/float(llc_height))
