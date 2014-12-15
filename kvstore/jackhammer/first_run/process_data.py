#!/usr/bin/python

from os import listdir
from os.path import isfile, join

onlyfiles = sorted([ f for f in listdir(".") if isfile(join(".", f)) ])


outputdatastruct = {}


def clean_number(num):
    return num.replace("\n", "")


### round 1: get data out of the file
def process_file_r1(filename):
    pdat = {}


    f = open(filename)
    g = f.readlines()
    f.close()


    ready_for_area = False
    

    arealabels = ["Combinational area", "Buf/Inv area", "Noncombinational area", "Net Interconnect area", "Total cell area", "Total area"]

    ### loop over the file, handling cases as we go
    for x in range(len(g)):

        ### "readies" - these determine if we're allowed to collect some piece of data
        if "redirect -file $REPORTS_DIR/$ICC_CHIP_FINISH_CEL.area.rpt {report_area -nosplit -hierarchy}" in g[x]:
            ready_for_area = True
        if "Hierarchical area distribution" in g[x]:
            print("triggered")
            ready_for_area = False

        # get config data
        if "class DSEConfig" in g[x]:
            pdat["config"] = "".join(g[x:x+5])

        # area
        if ready_for_area:
            for label in arealabels:
                if label in g[x]:
                    #print("setting " + label)
                    #print(g[x])
                    pdat[label] = clean_number(g[x].strip().split(" ")[-1])

        # power


        # timing







    return pdat


### round 2: postprocess data
def process_file_r2(dat):
    return dat



stuff = process_file_r2(process_file_r1(onlyfiles[0]))
print(stuff)
