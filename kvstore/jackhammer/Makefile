start: all

base_dir = ..
jh_dir   = .
sim_dir  = .
config_dir := src/main/scala/config
JACK_SBT := java -Xmx2048M -Xss8M -XX:MaxPermSize=128M -jar ~cs250/install/sbt/sbt-launch.jar -DchiselVersion="2.3-SNAPSHOT"
PROJ_SBT := java -Xmx2048M -Xss8M -XX:MaxPermSize=128M -jar ~cs250/install/sbt/sbt-launch.jar -DchiselVersion="2.3-SNAPSHOT"

include $(base_dir)/Makefrag
# PROJ,CONFIG
RAND ?= 0

CHISEL_ARGS := --noIoDebug --targetDir $(config_dir)

$(base_dir)/$(config_dir)/$(PROJ).$(addsuffix .cst, %): $(base_dir)/$(src_path)/*.scala
ifeq ($(PROJ),referencechip)
	cd $(base_dir) && $(PROJ_SBT) "project $(PROJ)" "elaborate $(MODEL) $(CHISEL_ARGS) --configCollect $(PROJ).$*"
else
	cd $(base_dir) && $(PROJ_SBT) "runMain $(PROJ).$(MODEL) $(MODEL_ARGS) $(CHISEL_ARGS) --configCollect $(PROJ).$*"
endif

$(base_dir)/$(config_dir)/$(PROJ).$(addsuffix .scala, %): $(base_dir)/$(config_dir)/$(PROJ).$(addsuffix .cst, %)
ifeq ($(RAND),0)
	$(JACK_SBT) "runMain jackhammer.Jack -d $(base_dir)/$(config_dir) -c $* -p $(PROJ)"
else 
	$(JACK_SBT) "runMain jackhammer.Jack -d $(base_dir)/$(config_dir) -c $* -p $(PROJ) -r $(RAND)"
endif

#Default Config
cst: $(base_dir)/$(config_dir)/$(PROJ).$(DCONFIG).cst

all: $(base_dir)/$(config_dir)/$(PROJ).$(DCONFIG).scala

clean: clean-cst clean-scala

clean-cst:
	rm $(base_dir)/$(config_dir)/*.cst
	rm $(base_dir)/$(config_dir)/*.knb

clean-scala:
	rm $(base_dir)/$(config_dir)/$(PROJ).*.scala

child:
	$(JACK_SBT) "runMain jackhammer.Hammer -mode c -d $(CONFIG) jackhammer.$(JSETTINGS)"

parent:
	$(JACK_SBT) "runMain jackhammer.Hammer -mode p jackhammer.$(JSETTINGS)"

.PHONY: all cst start

.SECONDARY: $(base_dir)/$(config_dir)/$(PROJ).$(addsuffix .cst, %)
