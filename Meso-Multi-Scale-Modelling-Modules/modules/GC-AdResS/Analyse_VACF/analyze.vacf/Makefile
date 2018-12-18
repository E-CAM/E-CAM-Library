#tmp_sources =	mk_mem.cpp main.cpp norm_var.cpp rhs.cpp pub_func.cpp solution.cpp
#tmp_sources :=	$(wildcard *.cpp)
XDR_INCLUDE = ./xdr/include
XDR_LINK = -L./xdr/lib -lxdrfile
boost-lib-option=	-lboost_program_options
link-options=		$(boost-lib-option) $(XDR_LINK) 

include_dirs = 	./include $(XDR_INCLUDE) $(MKL_INCLUDE) $(boost-include-path) 
#include_dirs = ./include $(MKL_INCLUDE) $(boost-include-path)


sources_dir =		src
application_dir=	app
application_target=	$(notdir $(subst .cpp,,$(wildcard $(application_dir)/*.cpp)))
cppsources := 		$(wildcard $(sources_dir)/*.cpp)
tmp_cppsources =  	$(notdir $(cppsources))
tmp_cppdepends = 	$(subst .cpp,.d,$(tmp_cppsources))
tmp_cppobjects = 	$(subst .cpp,.o,$(tmp_cppsources))

csources := 		$(wildcard $(sources_dir)/*.c)
tmp_csources =  	$(notdir $(csources))
tmp_cdepends = 		$(subst .c,.d,$(tmp_csources))
tmp_cobjects = 		$(subst .c,.o,$(tmp_csources))

cppdepends_dir = 	.depends/cpp
cppobjects_dir = 	lib/cpp
cdepends_dir = 		.depends/c
cobjects_dir = 		lib/c
vpath %.h 		$(include_dirs)

cppdepends = 	$(addprefix $(cppdepends_dir)/,$(tmp_cppdepends))
cppobjects = 	$(addprefix $(cppobjects_dir)/,$(tmp_cppobjects))

cdepends = 	$(addprefix $(cdepends_dir)/,$(tmp_cdepends))
cobjects = 	$(addprefix $(cobjects_dir)/,$(tmp_cobjects))


C:=		gcc
CFLAGS +=	-g -O0 -Wall 
CFLAGS += 	$(addprefix -I ,$(include_dirs))

CXX:=		g++
CXXFLAGS += 	$(CFLAGS)

target:		makedir $(application_target)

%:	$(cobjects) $(cppobjects) $(application_dir)/%.o
	$(call make-target,$@,$^)

.PHONY: clean
clean:
	rm -f $(application_target) $(cobjects) $(cdepends) $(cppobjects) $(cppdepends)  $(application_dir)/*.o $(application_dir)/*.d

.PHONY: makedir
makedir:
	test -d $(cppdepends_dir) || mkdir -p $(cppdepends_dir)	
	test -d $(cppobjects_dir) || mkdir -p $(cppobjects_dir)
	test -d $(cdepends_dir) || mkdir -p $(cdepends_dir)
	test -d $(cobjects_dir) || mkdir -p $(cobjects_dir)
	test -d $(sources_dir) || mkdir -p $(sources_dir)
	test -d $(application_dir) || mkdir -p $(application_dir)
	test -d include || mkdir -p include

ifneq "$(MAKECMDGOALS)" "clean"
-include $(cppdepends)
-include $(cdepends)
-include $(addprefix $(application_dir)/,$(notdir $(subst .cpp,.d,$(wildcard $(application_dir)/*.cpp))))
endif

# $(call make-depend,source-file,object-file,depend-file)
define make-depend
  $(CXX)	-MM		\
		-MF $3		\
		-MP		\
		-MT $2		\
		$(CXXFLAGS)	\
		$(TARGET_ARCH)	\
		$1
endef
# $(call make-target,target,object-files-or-lib-files)
define make-target
  $(CXX) $(CXXFLAGS) -o $1 $2 -lm $(link-options) 
endef


$(cppobjects_dir)/%.o:	$(sources_dir)/%.cpp
	$(call make-depend,$<,$@,$(cppdepends_dir)/$(notdir $(subst .o,.d,$@)))
	$(COMPILE.cpp) $(OUTPUT_OPTION) $<	
$(cobjects_dir)/%.o:	$(sources_dir)/%.c
	$(call make-depend,$<,$@,$(cdepends_dir)/$(notdir $(subst .o,.d,$@)))
	$(COMPILE.c) $(OUTPUT_OPTION) $<	
$(application_dir)/%.o:	$(application_dir)/%.cpp
	$(call make-depend,$<,$@,$(application_dir)/$(notdir $(subst .o,.d,$@)))
	$(COMPILE.cpp) $(OUTPUT_OPTION) $<	





















