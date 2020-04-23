# -----------------------------------------------------
# How many dimensions to simulate.
# -----------------------------------------------------

NUM_DIM := 3


# -----------------------------------------------------
# Which dimensions use periodic boundary conditions?
# -----------------------------------------------------

PBC_X := 1
PBC_Y := 1
PBC_Z := 1


# -----------------------------------------------------
# Use flow or not?
# -----------------------------------------------------

USE_FLOW := 0


# -----------------------------------------------------
# Profile or not?
# -----------------------------------------------------

USE_PROFILING := 0


# -----------------------------------------------------
# Directories
# -----------------------------------------------------

# bin will be created when building the program.
# Executable and the intermediate object files will be placed there.
# 'make clean' will completely remove bin and its contents.
BIN_PATH := bin

# All the source files recide here.
SRC_PATH := ../src

# Included (external) headers
INCL = -I../incl/ -I/opt/rocm/hiprand/include  -I/opt/rocm/rocrand/include -I/opt/rocm/hipcub/include/ -I/opt/rocm/rocprim/include/


# -----------------------------------------------------
# Object files, headers and the main executable
# -----------------------------------------------------

# List all objects that contain code.
OBJ_NAMES := Main.o Simulator.o Kernels.o
OBJS := $(addprefix $(BIN_PATH)/, $(OBJ_NAMES))

# Find all headers in source dir.
HEADERS := $(wildcard $(SRC_PATH)/*.h)

# Name of the final executable.
EXEC := $(BIN_PATH)/cubble

# -----------------------------------------------------
# Compilers to use
# -----------------------------------------------------

C_CPU := hipcc -g

# -----------------------------------------------------
# External libraries to link
# -----------------------------------------------------

LIBS := -lhiprand

# -----------------------------------------------------
# Preprocessor defines
# -----------------------------------------------------

DEFINES := -DNUM_DIM=$(NUM_DIM) -DUSE_FLOW=$(USE_FLOW) \
	   -DPBC_X=$(PBC_X) -DPBC_Y=$(PBC_Y) -DPBC_Z=$(PBC_Z) \
	   -DNDEBUG -DENABLE_HIP_PROFILE=0 -DUSE_PROFILING=$(USE_PROFILING)

# -----------------------------------------------------
# Flags
# -----------------------------------------------------

CPU_FLAGS := -Wall -std=c++14 -m64 --amdgpu-target=gfx906
COMMON_FLAGS := $(INCL)
OPTIM_FLAGS := -O3
PROFILING_FLAGS :=
ifeq ($(USE_PROFILING), 1)
PROFILING_FLAGS := -pg
endif

# -----------------------------------------------------
# First rule: Builds the project with default settings
# -----------------------------------------------------

.PHONY : all
all : | $(BIN_PATH)
	$(MAKE) -j8 $(EXEC)

# -----------------------------------------------------
# Rule for main executable.
# -----------------------------------------------------

# By default has some safety nets at place but also uses some optimizations.
$(EXEC) : $(HEADERS) $(OBJS) $(GPU_CODE)
	$(C_CPU) $(PROFILING_FLAGS) $(OBJS) $(GPU_CODE) $(LIBS) --amdgpu-target=gfx906 -o $@

# -----------------------------------------------------
# Rule for the intermediate objects
# -----------------------------------------------------

# CPU code
$(BIN_PATH)/%.o : $(SRC_PATH)/%.cpp
	$(eval OPTIONS = $(CPU_FLAGS) $(COMMON_FLAGS) \
	$(OPTIM_FLAGS) $(DEFINES) $(PROFILING_FLAGS))
	$(C_CPU) $< $(OPTIONS) -c -o $@

# -----------------------------------------------------
# Clean up
# -----------------------------------------------------

.PHONY : clean
clean :
	rm -rf $(BIN_PATH)


# -----------------------------------------------------
# mkdir bin
# -----------------------------------------------------

$(BIN_PATH) :
	mkdir $(BIN_PATH)
