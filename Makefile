TOP=..
include $(TOP)/mk/boilerplate.mk

ALL_DIRS = \
	Control/Concurrent \
	Control/Concurrent/STM \

PACKAGE = stm
VERSION = 1.0
PACKAGE_DEPS = base

SRC_HC_OPTS += -fglasgow-exts

SRC_HADDOCK_OPTS += -t "Haskell Hierarchical Libraries (stm package)"

include $(TOP)/mk/target.mk
