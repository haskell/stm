TOP=..
include $(TOP)/mk/boilerplate.mk

ALL_DIRS = \
	Control/Concurrent \
	Control/Concurrent/STM \
	Control/Monad

PACKAGE = stm
VERSION = 2.0
PACKAGE_DEPS = base

SRC_HC_OPTS += -fglasgow-exts -cpp

SRC_HADDOCK_OPTS += -t "Haskell Hierarchical Libraries (stm package)"

include $(TOP)/mk/target.mk
